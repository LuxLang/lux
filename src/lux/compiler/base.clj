;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns lux.compiler.base
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail*]]
                 [type :as &type]
                 [host :as &host])
            (lux.analyser [base :as &a]
                          [module :as &a-module]))
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)
           (java.io File
                    BufferedOutputStream
                    FileOutputStream)
           (java.lang.reflect Field)))

;; [Utils]
(defn ^:private write-file [^String file ^bytes data]
  (with-open [stream (BufferedOutputStream. (FileOutputStream. file))]
    (.write stream data)))

(defn ^:private write-output [module name data]
  (let [module* (&host/->module-class module)]
    (.mkdirs (File. (str "output/jvm/" module*)))
    (write-file (str "output/jvm/" module* "/" name ".class") data)))

(defn ^:private write-cache [module name data]
  (let [module* (&host/->module-class module)]
    (.mkdirs (File. (str "cache/jvm/" module*)))
    (write-file (str "cache/jvm/" module* "/" name ".class") data)))

(defn ^:private clean-file [^File file]
  (if (.isDirectory file)
    (do (doseq [f (seq (.listFiles file))]
          (clean-file f))
      (.delete file))
    (.delete file)))

(defn ^:private read-file [^File file]
  (with-open [reader (io/input-stream file)]
    (let [length (.length file)
          buffer (byte-array length)]
      (.read reader buffer 0 length)
      buffer)))

;; [Exports]
(def version "0.2")

(def local-prefix "l")
(def partial-prefix "p")
(def closure-prefix "c")
(def apply-signature "(Ljava/lang/Object;)Ljava/lang/Object;")

(defn load-class! [^ClassLoader loader name]
  ;; (prn 'load-class! name)
  (.loadClass loader name))

(defn save-class! [name bytecode]
  (|do [eval? &/get-eval
        module &/get-module-name
        loader &/loader
        !classes &/classes
        :let [real-name (str (&host/->module-class module) "." name)
              _ (swap! !classes assoc real-name bytecode)
              _ (load-class! loader real-name)
              _ (when (not eval?)
                  (do (write-output module name bytecode)
                    (write-cache module name bytecode)))]]
    (return nil)))

(defn cached? [module]
  (.exists (File. (str "cache/jvm/" (&host/->module-class module) "/_.class"))))

(defn delete-cache [module]
  (fn [state]
    (do (clean-file (File. (str "cache/jvm/" (&host/->module-class module))))
      (return* state nil))))

(defn ^:private replace-several [content & replacements]
  (let [replacement-list (partition 2 replacements)]
    (reduce #(try (let [[_pattern _rep] %2]
                    (string/replace %1 _pattern (string/re-quote-replacement _rep)))
               (catch Exception e
                 (prn 'replace-several content %1 %2)
                 (throw e)))
            content replacement-list)))

(defn ^:private get-field [^String field-name ^Class class]
  (-> class ^Field (.getField field-name) (.get nil))
  ;; (try (-> class ^Field (.getField field-name) (.get nil))
  ;;   (catch Error e
  ;;     (assert false (prn-str 'get-field field-name class))))
  )

(defn load-cache [module module-hash compile-module]
  (|do [loader &/loader
        !classes &/classes
        already-loaded? (&a-module/exists? module)
        _modules &/modules
        :let [redo-cache (|do [_ (delete-cache module)
                               _ (compile-module module)]
                           (return false))]]
    (do (prn 'load-cache module 'sources already-loaded?
             (&/->seq _modules))
      (if already-loaded?
        (return true)
        (if (cached? module)
          (do (prn 'load-cache/HASH module module-hash)
            (let [module* (&host/->module-class module)
                  module-path (str "cache/jvm/" module*)
                  class-name (str module* "._")
                  ^Class module-meta (do (swap! !classes assoc class-name (read-file (File. (str module-path "/_.class"))))
                                       (load-class! loader class-name))]
              (if (and (= module-hash (get-field "_hash" module-meta))
                       (= version (get-field "_compiler" module-meta)))
                (let [imports (string/split (-> module-meta (.getField "_imports") (.get nil)) #"\t")
                      _ (prn 'load-cache/IMPORTS module imports)
                      ]
                  (|do [loads (&/map% (fn [_import]
                                        (load-cache _import (-> (str "input/" _import ".lux") slurp hash) compile-module))
                                      (if (= [""] imports)
                                        (&/|list)
                                        (&/->list imports)))]
                    (if (->> loads &/->seq (every? true?))
                      (do (doseq [^File file (seq (.listFiles (File. module-path)))
                                  :let [file-name (.getName file)]
                                  :when (not= "_.class" file-name)]
                            (let [real-name (second (re-find #"^(.*)\.class$" file-name))
                                  bytecode (read-file file)
                                  ;; _ (prn 'load-cache module real-name)
                                  ]
                              (swap! !classes assoc (str module* "." real-name) bytecode)
                              (write-output module real-name bytecode)))
                        (let [defs (string/split (get-field "_defs" module-meta) #"\t")]
                          ;; (prn 'load-cache module defs)
                          (|do [_ (&a-module/enter-module module)
                                _ (&/map% (fn [_def]
                                            (let [[_exported? _name _ann] (string/split _def #" ")
                                                  ;; _ (prn '[_exported? _name _ann] [_exported? _name _ann])
                                                  ]
                                              (|do [_ (case _ann
                                                        "T" (&a-module/define module _name (&/V "lux;TypeD" nil) &type/Type)
                                                        "M" (|do [_ (&a-module/define module _name (&/V "lux;ValueD" &type/Macro) &type/Macro)]
                                                              (&a-module/declare-macro module _name))
                                                        "V" (let [def-class (load-class! loader (str module* "." (&/normalize-name _name)))
                                                                  ;; _ (println "Fetching _meta" module _name (str module* "." (&/normalize-name _name)) def-class)
                                                                  def-type (get-field "_meta" def-class)]
                                                              (matchv ::M/objects [def-type]
                                                                [["lux;ValueD" _def-type]]
                                                                (&a-module/define module _name def-type _def-type)))
                                                        ;; else
                                                        (let [[_ __module __name] (re-find #"^A(.*);(.*)$" _ann)]
                                                          (|do [__type (&a-module/def-type __module __name)]
                                                            (do ;; (prn '__type [__module __name] (&type/show-type __type))
                                                                (&a-module/def-alias module _name __module __name __type)))))]
                                                (if (= "1" _exported?)
                                                  (&a-module/export module _name)
                                                  (return nil)))
                                              ))
                                          (if (= [""] defs)
                                            (&/|list)
                                            (&/->list defs)))]
                            (return true))))
                      redo-cache)))
                redo-cache)
              ))
          redo-cache)))))
