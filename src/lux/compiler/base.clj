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
                 [type :as &type])
            (lux.analyser [base :as &a]
                          [module :as &a-module]))
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Utils]
(defn ^:private write-file [^String file ^bytes data]
  (with-open [stream (java.io.BufferedOutputStream. (java.io.FileOutputStream. file))]
    (.write stream data)))

(defn ^:private write-output [module name data]
  (let [module* module]
    (.mkdirs (java.io.File. (str "output/jvm/" module*)))
    (write-file (str "output/jvm/" module* "/" name ".class") data)))

(defn ^:private write-cache [module name data]
  (let [module* (string/replace module #"/" " ")]
    (.mkdirs (java.io.File. (str "cache/jvm/" module*)))
    (write-file (str "cache/jvm/" module* "/" name ".class") data)))

(defn ^:private clean-file [^java.io.File file]
  (if (.isDirectory file)
    (do (doseq [f (seq (.listFiles file))]
          (clean-file f))
      (.delete file))
    (.delete file)))

(defn ^:private read-file [file]
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
  (.loadClass loader name))

(defn save-class! [name bytecode]
  (|do [eval? &/get-eval
        module &/get-module-name
        loader &/loader
        !classes &/classes
        :let [real-name (str (string/replace module #"/" ".") "." name)
              _ (swap! !classes assoc real-name bytecode)
              _ (load-class! loader real-name)
              _ (when (not eval?)
                  (do (write-output module name bytecode)
                    (write-cache module name bytecode)))]]
    (return nil)))

(defn cached? [module]
  (.exists (java.io.File. (str "cache/jvm/" (string/replace module #"/" " ") "/_.class"))))

(defn delete-cache [module]
  (fn [state]
    (do (clean-file (java.io.File. (str "cache/jvm/" (string/replace module #"/" " "))))
      (return* state nil))))

(defn ^:private replace-several [content & replacements]
  (let [replacement-list (partition 2 replacements)]
    (reduce #(try (let [[_pattern _rep] %2]
                    (string/replace %1 _pattern (string/re-quote-replacement _rep)))
               (catch Exception e
                 (prn 'replace-several content %1 %2)
                 (throw e)))
            content replacement-list)))

(defn ^:private replace-cache [cache-name]
  (if (.startsWith cache-name "$")
    (replace-several cache-name
                     #"_ASTER_" "*"
                     #"_PLUS_" "+"
                     #"_DASH_" "-"
                     #"_SLASH_" "/"
                     #"_BSLASH_" "\\"
                     #"_UNDERS_" "_"
                     #"_PERCENT_" "%"
                     #"_DOLLAR_" "$"
                     #"_QUOTE_" "'"
                     #"_BQUOTE_" "`"
                     #"_AT_" "@"
                     #"_CARET_" "^"
                     #"_AMPERS_" "&"
                     #"_EQ_" "="
                     #"_BANG_" "!"
                     #"_QM_" "?"
                     #"_COLON_" ":"
                     #"_PERIOD_" "."
                     #"_COMMA_" ","
                     #"_LT_" "<"
                     #"_GT_" ">"
                     #"_TILDE_" "~"
                     #"_PIPE_" "|")
    cache-name))

(defn load-cache [module module-hash compile-module]
  (|do [loader &/loader
        !classes &/classes]
    (let [module-path (str "cache/jvm/" (string/replace module #"/" " "))
          module* (string/replace module #"/" ".")
          class-name (str module* "._")
          module-meta (do (swap! !classes assoc class-name (read-file (java.io.File. (str module-path "/_.class"))))
                        (load-class! loader class-name))]
      (if (and (= module-hash (-> module-meta (.getField "_hash") (.get nil)))
               (= version (-> module-meta (.getField "_compiler") (.get nil))))
        (let [imports (string/split (-> module-meta (.getField "_imports") (.get nil)) #"\t")
              ;; _ (prn module 'imports imports)
              ]
          (|do [loads (&/map% (fn [_import]
                                (load-cache _import (-> (str "input/" _import ".lux") slurp hash) compile-module))
                              (if (= [""] imports)
                                (&/|list)
                                (&/->list imports)))]
            (if (->> loads &/->seq (every? true?))
              (do (doseq [file (seq (.listFiles (java.io.File. module-path)))
                          :when (not= "_.class" (.getName file))]
                    (let [real-name (second (re-find #"^(.*)\.class$" (.getName file)))
                          bytecode (read-file file)
                          ;; _ (prn 'load-cache module real-name)
                          ]
                      ;; (swap! !classes assoc (str module* "." (replace-cache real-name)) bytecode)
                      (swap! !classes assoc (str module* "." real-name) bytecode)
                      ;; (swap! !classes assoc "__temp__" bytecode)
                      ;; (swap! !classes assoc (-> (load-class! loader "__temp__") (.getField "_name") (.get nil)) bytecode)
                      (write-output module real-name bytecode)))
                ;; (swap! !classes dissoc "__temp__")
                (let [defs (string/split (-> module-meta (.getField "_defs") (.get nil)) #"\t")]
                  (|do [_ (fn [state]
                            (&/run-state (&/map% (fn [_def]
                                                   (let [[_exported? _name _ann] (string/split _def #" ")
                                                         ;; _ (prn '[_exported? _name _ann] [_exported? _name _ann])
                                                         def-class (load-class! loader (str module* ".$" (&/normalize-ident _name)))
                                                         def-name (-> def-class (.getField "_name") (.get nil))]
                                                     (|do [_ (case _ann
                                                               "T" (&a-module/define module def-name (&/V "lux;TypeD" nil) &type/Type)
                                                               "M" (|do [_ (&a-module/define module def-name (&/V "lux;ValueD" &type/Macro) &type/Macro)]
                                                                     (&a-module/declare-macro module def-name))
                                                               "V" (let [def-type (-> def-class (.getField "_meta") (.get nil))]
                                                                     (matchv ::M/objects [def-type]
                                                                       [["lux;ValueD" _def-type]]
                                                                       (&a-module/define module def-name def-type _def-type)))
                                                               ;; else
                                                               (let [[_ __module __name] (re-find #"^A(.*);(.*)$" _ann)]
                                                                 (|do [__type (&a-module/def-type __module __name)]
                                                                   (do ;; (prn '__type [__module __name] (&type/show-type __type))
                                                                     (&a-module/def-alias module def-name __module __name __type)))))]
                                                       (if (= "1" _exported?)
                                                         (&a-module/export module def-name)
                                                         (return nil)))
                                                     ))
                                                 (if (= [""] defs)
                                                   (&/|list)
                                                   (&/->list defs)))
                                         (->> state
                                              (&/set$ &/$ENVS (&/|list (&/env module)))
                                              (&/update$ &/$MODULES #(&/|put module &a-module/init-module %)))))]
                    (return true))))
              (|do [_ (delete-cache module)
                    _ (compile-module module)]
                (return false)))))
        
        (|do [_ (delete-cache module)
              _ (compile-module module)]
          (return false)))
      )))
