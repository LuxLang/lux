;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns lux.compiler.cache
  (:refer-clojure :exclude [load])
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |case |let]]
                 [type :as &type]
                 [host :as &host])
            (lux.analyser [base :as &a]
                          [module :as &a-module])
            (lux.compiler [base :as &&]
                          [io :as &&io]))
  (:import (java.io File
                    BufferedOutputStream
                    FileOutputStream)
           (java.lang.reflect Field)))

;; [Utils]
(defn ^:private read-file [^File file]
  "(-> File (Array Byte))"
  (with-open [reader (io/input-stream file)]
    (let [length (.length file)
          buffer (byte-array length)]
      (.read reader buffer 0 length)
      buffer)))

(defn ^:private clean-file [^File file]
  "(-> File (,))"
  (if (.isDirectory file)
    (do (doseq [f (seq (.listFiles file))]
          (clean-file f))
      (.delete file))
    (.delete file)))

(defn ^:private get-field [^String field-name ^Class class]
  "(-> Text Class Object)"
  (-> class ^Field (.getField field-name) (.get nil)))

;; [Resources]
(defn cached? [module]
  "(-> Text Bool)"
  (.exists (new File (str &&/output-dir "/" (&host/->module-class module) "/_.class"))))

(defn delete [module]
  "(-> Text (Lux (,)))"
  (fn [state]
    (do (clean-file (new File (str &&/output-dir "/" (&host/->module-class module))))
      (return* state nil))))

(defn clean [state]
  "(-> Compiler (,))"
  (let [needed-modules (->> state (&/$get-modules) &/|keys &/->seq set)
        outdated? #(-> ^File % .getName (string/replace &host/module-separator "/") (->> (contains? needed-modules)) not)
        outdate-files (->> &&/output-dir (new File) .listFiles seq (filter outdated?))
        program-file (new File &&/output-package)]
    (when (.exists program-file)
      (.delete program-file))
    (doseq [f outdate-files]
      (clean-file f))
    nil))

(defn load [module module-hash compile-module]
  "(-> Text Int (-> Text (Lux (,))) (Lux Bool))"
  (|do [loader &/loader
        !classes &/classes
        already-loaded? (&a-module/exists? module)
        _modules &/modules
        :let [redo-cache (|do [_ (delete module)
                               _ (compile-module module)]
                           (return false))]]
    (do ;; (prn 'load module 'sources already-loaded?
        ;;      (&/->seq _modules))
        (if already-loaded?
          (return true)
          (if (cached? module)
            (do ;; (prn 'load/HASH module module-hash)
                (let [module* (&host/->module-class module)
                      module-path (str &&/output-dir "/" module*)
                      class-name (str module* "._")
                      ^Class module-meta (do (swap! !classes assoc class-name (read-file (File. (str module-path "/_.class"))))
                                           (&&/load-class! loader class-name))]
                  (if (and (= module-hash (get-field &/hash-field module-meta))
                           (= &&/version (get-field &/compiler-field module-meta)))
                    (let [imports (string/split (get-field &/imports-field module-meta) (re-pattern (java.util.regex.Pattern/quote &&/import-separator)))
                          ;; _ (prn 'load/IMPORTS module imports)
                          ]
                      (|do [loads (&/map% (fn [_import]
                                            (|do [content (&&io/read-file (str &&/input-dir "/" _import ".lux"))]
                                              (load _import (hash content) compile-module)))
                                          (if (= [""] imports)
                                            (&/|list)
                                            (&/->list imports)))]
                        (if (->> loads &/->seq (every? true?))
                          (do (doseq [^File file (seq (.listFiles (File. module-path)))
                                      :let [file-name (.getName file)]
                                      :when (not= "_.class" file-name)]
                                (let [real-name (second (re-find #"^(.*)\.class$" file-name))
                                      bytecode (read-file file)
                                      ;; _ (prn 'load module real-name)
                                      ]
                                  (swap! !classes assoc (str module* "." real-name) bytecode)))
                            (let [defs (string/split (get-field &/defs-field module-meta) (re-pattern (java.util.regex.Pattern/quote &&/def-separator)))
                                  ;; _ (prn module '(get-field &/tags-field module-meta)
                                  ;;        (string/split (get-field &/tags-field module-meta) (re-pattern (java.util.regex.Pattern/quote &&/tag-group-separator))))
                                  tag-groups (let [all-tags (get-field &/tags-field module-meta)]
                                               (if (= "" all-tags)
                                                 (&/|list)
                                                 (-> all-tags
                                                     (string/split (re-pattern (java.util.regex.Pattern/quote &&/tag-group-separator)))
                                                     (->> (map (fn [_group]
                                                                 ;; (prn '_group _group)
                                                                 (let [[_type _tags] (string/split _group (re-pattern (java.util.regex.Pattern/quote &&/type-separator)))]
                                                                   ;; (prn '[_type _tags] [_type _tags])
                                                                   (&/P _type (&/->list (string/split _tags (re-pattern (java.util.regex.Pattern/quote &&/tag-separator)))))))))
                                                     &/->list)))]
                              ;; (prn 'load module defs)
                              (|do [_ (&a-module/enter-module module)
                                    _ (&a-module/set-imports imports)
                                    _ (&/map% (fn [_def]
                                                (let [[_exported? _name _ann] (string/split _def #" ")
                                                      ;; _ (prn '[_exported? _name _ann] [_exported? _name _ann])
                                                      ]
                                                  (|do [_ (case _ann
                                                            "T" (let [def-class (&&/load-class! loader (str module* "." (&/normalize-name _name)))
                                                                      def-value (get-field &/datum-field def-class)]
                                                                  (&a-module/define module _name (&/S &/$TypeD def-value) &type/Type))
                                                            "M" (let [def-class (&&/load-class! loader (str module* "." (&/normalize-name _name)))
                                                                      def-value (get-field &/datum-field def-class)]
                                                                  (|do [_ (&a-module/define module _name (&/S &/$ValueD (&/P &type/Macro def-value)) &type/Macro)]
                                                                    (&a-module/declare-macro module _name)))
                                                            "V" (let [def-class (&&/load-class! loader (str module* "." (&/normalize-name _name)))
                                                                      ;; _ (println "Fetching _meta" module _name (str module* "." (&/normalize-name _name)) def-class)
                                                                      def-meta (get-field &/meta-field def-class)]
                                                                  (|case def-meta
                                                                    (&/$ValueD def-type _)
                                                                    (&a-module/define module _name def-meta def-type)))
                                                            ;; else
                                                            (let [[_ __module __name] (re-find #"^A(.*);(.*)$" _ann)]
                                                              (|do [__type (&a-module/def-type __module __name)]
                                                                (do ;; (prn '__type [__module __name] (&type/show-type __type))
                                                                    (&a-module/def-alias module _name __module __name __type)))))]
                                                    (if (= &&/exported-true _exported?)
                                                      (&a-module/export module _name)
                                                      (return nil)))
                                                  ))
                                              (if (= [""] defs)
                                                (&/|list)
                                                (&/->list defs)))
                                    _ (&/map% (fn [group]
                                                (|let [[_type _tags] group]
                                                  (|do [=type (&a-module/type-def module _type)]
                                                    (&a-module/declare-tags module _tags =type))))
                                              tag-groups)]
                                (return true))))
                          redo-cache)))
                    redo-cache)
                  ))
            redo-cache)))))
