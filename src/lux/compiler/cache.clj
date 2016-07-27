;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.compiler.cache
  (:refer-clojure :exclude [load])
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |case |let]]
                 [type :as &type]
                 [host :as &host])
            [lux.host.generics :as &host-generics]
            (lux.analyser [base :as &a]
                          [module :as &a-module]
                          [meta :as &a-meta])
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
  (doseq [^File f (seq (.listFiles file))
          :when (not (.isDirectory f))]
    (.delete f)))

(defn ^:private get-field [^String field-name ^Class class]
  "(-> Text Class Object)"
  (-> class ^Field (.getField field-name) (.get nil)))

;; [Resources]
(def module-class (str &/module-class-name ".class"))

(defn cached? [module]
  "(-> Text Bool)"
  (.exists (new File (str &&/output-dir "/" (&host/->module-class module) "/" module-class))))

(defn delete [module]
  "(-> Text (Lux Null))"
  (fn [state]
    (do (clean-file (new File (str &&/output-dir "/" (&host/->module-class module))))
      (return* state nil))))

(defn clean [state]
  "(-> Compiler Null)"
  (let [needed-modules (->> state (&/get$ &/$modules) &/|keys &/->seq set)
        outdated? #(-> ^File % .getName (string/replace &host/module-separator "/") (->> (contains? needed-modules)) not)
        outdate-files (->> &&/output-dir (new File) .listFiles seq (filter outdated?))
        program-file (new File &&/output-package)]
    (when (.exists program-file)
      (.delete program-file))
    (doseq [f outdate-files]
      (clean-file f))
    nil))

(defn ^:private install-all-classes-in-module [!classes module* ^String module-path]
  (doseq [^File file (seq (.listFiles (File. module-path)))
          :when (not (.isDirectory file))
          :let [file-name (.getName file)]
          :when (not= module-class file-name)]
    (let [real-name (second (re-find #"^(.*)\.class$" file-name))
          bytecode (read-file file)]
      (swap! !classes assoc (str module* "." real-name) bytecode))))

(let [->regex (fn [text] (re-pattern (java.util.regex.Pattern/quote text)))
      entry-separator-re (->regex &&/entry-separator)
      field-separator-re (->regex &&/field-separator)
      type-separator-re (->regex &&/type-separator)
      tag-separator-re (->regex &&/tag-separator)
      def-separator-re (->regex &&/def-separator)
      tag-group-separator-re (->regex &&/tag-group-separator)]
  (defn load [source-dirs module module-hash compile-module]
    "(-> (List Text) Text Int (-> Text (Lux (,))) (Lux Bool))"
    (|do [already-loaded? (&a-module/exists? module)]
      (if already-loaded?
        (return module-hash)
        (|let [redo-cache (|do [_ (delete module)]
                            (compile-module source-dirs module))]
          (if (cached? module)
            (|do [loader &/loader
                  !classes &/classes
                  :let [module* (&host-generics/->class-name module)
                        module-path (str &&/output-dir "/" module)
                        class-name (str module* "._")
                        old-classes @!classes
                        ^Class module-meta (do (swap! !classes assoc class-name (read-file (File. (str module-path "/_.class"))))
                                             (&&/load-class! loader class-name))
                        _ (install-all-classes-in-module !classes module* module-path)]]
              (if (and (= module-hash (get-field &/hash-field module-meta))
                       (= &/compiler-version (get-field &/compiler-field module-meta)))
                (let [imports (string/split (get-field &/imports-field module-meta) entry-separator-re)]
                  (|do [loads (&/map% (fn [_import]
                                        (let [[_module _hash] (string/split _import field-separator-re)]
                                          (|do [file-content (&&io/read-file source-dirs (str _module ".lux"))
                                                :let [file-hash (hash file-content)
                                                      __hash (Integer/parseInt _hash)]
                                                _ (load source-dirs _module file-hash compile-module)
                                                cached? (&/cached-module? _module)
                                                :let [consistent-cache? (= file-hash __hash)]]
                                            (return (and cached?
                                                         consistent-cache?)))))
                                      (if (= [""] imports)
                                        &/$Nil
                                        (&/->list imports)))]
                    (if (->> loads &/->seq (every? true?))
                      (let [defs (string/split (get-field &/defs-field module-meta) def-separator-re)
                            tag-groups (let [all-tags (get-field &/tags-field module-meta)]
                                         (if (= "" all-tags)
                                           &/$Nil
                                           (-> all-tags
                                               (string/split tag-group-separator-re)
                                               (->> (map (fn [_group]
                                                           (let [[_type _tags] (string/split _group type-separator-re)]
                                                             (&/T [_type (&/->list (string/split (or _tags "") tag-separator-re))])))))
                                               &/->list)))]
                        (|do [_ (&a-module/create-module module module-hash)
                              _ (&/flag-cached-module module)
                              _ (&a-module/set-imports imports)
                              _ (&/map% (fn [_def]
                                          (let [[_name _alias] (string/split _def #" ")]
                                            (if (= nil _alias)
                                              (let [def-class (&&/load-class! loader (str module* "." (&host/def-name _name)))
                                                    def-meta (get-field &/meta-field def-class)
                                                    def-type (|case (&a-meta/meta-get &a-meta/type?-tag def-meta)
                                                               (&/$Some (&/$BoolM true))
                                                               &type/Type

                                                               _
                                                               (get-field &/type-field def-class))
                                                    def-value (get-field &/value-field def-class)]
                                                (&a-module/define module _name def-type def-meta def-value))
                                              (let [[_ __module __name] (re-find #"^(.*);(.*)$" _alias)
                                                    def-class (&&/load-class! loader (str (&host-generics/->class-name __module) "." (&host/def-name __name)))
                                                    def-type (get-field &/type-field def-class)
                                                    def-meta (&/|list (&/T [&a-meta/alias-tag (&/$IdentM (&/T [__module __name]))]))
                                                    def-value (get-field &/value-field def-class)]
                                                (&a-module/define module _name def-type def-meta def-value)))
                                            ))
                                        (if (= [""] defs)
                                          &/$Nil
                                          (&/->list defs)))
                              _ (&/map% (fn [group]
                                          (|let [[_type _tags] group]
                                            (|do [[was-exported? =type] (&a-module/type-def module _type)]
                                              (&a-module/declare-tags module _tags was-exported? =type))))
                                        tag-groups)]
                          (return module-hash)))
                      redo-cache)))
                (do (reset! !classes old-classes)
                  redo-cache)))
            redo-cache))))))
