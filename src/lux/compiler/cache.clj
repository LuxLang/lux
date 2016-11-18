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
                          [io :as &&io])
            (lux.compiler.cache [type :as &&&type]
                                [ann :as &&&ann]))
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
  (.exists (new File (str @&&/!output-dir "/" (&host/->module-class module) "/" module-class))))

(defn delete [module]
  "(-> Text (Lux Null))"
  (fn [state]
    (do (clean-file (new File (str @&&/!output-dir "/" (&host/->module-class module))))
      (return* state nil))))

(defn ^:private module-dirs
  "(-> File (clojure.Seq File))"
  [^File module]
  (->> module
       .listFiles
       (filter #(.isDirectory %))
       (map module-dirs)
       (apply concat)
       (list* module)))

(defn clean [state]
  "(-> Compiler Null)"
  (let [needed-modules (->> state (&/get$ &/$modules) &/|keys &/->seq set)
        output-dir-prefix (str (.getAbsolutePath (new File @&&/!output-dir)) "/")
        outdated? #(->> % (contains? needed-modules) not)
        outdated-modules (->> (new File @&&/!output-dir)
                              .listFiles (filter #(.isDirectory %))
                              (map module-dirs) doall (apply concat)
                              (map #(-> ^File % .getAbsolutePath (string/replace output-dir-prefix "")))
                              (filter outdated?))]
    (doseq [^String f outdated-modules]
      (clean-file (new File (str output-dir-prefix f))))
    nil))

(defn ^:private install-all-classes-in-module [!classes module* ^String module-path]
  (doseq [^File file (seq (.listFiles (File. module-path)))
          :when (not (.isDirectory file))
          :let [file-name (.getName file)]
          :when (not= module-class file-name)]
    (let [real-name (second (re-find #"^(.*)\.class$" file-name))
          bytecode (read-file file)]
      (swap! !classes assoc (str module* "." real-name) bytecode))))

(defn ^:private assume-async-result
  "(-> (Error Compiler) (Lux Null))"
  [result]
  (fn [_]
    (|case result
      (&/$Left error)
      (&/$Left error)

      (&/$Right compiler)
      (return* compiler nil))))

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
        (|let [redo-cache (|do [_ (delete module)
                                async (compile-module module)]
                            (assume-async-result @async))]
          (if (cached? module)
            (|do [loader &/loader
                  !classes &/classes
                  :let [module* (&host-generics/->class-name module)
                        module-path (str @&&/!output-dir "/" module)
                        class-name (str module* "._")
                        old-classes @!classes
                        ^Class module-class (do (swap! !classes assoc class-name (read-file (File. (str module-path "/_.class"))))
                                              (&&/load-class! loader class-name))
                        _ (install-all-classes-in-module !classes module* module-path)]]
              (if (and (= module-hash (get-field &/hash-field module-class))
                       (= &/compiler-version (get-field &/compiler-field module-class)))
                (let [imports (string/split (get-field &/imports-field module-class) entry-separator-re)]
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
                      (let [defs (string/split (get-field &/defs-field module-class) def-separator-re)
                            tag-groups (let [all-tags (get-field &/tags-field module-class)]
                                         (if (= "" all-tags)
                                           &/$Nil
                                           (-> all-tags
                                               (string/split tag-group-separator-re)
                                               (->> (map (fn [_group]
                                                           (let [[_type _tags] (string/split _group type-separator-re)]
                                                             (&/T [_type (&/->list (string/split (or _tags "") tag-separator-re))])))))
                                               &/->list)))]
                        (|do [_ (&a-module/create-module module module-hash)
                              ^String descriptor (&&/read-module-descriptor! module)
                              :let [module-anns (get-field &/anns-field module-class)]
                              _ (&a-module/set-anns module-anns module)
                              _ (&/flag-cached-module module)
                              _ (&a-module/set-imports imports)
                              :let [desc-defs (vec (.split descriptor &&/def-entry-separator))]
                              _ (&/map% (fn [^String _def-entry]
                                          (let [parts (.split _def-entry &&/def-datum-separator)]
                                            (case (alength parts)
                                              2 (let [[_name _alias] parts
                                                      [_ __module __name] (re-find #"^(.*);(.*)$" _alias)
                                                      def-class (&&/load-class! loader (str (&host-generics/->class-name __module) "." (&host/def-name __name)))
                                                      def-type (&a-module/def-type __module __name)
                                                      def-anns (&/|list (&/T [&a-meta/alias-tag (&/$IdentM (&/T [__module __name]))]))
                                                      def-value (get-field &/value-field def-class)]
                                                  (&a-module/define module _name def-type def-anns def-value))
                                              3 (let [[_name _type _anns] parts
                                                      def-class (&&/load-class! loader (str module* "." (&host/def-name _name)))
                                                      [def-anns _] (&&&ann/deserialize-anns _anns)
                                                      [def-type _] (&&&type/deserialize-type _type)
                                                      def-value (get-field &/value-field def-class)]
                                                  (&a-module/define module _name def-type def-anns def-value)))))
                                        (if (= [""] desc-defs)
                                          &/$Nil
                                          (&/->list desc-defs)))
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
