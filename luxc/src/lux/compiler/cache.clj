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
(def module-class-file (str &/module-class-name ".class"))

(defn cached? [module]
  "(-> Text Bool)"
  (.exists (new File (str @&&/!output-dir
                          java.io.File/separator
                          (.replace ^String (&host/->module-class module) "/" java.io.File/separator)
                          java.io.File/separator
                          module-class-file))))

(defn delete [module]
  "(-> Text (Lux Null))"
  (fn [state]
    (do (clean-file (new File (str @&&/!output-dir
                                   java.io.File/separator
                                   (.replace ^String (&host/->module-class module) "/" java.io.File/separator))))
      (return* state nil))))

(defn ^:private module-dirs
  "(-> File (clojure.Seq File))"
  [^File module]
  (->> module
       .listFiles
       (filter #(.isDirectory ^File %))
       (map module-dirs)
       (apply concat)
       (list* module)))

(defn clean [state]
  "(-> Compiler Null)"
  (let [needed-modules (->> state (&/get$ &/$modules) &/|keys &/->seq set)
        output-dir-prefix (str (.getAbsolutePath (new File ^String @&&/!output-dir)) java.io.File/separator)
        outdated? #(->> % (contains? needed-modules) not)
        outdated-modules (->> (new File ^String @&&/!output-dir)
                              .listFiles (filter #(.isDirectory ^File %))
                              (map module-dirs) doall (apply concat)
                              (map (fn [^File dir-file]
                                     (let [^String dir-module (-> dir-file
                                                                  .getAbsolutePath
                                                                  (string/replace output-dir-prefix ""))
                                           corrected-dir-module (.replace dir-module java.io.File/separator "/")]
                                       corrected-dir-module)))
                              (filter outdated?))]
    (doseq [^String f outdated-modules]
      (clean-file (new File (str output-dir-prefix f))))
    nil))

(defn ^:private install-all-classes-in-module [!classes module* ^String module-path]
  (let [classes+bytecode (for [^File file (seq (.listFiles (File. module-path)))
                               :when (not (.isDirectory file))
                               :let [file-name (.getName file)]
                               :when (not= module-class-file file-name)]
                           [(second (re-find #"^(.*)\.class$" file-name))
                            (read-file file)])
        _ (doseq [[class-name bytecode] classes+bytecode]
            (swap! !classes assoc (str module* "." class-name) bytecode))]
    (map first classes+bytecode)))

(defn ^:private assume-async-result
  "(-> (Error Compiler) (Lux Null))"
  [result]
  (fn [_]
    (|case result
      (&/$Left error)
      (&/$Left error)

      (&/$Right compiler)
      (return* compiler nil))))

(defn ^:private parse-tag-groups [^String tags-section]
  (if (= "" tags-section)
    &/$Nil
    (-> tags-section
        (.split &&/entry-separator)
        seq
        (->> (map (fn [^String _group]
                    (let [[_type & _tags] (.split _group &&/datum-separator)]
                      (&/T [_type (->> _tags seq &/->list)])))))
        &/->list)))

(defn ^:private process-tag-group [module group]
  (|let [[_type _tags] group]
    (|do [[was-exported? =type] (&a-module/type-def module _type)]
      (&a-module/declare-tags module _tags was-exported? =type))))

(defn ^:private process-def-entry [loader module ^String _def-entry]
  (let [parts (.split _def-entry &&/datum-separator)]
    (case (alength parts)
      2 (let [[_name _alias] parts
              [_ __module __name] (re-find #"^(.*);(.*)$" _alias)
              def-class (&&/load-class! loader (str (&host-generics/->class-name __module) "." (&host/def-name __name)))
              def-anns (&/|list (&/T [&a-meta/alias-tag (&/$IdentM (&/T [__module __name]))]))
              def-value (get-field &/value-field def-class)]
          (|do [def-type (&a-module/def-type __module __name)]
            (&a-module/define module _name def-type def-anns def-value)))
      3 (let [[_name _type _anns] parts
              def-class (&&/load-class! loader (str (&host-generics/->class-name module) "." (&host/def-name _name)))
              def-anns (&&&ann/deserialize-anns _anns)
              [def-type _] (&&&type/deserialize-type _type)
              def-value (get-field &/value-field def-class)]
          (&a-module/define module _name def-type def-anns def-value)))))

(defn ^:private uninstall-cache [module]
  (|do [_ (delete module)]
    (return false)))

(defn ^:private install-module [loader module module-hash imports tag-groups module-anns def-entries]
  (|do [_ (&a-module/create-module module module-hash)
        _ (&a-module/set-anns module-anns module)
        _ (&a-module/set-imports imports)
        _ (&/map% (partial process-def-entry loader module)
                  def-entries)
        _ (&/map% (partial process-tag-group module) tag-groups)]
    (return nil)))

(defn ^:private process-module [pre-load! source-dirs cache-table module-name module-hash loader]
  (|do [^String descriptor (&&/read-module-descriptor! module-name)
        :let [[imports-section tags-section module-anns-section defs-section] (.split descriptor &&/section-separator)
              imports (let [imports (vec (.split ^String imports-section &&/entry-separator))
                            imports (if (= [""] imports)
                                      &/$Nil
                                      (&/->list imports))]
                        (&/|map #(.split ^String % &&/datum-separator 2) imports))]
        cache-table* (&/fold% (fn [cache-table* _import]
                                (|do [:let [[_module _hash] _import]
                                      file-content (&&io/read-file source-dirs (str _module ".lux"))
                                      output (pre-load! source-dirs cache-table* _module (hash file-content))]
                                  (return output)))
                              cache-table
                              imports)]
    (if (&/|every? (fn [_import]
                     (|let [[_module _hash] _import]
                       (contains? cache-table* _module)))
                   imports)
      (let [tag-groups (parse-tag-groups tags-section)
            module-anns (&&&ann/deserialize-anns module-anns-section)
            def-entries (let [def-entries (vec (.split ^String defs-section &&/entry-separator))]
                          (if (= [""] def-entries)
                            &/$Nil
                            (&/->list def-entries)))]
        (|do [_ (install-module loader module-name module-hash
                                imports tag-groups module-anns def-entries)
              =module (&/find-module module-name)]
          (return (&/T [true (assoc cache-table* module-name =module)]))))
      (return (&/T [false cache-table*])))))

(defn ^:private enumerate-cached-modules!* [^File parent]
  (if (.isDirectory parent)
    (let [children (for [^File child (seq (.listFiles parent))
                         entry (enumerate-cached-modules!* child)]
                     entry)]
      (if (.exists (new File parent "_.class"))
        (list* (.getAbsolutePath parent)
               children)
        children))
    (list)))

(defn ^:private enumerate-cached-modules! []
  (let [output-dir (new File ^String @&&/!output-dir)
        prefix-to-subtract (inc (.length (.getAbsolutePath output-dir)))]
    (->> output-dir
         enumerate-cached-modules!*
         rest
         (map #(-> ^String %
                   (.replace java.io.File/separator "/")
                   (.substring prefix-to-subtract)))
         &/->list)))

(defn ^:private pre-load! [source-dirs cache-table module module-hash]
  (cond (contains? cache-table module)
        (return cache-table)

        (not (cached? module))
        (return cache-table)

        :else
        (|do [loader &/loader
              !classes &/classes
              :let [module* (&host-generics/->class-name module)
                    module-path (str @&&/!output-dir java.io.File/separator module)
                    class-name (str module* "." &/module-class-name)
                    ^Class module-class (do (swap! !classes assoc class-name (read-file (new File (str module-path java.io.File/separator module-class-file))))
                                          (&&/load-class! loader class-name))
                    installed-classes (install-all-classes-in-module !classes module* module-path)
                    valid-cache? (and (= module-hash (get-field &/hash-field module-class))
                                      (= &/compiler-version (get-field &/compiler-field module-class)))
                    drop-cache! (|do [_ (uninstall-cache module)
                                      :let [_ (swap! !classes (fn [_classes-dict]
                                                                (reduce dissoc _classes-dict installed-classes)))]]
                                  (return cache-table))]]
          (if valid-cache?
            (|do [[success? cache-table*] (process-module pre-load! source-dirs cache-table module module-hash loader)
                  _ (if success?
                      (return nil)
                      drop-cache!)]
              (return cache-table*))
            drop-cache!))))

(def !pre-loaded-cache (atom nil))
(defn pre-load-cache! [source-dirs]
  (|do [:let [fs-cached-modules (enumerate-cached-modules!)]
        pre-loaded-modules (&/fold% (fn [cache-table module-name]
                                      (fn [_compiler]
                                        (|case ((&&io/read-file source-dirs (str module-name ".lux"))
                                                _compiler)
                                          (&/$Left error)
                                          (return* _compiler cache-table)

                                          (&/$Right _compiler* file-content)
                                          ((pre-load! source-dirs cache-table module-name (hash file-content))
                                           _compiler*))))
                                    {}
                                    fs-cached-modules)
        :let [_ (reset! !pre-loaded-cache pre-loaded-modules)]]
    (return nil)))

(defn ^:private inject-module
  "(-> (Module Compiler) (-> Compiler (Lux Null)))"
  [module-name module]
  (fn [compiler]
    (return* (&/update$ &/$modules
                        #(&/|put module-name module %)
                        compiler)
             nil)))

(defn load [module-name]
  "(-> Text (Lux Null))"
  (if-let [module-struct (get @!pre-loaded-cache module-name)]
    (|do [_ (inject-module module-name module-struct)
          _ (&/flag-cached-module module-name)]
      (return nil))
    (fail (str "[Cache Error] Module is not cached: " module-name))))
