(ns lux.compiler.cache
  (:refer-clojure :exclude [load])
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return |case |let]]
                 [type :as &type]
                 [host :as &host])
            (lux.analyser [base :as &a]
                          [module :as &a-module]
                          [meta :as &a-meta])
            (lux.compiler [core :as &&core]
                          [io :as &&io])
            (lux.compiler.cache [type :as &&&type]
                                [ann :as &&&ann]))
  (:import (java.io File)
           ))

;; [Resources]
(defn ^:private delete-all-module-files [^File file]
  (doseq [^File f (seq (.listFiles file))
          :when (not (.isDirectory f))]
    (.delete f)))

(defn ^:private ^String module-path [module]
  (str @&&core/!output-dir
       java.io.File/separator
       (.replace ^String (&host/->module-class module) "/" java.io.File/separator)))

(defn cached? [module]
  "(-> Text Bool)"
  (.exists (new File (str (module-path module) java.io.File/separator &&core/lux-module-descriptor-name))))

(defn delete [module]
  "(-> Text (Lux Null))"
  (fn [state]
    (do (delete-all-module-files (new File (module-path module)))
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
        output-dir-prefix (str (.getAbsolutePath (new File ^String @&&core/!output-dir)) java.io.File/separator)
        outdated? #(->> % (contains? needed-modules) not)
        outdated-modules (->> (new File ^String @&&core/!output-dir)
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
      (delete-all-module-files (new File (str output-dir-prefix f))))
    nil))

(defn ^:private parse-tag-groups [^String tags-section]
  (if (= "" tags-section)
    &/$Nil
    (-> tags-section
        (.split &&core/entry-separator)
        seq
        (->> (map (fn [^String _group]
                    (let [[_type & _tags] (.split _group &&core/datum-separator)]
                      (&/T [_type (->> _tags seq &/->list)])))))
        &/->list)))

(defn ^:private process-tag-group [module group]
  (|let [[_type _tags] group]
    (|do [[was-exported? =type] (&a-module/type-def module _type)]
      (&a-module/declare-tags module _tags was-exported? =type))))

(defn make-tag [ident]
  (&/T [(&/T ["" 0 0]) (&/$Tag ident)]))

(defn make-symbol [ident]
  (&/T [(&/T ["" 0 0]) (&/$Symbol ident)]))

(defn make-record [ident]
  (&/T [(&/T ["" 0 0]) (&/$Record ident)]))

(defn ^:private process-def-entry [load-def-value module ^String _def-entry]
  (let [parts (.split _def-entry &&core/datum-separator)]
    (case (alength parts)
      2 (let [[_name _alias] parts
              [_ __module __name] (re-find #"^(.*);(.*)$" _alias)
              def-anns (make-record (&/|list (&/T [(make-tag &a-meta/alias-tag)
                                                   (make-symbol (&/T [__module __name]))])))]
          (|do [def-type (&a-module/def-type __module __name)
                def-value (load-def-value __module __name)]
            (&a-module/define module _name def-type def-anns def-value)))
      3 (let [[_name _type _anns] parts
              [def-anns _] (&&&ann/deserialize _anns)
              [def-type _] (&&&type/deserialize-type _type)]
          (|do [def-value (load-def-value module _name)]
            (&a-module/define module _name def-type def-anns def-value))))))

(defn ^:private uninstall-cache [module]
  (|do [_ (delete module)]
    (return false)))

(defn ^:private install-module [load-def-value module module-hash imports tag-groups module-anns def-entries]
  (|do [_ (&a-module/create-module module module-hash)
        _ (&a-module/flag-cached-module module)
        _ (&a-module/set-anns module-anns module)
        _ (&a-module/set-imports imports)
        _ (&/map% (partial process-def-entry load-def-value module)
                  def-entries)
        _ (&/map% (partial process-tag-group module) tag-groups)]
    (return nil)))

(defn ^:private process-module [pre-load! source-dirs cache-table module-name module-hash
                                _imports-section _tags-section _module-anns-section _defs-section
                                load-def-value install-all-defs-in-module uninstall-all-defs-in-module]
  (|do [^String descriptor (&&core/read-module-descriptor! module-name)
        :let [imports (let [imports (vec (.split ^String _imports-section &&core/entry-separator))
                            imports (if (= [""] imports)
                                      &/$Nil
                                      (&/->list imports))]
                        (&/|map #(.split ^String % &&core/datum-separator 2) imports))]
        cache-table* (&/fold% (fn [cache-table* _import]
                                (|do [:let [[_module _hash] _import]
                                      [file-name file-content] (&&io/read-file source-dirs _module)
                                      output (pre-load! source-dirs cache-table* _module (hash file-content)
                                                        load-def-value install-all-defs-in-module uninstall-all-defs-in-module)]
                                  (return output)))
                              cache-table
                              imports)]
    (if (&/|every? (fn [_import]
                     (|let [[_module _hash] _import]
                       (contains? cache-table* _module)))
                   imports)
      (let [tag-groups (parse-tag-groups _tags-section)
            [module-anns _] (&&&ann/deserialize _module-anns-section)
            def-entries (let [def-entries (vec (.split ^String _defs-section &&core/entry-separator))]
                          (if (= [""] def-entries)
                            &/$Nil
                            (&/->list def-entries)))]
        (|do [_ (install-all-defs-in-module module-name)
              _ (install-module load-def-value module-name module-hash
                                imports tag-groups module-anns def-entries)
              =module (&/find-module module-name)]
          (return (&/T [true (assoc cache-table* module-name =module)]))))
      (return (&/T [false cache-table*])))))

(defn ^:private enumerate-cached-modules!* [^File parent]
  (if (.isDirectory parent)
    (let [children (for [^File child (seq (.listFiles parent))
                         entry (enumerate-cached-modules!* child)]
                     entry)]
      (if (.exists (new File parent &&core/lux-module-descriptor-name))
        (list* (.getAbsolutePath parent)
               children)
        children))
    (list)))

(defn ^:private enumerate-cached-modules! []
  (let [output-dir (new File ^String @&&core/!output-dir)
        prefix-to-subtract (inc (.length (.getAbsolutePath output-dir)))]
    (->> output-dir
         enumerate-cached-modules!*
         rest
         (map #(-> ^String %
                   (.replace java.io.File/separator "/")
                   (.substring prefix-to-subtract)))
         &/->list)))

(defn ^:private pre-load! [source-dirs cache-table module-name module-hash
                           load-def-value install-all-defs-in-module uninstall-all-defs-in-module]
  (cond (contains? cache-table module-name)
        (return cache-table)

        (not (cached? module-name))
        (return cache-table)

        :else
        (|do [^String descriptor (&&core/read-module-descriptor! module-name)
              :let [[_compiler _hash _imports-section _tags-section _module-anns-section _defs-section] (.split descriptor &&core/section-separator)
                    drop-cache! (|do [_ (uninstall-cache module-name)
                                      _ (uninstall-all-defs-in-module module-name)]
                                  (return cache-table))]]
          (if (and (= module-hash (Long/parseUnsignedLong ^String _hash))
                   (= &/compiler-version _compiler))
            (|do [[success? cache-table*] (process-module pre-load! source-dirs cache-table module-name module-hash
                                                          _imports-section _tags-section _module-anns-section _defs-section
                                                          load-def-value install-all-defs-in-module uninstall-all-defs-in-module)
                  _ (if success?
                      (return nil)
                      drop-cache!)]
              (return cache-table*))
            drop-cache!))))

(def ^:private !pre-loaded-cache (atom nil))
(defn pre-load-cache! [source-dirs
                       load-def-value install-all-defs-in-module uninstall-all-defs-in-module]
  (|do [:let [fs-cached-modules (enumerate-cached-modules!)]
        pre-loaded-modules (&/fold% (fn [cache-table module-name]
                                      (fn [_compiler]
                                        (|case ((&&io/read-file source-dirs module-name)
                                                _compiler)
                                          (&/$Left error)
                                          (return* _compiler cache-table)

                                          (&/$Right _compiler* [file-name file-content])
                                          ((pre-load! source-dirs cache-table module-name (hash file-content)
                                                      load-def-value install-all-defs-in-module uninstall-all-defs-in-module)
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
    (|do [_ (inject-module module-name module-struct)]
      (return nil))
    (&/fail (str "[Cache Error] Module is not cached: " module-name))))
