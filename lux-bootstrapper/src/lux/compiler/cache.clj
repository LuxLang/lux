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
                          [module :as &a-module])
            (lux.compiler [core :as &&core]
                          [io :as &&io])
            (lux.compiler.cache [type :as &&&type]))
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

(defn cached?
  "(-> Text Bit)"
  [module]
  (.exists (new File (str (module-path module) java.io.File/separator &&core/lux-module-descriptor-name))))

(defn delete
  "(-> Text (Lux Null))"
  [module]
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

(defn clean
  "(-> Lux Null)"
  [state]
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

(defn make-identifier [ident]
  (&/T [(&/T ["" 0 0]) (&/$Identifier ident)]))

(defn ^:private process-def-entry [load-def-value module ^String _def-entry]
  (let [parts (.split _def-entry &&core/datum-separator)]
    (case (first parts)
      "A" (let [[_ _name ^String _alias] parts
                [__module __name] (.split _alias &/+name-separator+)]
            (&a-module/define-alias module _name (&/T [__module __name])))
      "D" (let [[_ _name _exported? _type] parts
                [def-type _] (&&&type/deserialize-type _type)]
            (|do [def-value (load-def-value module _name)]
              (&a-module/define module _name (= "1" _exported?) def-type def-value)))
      ":" (let [[_ _name _exported? _record? _head _tail] parts
                labels (&/$Item _head (if _tail
                                        (&/->list (seq (.split _tail "\\.")))
                                        &/$End))]
            (|do [def-value (load-def-value module _name)]
              (&a-module/define-type
                module _name
                (= "1" _exported?)
                def-value (= "1" _record?) labels)))
      ;; "T" (let [[_ _name _exported? _type _index _group] parts
      ;;           [_type _] (&&&type/deserialize-type _type)
      ;;           _group (&/->list (seq (.split _group "\\.")))]
      ;;       (&a-module/define_tag module _name (= "1" _exported?) _type _group (Long/parseLong _index)))
      ;; "S" (let [[_ _name _exported? _type _index _group] parts
      ;;           [_type _] (&&&type/deserialize-type _type)
      ;;           _group (&/->list (seq (.split _group "\\.")))]
      ;;       (&a-module/define_slot module _name (= "1" _exported?) _type _group (Long/parseLong _index)))
      )))

(defn ^:private uninstall-cache [module]
  (|do [_ (delete module)]
    (return false)))

(defn ^:private install-module [load-def-value module module-hash imports def-entries]
  (|do [_ (&a-module/create-module module module-hash)
        _ (&a-module/flag-cached-module module)
        _ (&a-module/set-imports imports)
        _ (&/map% (partial process-def-entry load-def-value module)
                  def-entries)]
    (return nil)))

(defn ^:private process-module [pre-load! source-dirs cache-table module-name module-hash
                                _imports-section _defs-section
                                load-def-value install-all-defs-in-module uninstall-all-defs-in-module]
  (|do [^String descriptor (&&core/read-module-descriptor! module-name)
        :let [imports (let [imports (vec (.split ^String _imports-section &&core/entry-separator))
                            imports (if (= [""] imports)
                                      &/$End
                                      (&/->list imports))]
                        (&/|map #(first (vec (.split ^String % &&core/datum-separator 2))) imports))]
        cache-table* (&/fold% (fn [cache-table* _module]
                                (|do [[file-name file-content] (&&io/read-file source-dirs _module)
                                      output (pre-load! source-dirs cache-table* _module (hash file-content)
                                                        load-def-value install-all-defs-in-module uninstall-all-defs-in-module)]
                                  (return output)))
                              cache-table
                              imports)]
    (if (&/|every? (fn [_module] (contains? cache-table* _module))
                   imports)
      (let [def-entries (let [def-entries (vec (.split ^String _defs-section &&core/entry-separator))]
                          (if (= [""] def-entries)
                            &/$End
                            (&/->list def-entries)))]
        (|do [_ (install-all-defs-in-module module-name)
              _ (install-module load-def-value module-name module-hash imports def-entries)
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
              :let [[_compiler _hash _imports-section _defs-section] (.split descriptor &&core/section-separator)
                    drop-cache! (|do [_ (uninstall-cache module-name)
                                      _ (uninstall-all-defs-in-module module-name)]
                                  (return cache-table))]]
          (if (and (= module-hash (Long/parseUnsignedLong ^String _hash))
                   (= &/version _compiler))
            (|do [[success? cache-table*] (process-module pre-load! source-dirs cache-table module-name module-hash
                                                          _imports-section _defs-section
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
  "(-> Module Lux (Lux Null))"
  [module-name module]
  (fn [compiler]
    (return* (&/update$ &/$modules
                        #(&/|put module-name module %)
                        compiler)
             nil)))

(defn load
  "(-> Text (Lux Null))"
  [module-name]
  (if-let [module-struct (get @!pre-loaded-cache module-name)]
    (|do [_ (inject-module module-name module-struct)]
      (return nil))
    (&/fail (str "[Cache Error] Module is not cached: " module-name))))
