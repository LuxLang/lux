(ns lux.compiler.core
  (:require (clojure [template :refer [do-template]]
                     [string :as string])
            [clojure.java.io :as io]
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return fail*]]
                 [type :as &type]
                 [host :as &host])
            (lux.analyser [base :as &a]
                          [module :as &a-module])
            (lux.compiler.cache [type :as &&&type]
                                [ann :as &&&ann]))
  (:import (java.io File
                    BufferedOutputStream
                    FileOutputStream)))

;; [Constants]
(def !output-dir (atom nil))

(def ^:const section-separator (->> 29 char str))
(def ^:const datum-separator (->> 31 char str))
(def ^:const entry-separator (->> 30 char str))

;; [Utils]
(defn write-file [^String file-name ^bytes data]
  (do (assert (not (.exists (File. file-name))) (str "Can't overwrite file: " file-name))
    (with-open [stream (BufferedOutputStream. (FileOutputStream. file-name))]
      (.write stream data)
      (.flush stream))))

;; [Exports]
(def ^String lux-module-descriptor-name "lux_module_descriptor")

(defn write-module-descriptor! [^String name ^String descriptor]
  (|do [_ (return nil)
        :let [lmd-dir (str @!output-dir java.io.File/separator (.replace name "/" java.io.File/separator))
              _ (.mkdirs (File. lmd-dir))
              _ (write-file (str lmd-dir java.io.File/separator lux-module-descriptor-name) (.getBytes descriptor java.nio.charset.StandardCharsets/UTF_8))]]
    (return nil)))

(defn read-module-descriptor! [^String name]
  (|do [_ (return nil)]
    (return (slurp (str @!output-dir java.io.File/separator (.replace name "/" java.io.File/separator) java.io.File/separator lux-module-descriptor-name)
                   :encoding "UTF-8"))))

(def generate-module-descriptor
  (|do [module-name &/get-module-name
        module-anns (&a-module/get-anns module-name)
        defs &a-module/defs
        imports &a-module/imports
        tag-groups &a-module/tag-groups
        :let [def-entries (->> defs
                               (&/|map (fn [_def]
                                         (|let [[?name ?alias [?def-type ?def-anns ?def-value]] _def]
                                           (if (= "" ?alias)
                                             (str ?name datum-separator (&&&type/serialize-type ?def-type) datum-separator (&&&ann/serialize-anns ?def-anns))
                                             (str ?name datum-separator ?alias)))))
                               (&/|interpose entry-separator)
                               (&/fold str ""))
              import-entries (->> imports
                                  (&/|map (fn [import]
                                            (|let [[_module _hash] import]
                                              (str _module datum-separator _hash))))
                                  (&/|interpose entry-separator)
                                  (&/fold str ""))
              tag-entries (->> tag-groups
                               (&/|map (fn [group]
                                         (|let [[type tags] group]
                                           (->> tags
                                                (&/|interpose datum-separator)
                                                (&/fold str "")
                                                (str type datum-separator)))))
                               (&/|interpose entry-separator)
                               (&/fold str ""))
              module-descriptor (->> (&/|list import-entries
                                              tag-entries
                                              (&&&ann/serialize-anns module-anns)
                                              def-entries)
                                     (&/|interpose section-separator)
                                     (&/fold str ""))]]
    (return module-descriptor)))
