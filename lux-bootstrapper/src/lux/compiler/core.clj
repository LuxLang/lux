(ns lux.compiler.core
  (:require (clojure [template :refer [do-template]]
                     [string :as string])
            [clojure.java.io :as io]
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|case |let |do return* return fail*]])
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
  (do (assert (not (.exists (File. file-name))) (str "Cannot overwrite file: " file-name))
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

(defn generate-module-descriptor [file-hash]
  (|do [module-name &/get-module-name
        defs &a-module/defs
        imports &a-module/imports
        :let [def-entries (&/fold (fn [def-entries _def]
                                    (|let [[?name _definition] _def]
                                      (|case _definition
                                        (&/$AliasG [_dmodule _dname])
                                        (str "A"
                                             datum-separator ?name
                                             datum-separator _dmodule &/+name-separator+ _dname
                                             ;; Next
                                             entry-separator def-entries)
                                        
                                        (&/$DefinitionG [exported? ?def-type ?def-anns ?def-value])
                                        (str "D"
                                             datum-separator ?name
                                             datum-separator (if exported? "1" "0")
                                             datum-separator (&&&type/serialize-type ?def-type)
                                             datum-separator (&&&ann/serialize ?def-anns)
                                             ;; Next
                                             entry-separator def-entries)

                                        (&/$TypeG [exported? value labels])
                                        (let [[record? head tail] (|case labels
                                                                    (&/$Left [head tail])
                                                                    [false head tail]
                                                                    
                                                                    (&/$Right [head tail])
                                                                    [true head tail])]
                                          (str ":"
                                               datum-separator ?name
                                               datum-separator (if exported? "1" "0")
                                               datum-separator (if record? "1" "0")
                                               datum-separator head
                                               datum-separator (->> tail
                                                                    (&/|interpose &/+name-separator+)
                                                                    (&/fold str ""))
                                               ;; Next
                                               entry-separator def-entries))

                                        (&/$TagG [?export ?type ?group ?index])
                                        def-entries
                                        ;; (str "T"
                                        ;;      datum-separator ?name
                                        ;;      datum-separator (if ?export "1" "0")
                                        ;;      datum-separator (&&&type/serialize-type ?type)
                                        ;;      datum-separator ?index
                                        ;;      datum-separator (->> ?group
                                        ;;                           (&/|interpose &/+name-separator+)
                                        ;;                           (&/fold str "")))

                                        (&/$SlotG [?export ?type ?group ?index])
                                        def-entries
                                        ;; (str "S"
                                        ;;      datum-separator ?name
                                        ;;      datum-separator (if ?export "1" "0")
                                        ;;      datum-separator (&&&type/serialize-type ?type)
                                        ;;      datum-separator ?index
                                        ;;      datum-separator (->> ?group
                                        ;;                           (&/|interpose &/+name-separator+)
                                        ;;                           (&/fold str "")))
                                        )))
                                  ""
                                  defs)
              import-entries (->> imports
                                  (&/|map (fn [import]
                                            (|let [[_module _hash] import]
                                              (str _module datum-separator _hash))))
                                  (&/|interpose entry-separator)
                                  (&/fold str ""))
              module-descriptor (->> (&/|list &/version
                                              (Long/toUnsignedString file-hash)
                                              import-entries
                                              def-entries)
                                     (&/|interpose section-separator)
                                     (&/fold str ""))]]
    (return module-descriptor)))
