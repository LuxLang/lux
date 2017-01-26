(ns lux.compiler.base
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
            [lux.host.generics :as &host-generics])
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)
           (java.io File
                    BufferedOutputStream
                    FileOutputStream)
           (java.lang.reflect Field)))

;; [Constants]
(def !output-dir (atom nil))

(def ^:const ^String function-class "lux/Function")
(def ^:const ^String lux-utils-class "lux/LuxRT")
(def ^:const ^String unit-tag-field "unit_tag")

;; Formats
(def ^:const ^String local-prefix "l")
(def ^:const ^String partial-prefix "p")
(def ^:const ^String closure-prefix "c")
(def ^:const ^String apply-method "apply")
(defn ^String apply-signature [n]
  (str "(" (apply str (repeat n "Ljava/lang/Object;")) ")Ljava/lang/Object;"))
(def ^:const num-apply-variants 8)
(def ^:const arity-field "_arity_")
(def ^:const partials-field "_partials_")

(def ^:const section-separator (->> 29 char str))
(def ^:const datum-separator (->> 31 char str))
(def ^:const entry-separator (->> 30 char str))

;; [Utils]
(defn ^:private write-file [^String file-name ^bytes data]
  (do (assert (not (.exists (File. file-name))) (str "Can't overwrite file: " file-name))
    (with-open [stream (BufferedOutputStream. (FileOutputStream. file-name))]
      (.write stream data)
      (.flush stream))))

(defn ^:private write-output [module name data]
  (let [^String module* (&host/->module-class module)
        module-dir (str @!output-dir java.io.File/separator (.replace module* "/" java.io.File/separator))]
    (.mkdirs (File. module-dir))
    (write-file (str module-dir java.io.File/separator name ".class") data)))

(defn class-exists? [^String module ^String class-name]
  "(-> Text Text (IO Bool))"
  (|do [_ (return nil)
        :let [full-path (str @!output-dir java.io.File/separator module java.io.File/separator class-name ".class")
              exists? (.exists (File. full-path))]]
    (return exists?)))

;; [Exports]
(defn ^Class load-class! [^ClassLoader loader name]
  ;; (prn 'load-class! name)
  (.loadClass loader name))

(defn save-class! [name bytecode]
  (|do [eval? &/get-eval
        module &/get-module-name
        loader &/loader
        !classes &/classes
        :let [real-name (str (&host-generics/->class-name module) "." name)
              _ (swap! !classes assoc real-name bytecode)
              _ (when (not eval?)
                  (write-output module name bytecode))
              _ (load-class! loader real-name)]]
    (return nil)))

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

(do-template [<wrap-name> <unwrap-name> <class> <unwrap-method> <prim> <dup>]
  (do (defn <wrap-name> [^MethodVisitor writer]
        (doto writer
          (.visitMethodInsn Opcodes/INVOKESTATIC <class> "valueOf" (str "(" <prim> ")" (&host-generics/->type-signature <class>)))))
    (defn <unwrap-name> [^MethodVisitor writer]
      (doto writer
        (.visitTypeInsn Opcodes/CHECKCAST <class>)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL <class> <unwrap-method> (str "()" <prim>)))))

  wrap-boolean unwrap-boolean "java/lang/Boolean"   "booleanValue" "Z" Opcodes/DUP_X1
  wrap-byte    unwrap-byte    "java/lang/Byte"      "byteValue"    "B" Opcodes/DUP_X1
  wrap-short   unwrap-short   "java/lang/Short"     "shortValue"   "S" Opcodes/DUP_X1
  wrap-int     unwrap-int     "java/lang/Integer"   "intValue"     "I" Opcodes/DUP_X1
  wrap-long    unwrap-long    "java/lang/Long"      "longValue"    "J" Opcodes/DUP_X2
  wrap-float   unwrap-float   "java/lang/Float"     "floatValue"   "F" Opcodes/DUP_X1
  wrap-double  unwrap-double  "java/lang/Double"    "doubleValue"  "D" Opcodes/DUP_X2
  wrap-char    unwrap-char    "java/lang/Character" "charValue"    "C" Opcodes/DUP_X1
  )
