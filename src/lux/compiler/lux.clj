(ns lux.compiler.lux
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [exec return* return fail fail*]]
                 [type :as &type]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser]
                 [host :as &host])
            [lux.analyser.base :as &a]
            (lux.compiler [base :as &&]
                          [lambda :as &&lambda])
            :reload)
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Exports]
(let [+class+ (&host/->class "java.lang.Boolean")
      +sig+ (&host/->type-signature "java.lang.Boolean")]
  (defn compile-bool [compile *type* ?value]
    (exec [*writer* &/get-writer
           :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC (&host/->class "java.lang.Boolean") (if ?value "TRUE" "FALSE") (&host/->type-signature "java.lang.Boolean"))]]
      (return nil))))

(do-template [<name> <class> <sig> <caster>]
  (let [+class+ (&host/->class <class>)]
    (defn <name> [compile *type* value]
      (exec [*writer* &/get-writer
             :let [_ (doto *writer*
                       (.visitTypeInsn Opcodes/NEW +class+)
                       (.visitInsn Opcodes/DUP)
                       (.visitLdcInsn (<caster> value))
                       (.visitMethodInsn Opcodes/INVOKESPECIAL +class+ "<init>" <sig>))]]
        (return nil))))

  compile-int  "java.lang.Long"      "(J)V" long
  compile-real "java.lang.Double"    "(D)V" double
  compile-char "java.lang.Character" "(C)V" char
  )

(defn compile-text [compile *type* ?value]
  (exec [*writer* &/get-writer
         :let [_ (.visitLdcInsn *writer* ?value)]]
    (return nil)))

(defn compile-tuple [compile *type* ?elems]
  (exec [*writer* &/get-writer
         :let [num-elems (count ?elems)
               _ (doto *writer*
                   (.visitLdcInsn (int num-elems))
                   (.visitTypeInsn Opcodes/ANEWARRAY (&host/->class "java.lang.Object")))]
         _ (&/map% (fn [[idx elem]]
                     (exec [:let [_ (doto *writer*
                                      (.visitInsn Opcodes/DUP)
                                      (.visitLdcInsn (int idx)))]
                            ret (compile elem)
                            :let [_ (.visitInsn *writer* Opcodes/AASTORE)]]
                       (return ret)))
                   (map vector (range num-elems) ?elems))]
    (return nil)))

(defn compile-record [compile *type* ?elems]
  (exec [*writer* &/get-writer
         :let [num-elems (count ?elems)
               _ (doto *writer*
                   (.visitLdcInsn (int (* 2 num-elems)))
                   (.visitTypeInsn Opcodes/ANEWARRAY (&host/->class "java.lang.Object")))]
         _ (&/map% (fn [[idx [k v]]]
                     (exec [:let [idx* (* 2 idx)
                                  _ (doto *writer*
                                      (.visitInsn Opcodes/DUP)
                                      (.visitLdcInsn (int idx*))
                                      (.visitLdcInsn k)
                                      (.visitInsn Opcodes/AASTORE))]
                            :let [_ (doto *writer*
                                      (.visitInsn Opcodes/DUP)
                                      (.visitLdcInsn (int (inc idx*))))]
                            ret (compile v)
                            :let [_ (.visitInsn *writer* Opcodes/AASTORE)]]
                       (return ret)))
                   (map vector (range num-elems) ?elems))]
    (return nil)))

(defn compile-variant [compile *type* ?tag ?value]
  (exec [*writer* &/get-writer
         :let [_ (doto *writer*
                   (.visitLdcInsn (int 2))
                   (.visitTypeInsn Opcodes/ANEWARRAY (&host/->class "java.lang.Object"))
                   (.visitInsn Opcodes/DUP)
                   (.visitLdcInsn (int 0))
                   (.visitLdcInsn ?tag)
                   (.visitInsn Opcodes/AASTORE)
                   (.visitInsn Opcodes/DUP)
                   (.visitLdcInsn (int 1)))]
         _ (compile ?value)
         :let [_ (.visitInsn *writer* Opcodes/AASTORE)]]
    (return nil)))

(defn compile-local [compile *type* ?idx]
  (exec [*writer* &/get-writer
         :let [_ (.visitVarInsn *writer* Opcodes/ALOAD (int ?idx))]]
    (return nil)))

(defn compile-captured [compile *type* ?scope ?captured-id ?source]
  ;; (prn 'compile-captured ?scope ?captured-id)
  (exec [*writer* &/get-writer
         :let [_ (doto *writer*
                   (.visitVarInsn Opcodes/ALOAD 0)
                   (.visitFieldInsn Opcodes/GETFIELD
                                    (&host/location ?scope)
                                    (str &&/closure-prefix ?captured-id)
                                    "Ljava/lang/Object;"))]]
    (return nil)))

(defn compile-global [compile *type* ?owner-class ?name]
  (exec [*writer* &/get-writer
         :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC (&host/->class (&host/location (list ?owner-class ?name))) "_datum" "Ljava/lang/Object;")]]
    (return nil)))

(defn compile-call [compile *type* ?fn ?args]
  (exec [*writer* &/get-writer
         _ (compile ?fn)
         _ (&/map% (fn [arg]
                     (exec [ret (compile arg)
                            :let [_ (.visitMethodInsn *writer* Opcodes/INVOKEINTERFACE (&host/->class &host/function-class) "apply" &&/apply-signature)]]
                       (return ret)))
                   ?args)]
    (return nil)))

(defn compile-get [compile *type* ?slot ?record]
  (exec [*writer* &/get-writer
         _ (compile ?record)
         :let [$then (new Label)
               $test-else (new Label)
               $end (new Label)
               $start (new Label)
               _ (doto *writer* ;; record
                   (.visitInsn Opcodes/DUP) ;; record, record
                   (.visitInsn Opcodes/ARRAYLENGTH) ;; record, length
                   (.visitInsn Opcodes/ICONST_2) ;; record, length, 2
                   (.visitInsn Opcodes/ISUB) ;; record, length--

                   (.visitLabel $start)
                   (.visitInsn Opcodes/DUP) ;; record, length, length
                   (.visitLdcInsn (int -2)) ;; record, length, length, -2
                   (.visitJumpInsn Opcodes/IF_ICMPEQ $then) ;; record, length
                   ;;;
                   (.visitInsn Opcodes/DUP2) ;; record, length, record, length
                   (.visitInsn Opcodes/AALOAD) ;; record, length, aslot
                   (.visitLdcInsn ?slot) ;; record, length, aslot, eslot
                   (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host/->class "java.lang.Object") "equals" (str "(" (&host/->type-signature "java.lang.Object") ")Z")) ;; record, length, Z
                   (.visitJumpInsn Opcodes/IFEQ $test-else) ;; record, length
                   (.visitInsn Opcodes/ICONST_1) ;; record, length, 1
                   (.visitInsn Opcodes/IADD) ;; record, length+
                   (.visitInsn Opcodes/AALOAD) ;; value
                   (.visitJumpInsn Opcodes/GOTO $end)
                   (.visitLabel $test-else)
                   (.visitInsn Opcodes/ICONST_2) ;; record, length, 2
                   (.visitInsn Opcodes/ISUB) ;; record, length--
                   (.visitJumpInsn Opcodes/GOTO $start)
                   ;;;
                   (.visitLabel $then)
                   (.visitInsn Opcodes/POP) ;; record
                   (.visitInsn Opcodes/POP) ;;
                   (.visitInsn Opcodes/ACONST_NULL) ;; null
                   (.visitLabel $end))]]
    (return nil)))

(let [o-sig (&host/->type-signature "java.lang.Object")]
  (defn compile-set [compile *type* ?slot ?value ?record]
    (exec [*writer* &/get-writer
           _ (compile ?record)
           :let [$then (new Label)
                 $test-else (new Label)
                 $end (new Label)
                 $start (new Label)
                 _ (doto *writer* ;; record1
                     ;;;
                     (.visitInsn Opcodes/DUP) ;; record1, record1
                     (.visitInsn Opcodes/ARRAYLENGTH) ;; record1, length1
                     (.visitTypeInsn Opcodes/ANEWARRAY (&host/->class "java.lang.Object")) ;; record1, record2
                     (.visitInsn Opcodes/DUP_X1) ;; record2, record1, record2
                     (.visitInsn Opcodes/ICONST_0) ;; record2, record1, record2, 0
                     (.visitInsn Opcodes/SWAP) ;; record2, record1, 0, record2
                     (.visitInsn Opcodes/DUP) ;; record2, record1, 0, record2, record2
                     (.visitInsn Opcodes/ARRAYLENGTH) ;; record2, record1, 0, record2, length2
                     (.visitInsn Opcodes/ICONST_0) ;; record2, record1, 0, record2, length2, 0
                     (.visitInsn Opcodes/SWAP) ;; record2, record1, 0, record2, 0, length2
                     (.visitMethodInsn Opcodes/INVOKESTATIC (&host/->class "java.lang.System") "arraycopy" (str "(" o-sig "I" o-sig "I" "I" ")V")) ;; record2
                     ;;;
                     (.visitInsn Opcodes/DUP) ;; record, record
                     (.visitInsn Opcodes/ARRAYLENGTH) ;; record, length
                     (.visitInsn Opcodes/ICONST_2) ;; record, length, 2
                     (.visitInsn Opcodes/ISUB) ;; record, length--

                     (.visitLabel $start)
                     (.visitInsn Opcodes/DUP) ;; record, length, length
                     (.visitLdcInsn (int -2)) ;; record, length, length, -2
                     (.visitJumpInsn Opcodes/IF_ICMPEQ $then) ;; record, length
                     ;;;
                     (.visitInsn Opcodes/DUP2) ;; record, length, record, length
                     (.visitInsn Opcodes/AALOAD) ;; record, length, aslot
                     (.visitLdcInsn ?slot) ;; record, length, aslot, eslot
                     (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host/->class "java.lang.Object") "equals" (str "(" (&host/->type-signature "java.lang.Object") ")Z")) ;; record, length, Z
                     (.visitJumpInsn Opcodes/IFEQ $test-else) ;; record, length
                     (.visitInsn Opcodes/DUP2) ;; record, length, record, length
                     (.visitInsn Opcodes/ICONST_1) ;; record, length, record, length, 1
                     (.visitInsn Opcodes/IADD) ;; record, length, record, length+
                     (do (compile ?value)) ;; record, length, record, length+, value
                     (.visitInsn Opcodes/AASTORE) ;; record, length
                     (.visitInsn Opcodes/POP) ;; record
                     (.visitJumpInsn Opcodes/GOTO $end)
                     (.visitLabel $test-else)
                     (.visitInsn Opcodes/ICONST_2) ;; record, length, 2
                     (.visitInsn Opcodes/ISUB) ;; record, length--
                     (.visitJumpInsn Opcodes/GOTO $start)
                     ;;;
                     (.visitLabel $then)
                     (.visitInsn Opcodes/POP) ;; record
                     (.visitLabel $end))]]
      (return nil))))

(defn compile-def [compile ?name ?body]
  (exec [*writer* &/get-writer
         module-name &/get-module-name
         :let [outer-class (&host/->class module-name)
               datum-sig (&host/->type-signature "java.lang.Object")
               current-class (&host/location (&/|list outer-class ?name))
               _ (.visitInnerClass *writer* current-class outer-class nil (+ Opcodes/ACC_STATIC Opcodes/ACC_SYNTHETIC))
               =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                        (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
                                current-class nil "java/lang/Object" (into-array [(&host/->class &host/function-class)]))
                        (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) "_datum" datum-sig nil nil)
                            (doto (.visitEnd))))]
         _ (&/with-writer (.visitMethod =class Opcodes/ACC_PUBLIC "<clinit>" "()V" nil nil)
             (exec [*writer* &/get-writer
                    :let [_ (.visitCode *writer*)]
                    _ (compile ?body)
                    :let [_ (doto *writer*
                              (.visitFieldInsn Opcodes/PUTSTATIC current-class "_datum" datum-sig)
                              (.visitInsn Opcodes/RETURN)
                              (.visitMaxs 0 0)
                              (.visitEnd))]]
               (return nil)))
         :let [_ (.visitEnd *writer*)]
         _ (&&/save-class! current-class (.toByteArray =class))
         :let [_ (prn 'compile-def ?name)]]
    (return nil)))
