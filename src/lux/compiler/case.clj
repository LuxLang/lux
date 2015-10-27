;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.compiler.case
  (:require (clojure [set :as set]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |let |case]]
                 [type :as &type]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser]
                 [host :as &host])
            [lux.analyser.case :as &a-case]
            [lux.compiler.base :as &&])
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Utils]
(let [compare-kv #(.compareTo ^String (aget ^objects %1 0) ^String (aget ^objects %2 0))]
  (defn ^:private compile-match [^MethodVisitor writer ?match $target $else]
    (|case ?match
      (&a-case/$StoreTestAC ?idx)
      (if (< ?idx 0)
        (doto writer
          (.visitInsn Opcodes/POP) ;; Basically, a No-Op
          (.visitJumpInsn Opcodes/GOTO $target))
        (doto writer
          (.visitVarInsn Opcodes/ASTORE ?idx)
          (.visitJumpInsn Opcodes/GOTO $target)))

      (&a-case/$BoolTestAC ?value)
      (doto writer
        (.visitTypeInsn Opcodes/CHECKCAST "java/lang/Boolean")
        (.visitInsn Opcodes/DUP)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/Boolean" "booleanValue" "()Z")
        (.visitLdcInsn ?value)
        (.visitJumpInsn Opcodes/IF_ICMPNE $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))

      (&a-case/$IntTestAC ?value)
      (doto writer
        (.visitTypeInsn Opcodes/CHECKCAST "java/lang/Long")
        (.visitInsn Opcodes/DUP)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/Long" "longValue" "()J")
        (.visitLdcInsn (long ?value))
        (.visitInsn Opcodes/LCMP)
        (.visitJumpInsn Opcodes/IFNE $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))

      (&a-case/$RealTestAC ?value)
      (doto writer
        (.visitTypeInsn Opcodes/CHECKCAST "java/lang/Double")
        (.visitInsn Opcodes/DUP)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/Double" "doubleValue" "()D")
        (.visitLdcInsn (double ?value))
        (.visitInsn Opcodes/DCMPL)
        (.visitJumpInsn Opcodes/IFNE $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))

      (&a-case/$CharTestAC ?value)
      (doto writer
        (.visitTypeInsn Opcodes/CHECKCAST "java/lang/Character")
        (.visitInsn Opcodes/DUP)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/Character" "charValue" "()C")
        (.visitLdcInsn ?value)
        (.visitJumpInsn Opcodes/IF_ICMPNE $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))

      (&a-case/$TextTestAC ?value)
      (doto writer
        (.visitInsn Opcodes/DUP)
        (.visitLdcInsn ?value)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/Object" "equals" "(Ljava/lang/Object;)Z")
        (.visitJumpInsn Opcodes/IFEQ $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))

      (&a-case/$TupleTestAC ?members)
      (if (&/|empty? ?members)
        (doto writer
          (.visitInsn Opcodes/POP)
          (.visitJumpInsn Opcodes/GOTO $target))
        (doto writer
          (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
          (-> (doto (.visitInsn Opcodes/DUP)
                (.visitLdcInsn (int idx))
                (.visitInsn Opcodes/AALOAD)
                (compile-match test $next $sub-else)
                (.visitLabel $sub-else)
                (.visitInsn Opcodes/POP)
                (.visitJumpInsn Opcodes/GOTO $else)
                (.visitLabel $next))
              (->> (|let [[idx test] idx+member
                          $next (new Label)
                          $sub-else (new Label)])
                   (doseq [idx+member (->> ?members &/enumerate &/->seq)])))
          (.visitInsn Opcodes/POP)
          (.visitJumpInsn Opcodes/GOTO $target)))

      (&a-case/$VariantTestAC ?tag ?count ?test)
      (doto writer
        (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
        (.visitInsn Opcodes/DUP)
        (.visitLdcInsn (int 0))
        (.visitInsn Opcodes/AALOAD)
        (.visitLdcInsn ?tag)
        (&&/wrap-long)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/Object" "equals" "(Ljava/lang/Object;)Z")
        (.visitJumpInsn Opcodes/IFEQ $else)
        (.visitInsn Opcodes/DUP)
        (.visitLdcInsn (int 1))
        (.visitInsn Opcodes/AALOAD)
        (-> (doto (compile-match ?test $value-then $value-else)
              (.visitLabel $value-then)
              (.visitInsn Opcodes/POP)
              (.visitJumpInsn Opcodes/GOTO $target)
              (.visitLabel $value-else)
              (.visitInsn Opcodes/POP)
              (.visitJumpInsn Opcodes/GOTO $else))
            (->> (let [$value-then (new Label)
                       $value-else (new Label)]))))
      )))

(defn ^:private separate-bodies [patterns]
  (|let [[_ mappings patterns*] (&/fold (fn [$id+mappings+=matches pattern+body]
                                          (|let [[$id mappings =matches] $id+mappings+=matches
                                                 [pattern body] pattern+body]
                                            (&/T (inc $id) (&/|put $id body mappings) (&/|put $id pattern =matches))))
                                        (&/T 0 (&/|table) (&/|table))
                                        patterns)]
    (&/T mappings (&/|reverse patterns*))))

(defn ^:private compile-pattern-matching [^MethodVisitor writer compile mappings patterns $end]
  (let [entries (&/|map (fn [?branch+?body]
                          (|let [[?branch ?body] ?branch+?body
                                 label (new Label)]
                            (&/T (&/T ?branch label)
                                 (&/T label ?body))))
                        mappings)
        mappings* (&/|map &/|first entries)]
    (doto writer
      (-> (doto (compile-match ?match (&/|get ?body mappings*) $else)
            (.visitLabel $else))
          (->> (|let [[?body ?match] ?body+?match])
               (doseq [?body+?match (&/->seq patterns)
                       :let [$else (new Label)]])))
      (.visitInsn Opcodes/POP)
      (.visitTypeInsn Opcodes/NEW "java/lang/IllegalStateException")
      (.visitInsn Opcodes/DUP)
      (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/IllegalStateException" "<init>" "()V")
      (.visitInsn Opcodes/ATHROW))
    (&/map% (fn [?label+?body]
              (|let [[?label ?body] ?label+?body]
                (|do [:let [_ (.visitLabel writer ?label)]
                      ret (compile ?body)
                      :let [_ (.visitJumpInsn writer Opcodes/GOTO $end)]]
                  (return ret))))
            (&/|map &/|second entries))
    ))

;; [Resources]
(defn compile-case [compile ?value ?matches]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [$end (new Label)]
        _ (compile ?value)
        _ (|let [[mappings patterns] (separate-bodies ?matches)]
            (compile-pattern-matching *writer* compile mappings patterns $end))
        :let [_ (.visitLabel *writer* $end)]]
    (return nil)))
