(ns lux.compiler.case
  (:require (clojure [set :as set]
                     [template :refer [do-template]])
            [clojure.core.match :as M :refer [match matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |let]]
                 [type :as &type]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser]
                 [host :as &host])
            [lux.compiler.base :as &&])
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Utils]
(let [+tag-sig+ (&host/->type-signature "java.lang.String")
      +oclass+ (&host/->class "java.lang.Object")
      +equals-sig+ (str "(" (&host/->type-signature "java.lang.Object") ")Z")]
  (defn ^:private compile-match [writer ?match $target $else]
    (prn 'compile-match (aget ?match 0) $target $else)
    (matchv ::M/objects [?match]
      [["StoreTestAC" [?idx ?name ?value]]]
      (doto writer
        (.visitVarInsn Opcodes/ASTORE ?idx)
        (.visitJumpInsn Opcodes/GOTO $target))

      [["BoolTestAC" ?value]]
      (doto writer
        (.visitInsn Opcodes/DUP)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host/->class "java.lang.Boolean") "booleanValue" "()Z")
        (.visitLdcInsn ?value)
        (.visitJumpInsn Opcodes/IF_ICMPNE $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))

      [["IntTestAC" ?value]]
      (doto writer
        (.visitInsn Opcodes/DUP)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host/->class "java.lang.Long") "longValue" "()J")
        (.visitLdcInsn ?value)
        (.visitInsn Opcodes/LCMP)
        (.visitJumpInsn Opcodes/IFNE $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))

      [["RealTestAC" ?value]]
      (doto writer
        (.visitInsn Opcodes/DUP)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host/->class "java.lang.Double") "doubleValue" "()D")
        (.visitLdcInsn ?value)
        (.visitInsn Opcodes/DCMPL)
        (.visitJumpInsn Opcodes/IFNE $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))

      [["CharTestAC" ?value]]
      (doto writer
        (.visitInsn Opcodes/DUP)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host/->class "java.lang.Character") "charValue" "()C")
        (.visitLdcInsn ?value)
        (.visitJumpInsn Opcodes/IF_ICMPNE $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))

      [["TextTestAC" ?value]]
      (doto writer
        (.visitInsn Opcodes/DUP)
        (.visitLdcInsn ?value)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host/->class "java.lang.Object") "equals" (str "(" (&host/->type-signature "java.lang.Object") ")Z"))
        (.visitJumpInsn Opcodes/IFEQ $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))

      [["TupleTestAC" ?members]]
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
                 (doseq [idx+member (&/->seq (&/zip2 (&/|range (&/|length ?members)) ?members))])))
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))
      
      [["VariantTestAC" [?tag ?test]]]
      (doto writer
        (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
        (.visitInsn Opcodes/DUP)
        (.visitLdcInsn (int 0))
        (.visitInsn Opcodes/AALOAD)
        (.visitLdcInsn ?tag)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL +oclass+ "equals" +equals-sig+)
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

(defn ^:private separate-bodies [matches]
  (prn 'separate-bodies (aget matches 0))
  (matchv ::M/objects [matches]
    [["MatchAC" ?tests]]
    (|let [[_ mappings patterns*] (&/fold (fn [$id+mappings+=matches pattern+body]
                                            (|let [[$id mappings =matches] $id+mappings+=matches
                                                   [pattern body] pattern+body]
                                              (&/T (inc $id) (&/|put $id body mappings) (&/|put $id pattern =matches))))
                                          (&/T 0 (&/|table) (&/|table))
                                          ?tests)]
      (&/T mappings (&/|reverse patterns*)))))

(let [ex-class (&host/->class "java.lang.IllegalStateException")]
  (defn ^:private compile-pattern-matching [writer compile mappings patterns $end]
    ;; (prn 'compile-pattern-matching ?matches $end)
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
                         :let [;; _ (prn 'compile-pattern-matching/pattern pattern)
                               ;; _ (prn '?body+?match (alength ?body+?match) (aget ?body+?match 0))
                               _ (prn '?body+?match (aget ?body+?match 0))
                               $else (new Label)]])))
        (.visitInsn Opcodes/POP)
        (.visitTypeInsn Opcodes/NEW ex-class)
        (.visitInsn Opcodes/DUP)
        (.visitMethodInsn Opcodes/INVOKESPECIAL ex-class "<init>" "()V")
        (.visitInsn Opcodes/ATHROW))
      (&/map% (fn [?label+?body]
                (|let [[?label ?body] ?label+?body]
                  (|do [:let [_ (.visitLabel writer ?label)]
                        ret (compile ?body)
                        :let [_ (.visitJumpInsn writer Opcodes/GOTO $end)]]
                    (return ret))))
              (&/|map &/|second entries))
      )))

;; [Resources]
(defn compile-case [compile *type* ?value ?matches]
  ;; (prn 'compile-case ?value ?matches)
  (|do [*writer* &/get-writer
        :let [$end (new Label)]
        _ (compile ?value)
        _ (|let [[mappings patterns] (separate-bodies ?matches)]
            (compile-pattern-matching *writer* compile mappings patterns $end))
        :let [_ (.visitLabel *writer* $end)]]
    (return nil)))
