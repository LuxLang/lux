(ns lux.compiler.case
  (:require (clojure [set :as set]
                     [template :refer [do-template]])
            [clojure.core.match :as M :refer [match matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [exec return* return fail fail* |let]]
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
(defn ^:private ->match [$body register token]
  (prn '->match token)
  (prn '->match (aget token 0))
  (matchv ::M/objects [token]
    [["Symbol" ?name]]
    (&/T (inc register) (&/V "Pattern" (&/T $body [&/V "StoreMatch" register])))
    
    [["Bool" ?value]]
    (&/T register (&/V "Pattern" (&/T $body [&/V "BoolMatch" ?value])))

    [["Int" ?value]]
    (&/T register (&/V "Pattern" (&/T $body [&/V "IntMatch" ?value])))

    [["Real" ?value]]
    (&/T register (&/V "Pattern" (&/T $body [&/V "RealMatch" ?value])))

    [["Char" ?value]]
    (&/T register (&/V "Pattern" (&/T $body [&/V "CharMatch" ?value])))

    [["Text" ?value]]
    (&/T register (&/V "Pattern" (&/T $body [&/V "TextMatch" ?value])))

    [["Tuple" ?members]]
    (|let [[register* =members] (&/fold (fn [[register =members] member]
                                          (|let [[register* =member] (->match $body register member)]
                                            (&/T register* (&/|cons =member =members))))
                                        (&/T register (&/|list))
                                        ?members)]
      (&/T register* (&/V "Pattern" (&/T $body [&/V "TupleMatch" (&/|reverse =members)]))))

    [["Tag" ?tag]]
    (&/T register (&/V "Pattern" (&/T $body [&/V "VariantMatch" (&/T ?tag [&/V "Pattern" (&/T $body [&/V "TupleMatch" (list)])])])))

    [["Form" ["Cons" [["Tag" ?tag]
                      ["Cons" [?value
                               ["Nil" _]]]]]]]
    (|let [[register* =value] (->match $body register ?value)]
      
      (&/T register* (&/V "Pattern" (&/T $body [&/V "VariantMatch" (&/T ?tag =value)]))))
    ))

(defn ^:private process-branches [base-register branches]
  (prn 'process-branches base-register branches)
  (|let [[_ mappings pms] (&/fold (fn [$id+mappings+=matches pattern+body]
                                    (|let [[$id mappings =matches] $id+mappings+=matches
                                           [pattern body] pattern+body
                                           [_ =match] (->match $id base-register pattern)]
                                      (&/T (inc $id) (&/|put $id body mappings) (&/|cons =match =matches))))
                                  (&/T 0 (&/|table) (&/|list))
                                  branches)]
    (&/T mappings (&/|reverse pms))))

(let [+tag-sig+ (&host/->type-signature "java.lang.String")
      +oclass+ (&host/->class "java.lang.Object")
      +equals-sig+ (str "(" (&host/->type-signature "java.lang.Object") ")Z")]
  (defn ^:private compile-match [writer ?match $target $else]
    (matchv ::M/objects [?match]
      [["StoreMatch" ?register]]
      (doto writer
        (.visitVarInsn Opcodes/ASTORE ?register)
        (.visitJumpInsn Opcodes/GOTO $target))

      [["BoolMatch" ?value]]
      (doto writer
        (.visitInsn Opcodes/DUP)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host/->class "java.lang.Boolean") "booleanValue" "()Z")
        (.visitLdcInsn ?value)
        (.visitJumpInsn Opcodes/IF_ICMPNE $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))

      [["IntMatch" ?value]]
      (doto writer
        (.visitInsn Opcodes/DUP)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host/->class "java.lang.Long") "longValue" "()J")
        (.visitLdcInsn ?value)
        (.visitInsn Opcodes/LCMP)
        (.visitJumpInsn Opcodes/IFNE $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))

      [["RealMatch" ?value]]
      (doto writer
        (.visitInsn Opcodes/DUP)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host/->class "java.lang.Double") "doubleValue" "()D")
        (.visitLdcInsn ?value)
        (.visitInsn Opcodes/DCMPL)
        (.visitJumpInsn Opcodes/IFNE $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))

      [["CharMatch" ?value]]
      (doto writer
        (.visitInsn Opcodes/DUP)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host/->class "java.lang.Character") "charValue" "()C")
        (.visitLdcInsn ?value)
        (.visitJumpInsn Opcodes/IF_ICMPNE $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))

      [["TextMatch" ?value]]
      (doto writer
        (.visitInsn Opcodes/DUP)
        (.visitLdcInsn ?value)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host/->class "java.lang.Object") "equals" (str "(" (&host/->type-signature "java.lang.Object") ")Z"))
        (.visitJumpInsn Opcodes/IFEQ $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))

      [["TupleMatch" ?members]]
      (doto writer
        (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
        (-> (doto (.visitInsn Opcodes/DUP)
              (.visitLdcInsn (int idx))
              (.visitInsn Opcodes/AALOAD)
              (compile-match member $next $sub-else)
              (.visitLabel $sub-else)
              (.visitInsn Opcodes/POP)
              (.visitJumpInsn Opcodes/GOTO $else)
              (.visitLabel $next))
            (->> (|let [[idx [_ _ member]] idx+member
                        $next (new Label)
                        $sub-else (new Label)])
                 (doseq [idx+member (&/->seq (&/zip2 (&/|range (&/|length ?members)) ?members))])))
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))
      
      [["VariantMatch" [?tag ["Pattern" [_ ?value]]]]]
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
        (-> (doto (compile-match ?value $value-then $value-else)
              (.visitLabel $value-then)
              (.visitInsn Opcodes/POP)
              (.visitJumpInsn Opcodes/GOTO $target)
              (.visitLabel $value-else)
              (.visitInsn Opcodes/POP)
              (.visitJumpInsn Opcodes/GOTO $else))
            (->> (let [$value-then (new Label)
                       $value-else (new Label)]))))
      )))

(let [ex-class (&host/->class "java.lang.IllegalStateException")]
  (defn ^:private compile-pattern-matching [writer compile mappings patterns $end]
    (prn 'compile-pattern-matching mappings patterns $end)
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
            (->> (|let [[_ ?body ?match] ?body+?match])
                 (doseq [?body+?match (&/->seq patterns)
                         :let [;; _ (prn 'compile-pattern-matching/pattern pattern)
                               $else (new Label)]])))
        (.visitInsn Opcodes/POP)
        (.visitTypeInsn Opcodes/NEW ex-class)
        (.visitInsn Opcodes/DUP)
        (.visitMethodInsn Opcodes/INVOKESPECIAL ex-class "<init>" "()V")
        (.visitInsn Opcodes/ATHROW))
      (&/map% (fn [?label+?body]
                (|let [[?label ?body] ?label+?body]
                  (exec [:let [_ (.visitLabel writer ?label)]
                         ret (compile ?body)
                         :let [_ (.visitJumpInsn writer Opcodes/GOTO $end)]]
                    (return ret))))
              (&/|map &/|second entries))
      )))

;; [Resources]
(defn compile-case [compile *type* ?variant ?base-register ?num-registers ?branches]
  (exec [*writer* &/get-writer
         :let [$end (new Label)]
         _ (compile ?variant)]
    (|let [[mappings patterns] (process-branches ?base-register ?branches)]
      (exec [_ (compile-pattern-matching *writer* compile mappings patterns $end)
             :let [_ (.visitLabel *writer* $end)]]
        (return nil)))
    ))
