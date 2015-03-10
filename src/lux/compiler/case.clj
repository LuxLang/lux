(ns lux.compiler.case
  (:require (clojure [set :as set]
                     [template :refer [do-template]])
            [clojure.core.match :as M :refer [match matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [exec return* return fail fail*
                                     repeat-m exhaust-m try-m try-all-m map-m reduce-m
                                     apply-m
                                     normalize-ident]]
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
  (matchv ::M/objects [token]
    [["Ident" ?name]]
    [(inc register) [::Pattern $body [::StoreMatch register]]]
    
    [["Bool" ?value]]
    [register [::Pattern $body [::BoolMatch ?value]]]

    [["Int" ?value]]
    [register [::Pattern $body [::IntMatch ?value]]]

    [["Real" ?value]]
    [register [::Pattern $body [::RealMatch ?value]]]

    [["Char" ?value]]
    [register [::Pattern $body [::CharMatch ?value]]]

    [["Text" ?value]]
    [register [::Pattern $body [::TextMatch ?value]]]

    [["Tuple" ?members]]
    (let [[register* =members] (reduce (fn [[register =members] member]
                                         (let [[register* =member] (->match $body register member)]
                                           [register* (cons =member =members)]))
                                       [register (list)]
                                       (&/->seq ?members))]
      [register* [::Pattern $body [::TupleMatch (reverse =members)]]])

    [["Tag" ?tag]]
    [register [::Pattern $body [::VariantMatch ?tag [::Pattern $body [::TupleMatch (list)]]]]]

    [["Form" ["Cons" [["Tag" ?tag]
                      ["Cons" [?value
                               ["Nil" _]]]]]]]
    (let [[register* =value] (->match $body register ?value)]
      
      [register* [::Pattern $body [::VariantMatch ?tag =value]]])
    ))

(defn ^:private process-branches [base-register branches]
  (let [[_ mappings pms] (reduce (fn [[$id mappings =matches] [pattern body]]
                                   (let [[_ =match] (->match $id base-register pattern)]
                                     [(inc $id) (assoc mappings $id body) (cons =match =matches)]))
                                 [0 {} (list)]
                                 branches)]
    [mappings (reverse pms)]))

(let [+tag-sig+ (&host/->type-signature "java.lang.String")
      +oclass+ (&host/->class "java.lang.Object")
      +equals-sig+ (str "(" (&host/->type-signature "java.lang.Object") ")Z")]
  (defn ^:private compile-match [writer ?match $target $else]
    (match ?match
      [::StoreMatch ?register]
      (doto writer
        (.visitVarInsn Opcodes/ASTORE ?register)
        (.visitJumpInsn Opcodes/GOTO $target))

      [::BoolMatch ?value]
      (doto writer
        (.visitInsn Opcodes/DUP)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host/->class "java.lang.Boolean") "booleanValue" "()Z")
        (.visitLdcInsn ?value)
        (.visitJumpInsn Opcodes/IF_ICMPNE $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))

      [::IntMatch ?value]
      (doto writer
        (.visitInsn Opcodes/DUP)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host/->class "java.lang.Long") "longValue" "()J")
        (.visitLdcInsn ?value)
        (.visitInsn Opcodes/LCMP)
        (.visitJumpInsn Opcodes/IFNE $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))

      [::RealMatch ?value]
      (doto writer
        (.visitInsn Opcodes/DUP)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host/->class "java.lang.Double") "doubleValue" "()D")
        (.visitLdcInsn ?value)
        (.visitInsn Opcodes/DCMPL)
        (.visitJumpInsn Opcodes/IFNE $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))

      [::CharMatch ?value]
      (doto writer
        (.visitInsn Opcodes/DUP)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host/->class "java.lang.Character") "charValue" "()C")
        (.visitLdcInsn ?value)
        (.visitJumpInsn Opcodes/IF_ICMPNE $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))

      [::TextMatch ?value]
      (doto writer
        (.visitInsn Opcodes/DUP)
        (.visitLdcInsn ?value)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host/->class "java.lang.Object") "equals" (str "(" (&host/->type-signature "java.lang.Object") ")Z"))
        (.visitJumpInsn Opcodes/IFEQ $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))

      [::TupleMatch ?members]
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
            (->> (doseq [[idx [_ _ member]] (map vector (range (count ?members)) ?members)
                         :let [$next (new Label)
                               $sub-else (new Label)]])))
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $target))
      
      [::VariantMatch ?tag [::Pattern _ ?value]]
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
    ;; (prn 'compile-pattern-matching patterns)
    (let [entries (for [[?branch ?body] mappings
                        :let [label (new Label)]]
                    [[?branch label]
                     [label ?body]])
          mappings* (into {} (map first entries))]
      (doto writer
        (-> (doto (compile-match ?match (get mappings* ?body) $else)
              (.visitLabel $else))
            (->> (doseq [[_ ?body ?match :as pattern] patterns
                         :let [;; _ (prn 'compile-pattern-matching/pattern pattern)
                               $else (new Label)]])))
        (.visitInsn Opcodes/POP)
        (.visitTypeInsn Opcodes/NEW ex-class)
        (.visitInsn Opcodes/DUP)
        (.visitMethodInsn Opcodes/INVOKESPECIAL ex-class "<init>" "()V")
        (.visitInsn Opcodes/ATHROW))
      (map-m (fn [[?label ?body]]
               (exec [:let [_ (.visitLabel writer ?label)]
                      ret (compile ?body)
                      :let [_ (.visitJumpInsn writer Opcodes/GOTO $end)]]
                 (return ret)))
             (map second entries))
      )))

;; [Resources]
(defn compile-case [compile *type* ?variant ?base-register ?num-registers ?branches]
  (exec [*writer* &/get-writer
         :let [$end (new Label)]
         _ (compile ?variant)
         :let [[mappings patterns] (process-branches ?base-register ?branches)]
         _ (compile-pattern-matching *writer* compile mappings patterns $end)
         :let [_ (.visitLabel *writer* $end)]]
    (return nil)))
