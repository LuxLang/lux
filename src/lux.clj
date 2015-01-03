(ns lux
  (:require (lux [lexer :as &lexer]
                 [parser :as &parser]
                 [type :as &type]
                 [analyser :as &analyser]
                 [compiler :as &compiler])
            :reload))

(comment
  ;; TODO: Make macros monadic.
  ;; TODO: Finish type system.
  ;; TODO: Re-implement compiler in language.
  ;; TODO: Add signatures & structures OR type-classes.
  ;; TODO: Add type-level computations.
  ;; TODO: Add thunks.
  ;; TODO: Do tail-call optimization.
  ;; TODO: Adding metadata to global vars.
  ;; TODO: Add records.
  ;; TODO: throw, try, catch, finally
  ;; TODO: Tuple8 and Tuple8X (for arbitrary-size tuples).
  ;; TODO: Add extra arities (apply2, apply3, ..., apply16)
  ;; TODO: When doing partial application, skip "apply" and just call constructor appropiatedly.
  ;; TODO: Add "new". Allow setting fields.
  ;; TODO: Don't take into account newlines in strings unless they come from \n to allow better coding.
  ;; TODO: monitor enter & monitor exit.
  ;; TODO: 
  ;; TODO: 
  ;; TODO: 

  (let [source-code (slurp "test2.lux")
        tokens (&lexer/lex source-code)
        ;; _ (prn 'tokens tokens)
        syntax (&parser/parse tokens)
        ;; _ (prn 'syntax syntax)
        ;; ann-syntax (&analyser/analyse "test2" syntax)
        ;; _ (prn 'ann-syntax ann-syntax)
        ;; class-data (&compiler/compile "test2" ann-syntax)
        class-data (&compiler/compile "test2" syntax)
        ;; _ (prn 'class-data class-data)
        ]
    )

  ;; jar cvf test2.jar *.class test2 && java -cp "test2.jar" test2
  )

;; (def (workday? d)
;;   (case d
;;     (or [#Monday #Tuesday #Wednesday #Thursday #Friday]
;;         true)
;;     (or [#Saturday #Sunday]
;;         false)))
