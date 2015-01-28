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
  ;; TODO: Add extra arities (apply2, apply3, ..., apply16)
  ;; TODO: Allow setting fields.
  ;; TODO: monitor enter & monitor exit.
  ;; TODO: Reinplement "if" as a macro on top of case.
  ;; TODO: Remember to optimized calling global functions.
  ;; TODO: Reader macros.
  ;; TODO: Automatic currying of functions.
  ;; TODO: 
  ;; TODO: 
  ;; TODO: 
  ;; TODO: 
  
  (time (&compiler/compile-all ["lux" ;; "test2"
                                ]))

  
  
  ;; jar cvf test2.jar *.class test2 && java -cp "test2.jar" test2
  ;; cd output && jar cvf test2.jar * && java -cp "test2.jar" test2 && cd ..
  )

;; (def (workday? d)
;;   (case d
;;     (or [#Monday #Tuesday #Wednesday #Thursday #Friday]
;;         true)
;;     (or [#Saturday #Sunday]
;;         false)))
