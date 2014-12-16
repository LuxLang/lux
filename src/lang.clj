(ns lang
  (:require (lang [lexer :as &lexer]
                  [parser :as &parser]
                  [type :as &type]
                  [analyser :as &analyser]
                  [compiler :as &compiler])
            :reload))

(defn write-file [file data]
  (with-open [stream (java.io.BufferedOutputStream. (java.io.FileOutputStream. file))]
    (.write stream data)))

(comment
  ;; TODO: Add pattern-matching.
  ;; TODO: Add "do" expressions.
  ;; TODO: Add all the missing literal types.
  ;; TODO: Allow strings to have escape characters.
  ;; TODO: Fold all closure classes into one.
  ;; TODO: When doing partial application, skip "apply" and just call constructor appropiatedly.
  ;; TODO: Add extra arities (apply2, apply3, ..., apply16)
  ;; TODO: Tuple8 and Tuple8X (for arbitrary-size tuples).
  ;; TODO: Allow "lambdas" to be closures.
  ;; TODO: Add Java-interop.
  ;; TODO: Add interpreter.
  ;; TODO: Add macros.
  ;; TODO: Re-implement compiler in language.
  ;; TODO: Add signatures & structures OR type-classes.
  ;; TODO: Add type-level computations.
  ;; TODO: Add thunks.
  ;; TODO: Do tail-call optimization.
  ;; TODO: Adding metadata to global vars.
  ;; TODO: Add records.
  ;; TODO: 
  ;; TODO: 
  ;; TODO: 
  
  (let [source-code (slurp "test2.lang")
        tokens (&lexer/lex source-code)
        ;; _ (prn 'tokens tokens)
        syntax (&parser/parse tokens)
        ;; _ (prn 'syntax syntax)
        ann-syntax (&analyser/analyse "test2" syntax)
        ;; _ (prn 'ann-syntax ann-syntax)
        class-data (&compiler/compile "test2" ann-syntax)]
    (write-file "test2.class" class-data))
  
  ;; jar cvf test2.jar *.class test2 && java -cp "test2.jar" test2

  )
