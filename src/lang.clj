(ns lang
  (:require (lang [lexer :as &lexer]
                  [parser :as &parser]
                  [compiler :as &compiler])
            :reload))

(defn write-file [file data]
  (with-open [stream (java.io.BufferedOutputStream. (java.io.FileOutputStream. file))]
    (.write stream data)))

(comment
  (let [source-code (slurp "test2.lang")
        tokens (&lexer/lex source-code)
        _ (prn 'tokens tokens)
        syntax (&parser/parse tokens)
        _ (prn 'syntax syntax)
        class-data (&compiler/compile "test2" syntax)]
    (write-file "test2.class" class-data))

  (->> (slurp "test2.lang")
       &lexer/lex
       &parser/parse
       (&compiler/compile "test2")
       (write-file "test2.class"))

  ;; TODO: Define the Function abstract class.
  ;; TODO: Define functions as classes inheriting Function.
  ;; TODO: Add tuples.
  ;; TODO: Add let.
  ;; TODO: Add pattern-matching.
  ;; TODO: Add tagged values.
  ;; TODO: Do tail-call optimization.
  ;; TODO: Add macros.
  ;; TODO: Add type-level computations.
  ;; TODO: Add interpreter.
  ;; TODO: Add Java-interop.
  ;; TODO: Add signatures & structures.
  ;; TODO: Add type-system.
  ;; TODO: Allow defining constants.
  ;; TODO: 
  )
