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
  (let [source-code (slurp "test2.lang")
        tokens (&lexer/lex source-code)
        ;; _ (prn 'tokens tokens)
        syntax (&parser/parse tokens)
        ;; _ (prn 'syntax syntax)
        class-data (&compiler/compile "test2" syntax)]
    (write-file "test2.class" class-data))

  (->> (slurp "test2.lang")
       &lexer/lex
       &parser/parse
       (&compiler/compile "test2")
       (write-file "test2.class"))

  (->> (slurp "test2.lang")
       &lexer/lex
       &parser/parse
       (&analyser/analyse "test2"))

  (let [source-code (slurp "test2.lang")
        tokens (&lexer/lex source-code)
        ;; _ (prn 'tokens tokens)
        syntax (&parser/parse tokens)
        ;; _ (prn 'syntax syntax)
        ann-syntax (&analyser/analyse "test2" syntax)
        _ (prn 'ann-syntax ann-syntax)
        class-data (&compiler/compile "test2" syntax)]
    (write-file "test2.class" class-data))

  (let [source-code (slurp "test2.lang")
        tokens (&lexer/lex source-code)
        ;; _ (prn 'tokens tokens)
        syntax (&parser/parse tokens)
        ;; _ (prn 'syntax syntax)
        ann-syntax (&analyser/analyse "test2" syntax)
        ;; _ (prn 'ann-syntax ann-syntax)
        class-data (&compiler/compile "test2" ann-syntax)]
    (write-file "test2.class" class-data))

  (let y ...
       (lambda x (* x y)))

  (let y ...
       (proxy Function1
         (apply1 [_ x] (* x y))))

  (def (foo w x y z)
    ($ * w x y z))
  =>
  (let f1 (proxy Function1 [w x y]
                 (apply1 [_ z]
                   (STATIC-METHOD w x y z)))
       (let f2 (proxy Function2 [w x]
                      (apply1 [_ y]
                        f1)
                      (apply2 [_ y z]
                        (STATIC-METHOD w x y z)))
            (proxy Function4
              (apply1 [_ w x]
                      (proxy Function3 [w]
                             (apply1 [_ x]
                               f2)
                             (apply2 [_ x y]
                               f1)
                             (apply3 [_ x y z]
                               (STATIC-METHOD w x y z))))
              (apply2 [_ w x]
                f2)
              (apply3 [_ w x y]
                f1)
              (apply4 [_ w x y z]
                (STATIC-METHOD w x y z)))))

  <OR AS...>
  (proxy Function []
         (apply [_ w]
           (proxy Function [w]
                  (apply [_ x]
                    (proxy Function [w x]
                           (apply [_ y]
                             (proxy Function [w x y]
                                    (apply [_ z]
                                      (STATIC-METHOD w x y z)))))))))
  

  ;; TODO: Define functions as classes inheriting Function.
  ;; TODO: Add tuples.
  ;; TODO: Add pattern-matching.
  ;; TODO: Add thunks.
  ;; TODO: Add Java-interop.
  ;; TODO: Do tail-call optimization.
  ;; TODO: Add macros.
  ;; TODO: Add signatures & structures OR type-classes.
  ;; TODO: Add type-level computations.
  ;; TODO: Add interpreter.
  ;; TODO: Re-implement compiler in language.
  ;; TODO: Add all the missing literal types.
  ;; TODO: Allow strings to have escape characters.
  ;; TODO: 
  
  ;; jar cvf test2.jar test2 test2.class
  ;; java -cp "test2.jar" test2
  ;; jar cvf test2.jar test2 test2.class && java -cp "test2.jar" test2
  ;; jar cvf test2.jar test2 test2.class another.class && java -cp "test2.jar" test2
  )
