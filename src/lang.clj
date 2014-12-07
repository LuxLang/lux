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
        class-data (&compiler/compile syntax)]
    (write-file "output.class" class-data))

  (->> (slurp "test2.lang")
       &lexer/lex
       &parser/parse
       &compiler/compile
       (write-file "output.class"))
  )
