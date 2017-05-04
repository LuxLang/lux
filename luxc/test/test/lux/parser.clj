(ns test.lux.parser
  (:use (clojure test
                 template))
  (:require (lux [base :as & :refer [|do return* return fail fail* |let |case]]
                 [reader :as &reader]
                 [parser :as &parser])
            [lux.analyser.module :as &a-module]
            :reload-all))

;; [Utils]
(def ^:private module-name "test")

(defn ^:private make-state [source-code]
  (&/set$ &/$source (&reader/from module-name source-code)
          (&/init-state nil)))

;; [Tests]
(deftest parse-white-space
  (let [input " \t"]
    (|case (&/run-state &parser/parse (make-state input))
      (&/$Right state (&/$Nil))
      (is true)

      _
      (is false "Couldn't read.")
      )))

(deftest parse-comment
  (let [input1 " YOLO"
        input2 "\nLOL\n"
        input3 " NYAN\n#(\nCAT )#\n"]
    (|case (&/run-state &parser/parse (make-state (str "##" input1 "\n" "#(" input2 ")#" "\n" "#(" input3 ")#")))
      (&/$Right state (&/$Nil))
      (is true)
      
      _
      (is false "Couldn't read.")
      )))

(deftest parse-bool
  (let [input1 "true"
        input2 "false"]
    (|case (&/run-state (|do [output1 &parser/parse
                              output2 &parser/parse]
                          (return (&/|++ output1 output2)))
                        (make-state (str input1 "\n" input2)))
      (&/$Right state (&/$Cons [_ (&/$Bool output1)] (&/$Cons [_ (&/$Bool output2)] (&/$Nil))))
      (are [input output] (= input output)
           true  output1
           false output2)
      
      _
      (is false "Couldn't read.")
      )))

(deftest parse-int
  (let [input1 "0"
        input2 "12"
        input3 "-123"]
    (|case (&/run-state (|do [output1 &parser/parse
                              output2 &parser/parse
                              output3 &parser/parse]
                          (return (&/|++ output1 (&/|++ output2 output3))))
                        (make-state (str input1 "\n" input2 "\n" input3)))
      (&/$Right state (&/$Cons [_ (&/$Int output1)] (&/$Cons [_ (&/$Int output2)] (&/$Cons [_ (&/$Int output3)] (&/$Nil)))))
      (are [input output] (= input output)
           0    output1
           12   output2
           -123 output3)
      
      _
      (is false "Couldn't read.")
      )))

(deftest parse-real
  (let [input1 "0.00123"
        input2 "12.01020300"
        input3 "-12.3"]
    (|case (&/run-state (|do [output1 &parser/parse
                              output2 &parser/parse
                              output3 &parser/parse]
                          (return (&/|++ output1 (&/|++ output2 output3))))
                        (make-state (str input1 "\n" input2 "\n" input3)))
      (&/$Right state (&/$Cons [_ (&/$Real output1)] (&/$Cons [_ (&/$Real output2)] (&/$Cons [_ (&/$Real output3)] (&/$Nil)))))
      (are [input output] (= input output)
           0.00123   output1
           12.010203 output2
           -12.3     output3)
      
      _
      (is false "Couldn't read.")
      )))

(deftest parse-char
  (let [input1 "a"
        input2 "\\n"
        input3 " "
        input4 "\\t"
        input5 "\\b"
        input6 "\\r"
        input7 "\\f"
        input8 "\\\""
        input9 "\\\\"]
    (|case (&/run-state (|do [output1 &parser/parse
                              output2 &parser/parse
                              output3 &parser/parse
                              output4 &parser/parse
                              output5 &parser/parse
                              output6 &parser/parse
                              output7 &parser/parse
                              output8 &parser/parse
                              output9 &parser/parse]
                          (return (&/|++ output1 (&/|++ output2 (&/|++ output3 (&/|++ output4 (&/|++ output5 (&/|++ output6 (&/|++ output7 (&/|++ output8 output9))))))))))
                        (make-state (str "#\"" input1 "\"" "\n" "#\"" input2 "\"" "\n" "#\"" input3 "\""
                                         "\n" "#\"" input4 "\"" "\n" "#\"" input5 "\"" "\n" "#\"" input6 "\""
                                         "\n" "#\"" input7 "\"" "\n" "#\"" input8 "\"" "\n" "#\"" input9 "\"")))
      (&/$Right state (&/$Cons [_ (&/$Char output1)]
                               (&/$Cons [_ (&/$Char output2)]
                                        (&/$Cons [_ (&/$Char output3)]
                                                 (&/$Cons [_ (&/$Char output4)]
                                                          (&/$Cons [_ (&/$Char output5)]
                                                                   (&/$Cons [_ (&/$Char output6)]
                                                                            (&/$Cons [_ (&/$Char output7)]
                                                                                     (&/$Cons [_ (&/$Char output8)]
                                                                                              (&/$Cons [_ (&/$Char output9)]
                                                                                                       (&/$Nil)))))))))))
      (are [input output] (= input output)
           \a         output1
           \newline   output2
           \space     output3
           \tab       output4
           \backspace output5
           \return    output6
           \formfeed  output7
           \"         output8
           \\         output9)
      
      _
      (is false "Couldn't read.")
      )))

(deftest parse-text
  (let [input1 ""
        input2 "abc"
        input3 "yolo\\nlol\\tmeme"
        input4 "This is a test\\nof multi-line text.\\n\\nI just wanna make sure it works alright..."]
    (|case (&/run-state (|do [output1 &parser/parse
                              output2 &parser/parse
                              output3 &parser/parse
                              output4 &parser/parse]
                          (return (&/|++ output1 (&/|++ output2 (&/|++ output3 output4)))))
                        (make-state (str "\"" input1 "\"" "\n" "\"" input2 "\"" "\n" "\"" input3 "\"" "\n" "\"" input4 "\"")))
      (&/$Right state (&/$Cons [_ (&/$Text output1)] (&/$Cons [_ (&/$Text output2)] (&/$Cons [_ (&/$Text output3)] (&/$Cons [_ (&/$Text output4)] (&/$Nil))))))
      (are [input output] (= input output)
           input1            output1
           input2            output2
           "yolo\nlol\tmeme" output3
           "This is a test\nof multi-line text.\n\nI just wanna make sure it works alright..." output4)
      
      _
      (is false "Couldn't read.")
      )))

(deftest parse-symbol
  (let [input1 "foo"
        input2 "test;bar0123456789"
        input3 ";b1a2z3"
        input4 ";;quux"
        input5 "!_@$%^&*-+=.<>?/|\\~`':"]
    (|case (&/run-state (|do [_ (&a-module/enter-module module-name)
                              output1 &parser/parse
                              output2 &parser/parse
                              output3 &parser/parse
                              output4 &parser/parse
                              output5 &parser/parse]
                          (return (&/|++ output1 (&/|++ output2 (&/|++ output3 (&/|++ output4 output5))))))
                        (make-state (str input1 "\n" input2 "\n" input3 "\n" input4 "\n" input5 " ")))
      (&/$Right state (&/$Cons [_ (&/$Symbol output1)]
                               (&/$Cons [_ (&/$Symbol output2)]
                                        (&/$Cons [_ (&/$Symbol output3)]
                                                 (&/$Cons [_ (&/$Symbol output4)]
                                                          (&/$Cons [_ (&/$Symbol output5)]
                                                                   (&/$Nil)))))))
      (are [input output] (&/ident= input output)
           (&/T ["" "foo"])                     output1
           (&/T ["test" "bar0123456789"])       output2
           (&/T ["lux" "b1a2z3"])               output3
           (&/T ["test" "quux"])                output4
           (&/T ["" "!_@$%^&*-+=.<>?/|\\~`':"]) output5)
      
      _
      (is false "Couldn't read.")
      )))

(deftest parse-tag
  (let [input1 "foo"
        input2 "test;bar0123456789"
        input3 ";b1a2z3"
        input4 ";;quux"
        input5 "!_@$%^&*-+=.<>?/|\\~`':"]
    (|case (&/run-state (|do [_ (&a-module/enter-module module-name)
                              output1 &parser/parse
                              output2 &parser/parse
                              output3 &parser/parse
                              output4 &parser/parse
                              output5 &parser/parse]
                          (return (&/|++ output1 (&/|++ output2 (&/|++ output3 (&/|++ output4 output5))))))
                        (make-state (str "#" input1 "\n" "#" input2 "\n" "#" input3 "\n" "#" input4 "\n" "#" input5 " ")))
      (&/$Right state (&/$Cons [_ (&/$Tag output1)]
                               (&/$Cons [_ (&/$Tag output2)]
                                        (&/$Cons [_ (&/$Tag output3)]
                                                 (&/$Cons [_ (&/$Tag output4)]
                                                          (&/$Cons [_ (&/$Tag output5)]
                                                                   (&/$Nil)))))))
      (are [input output] (&/ident= input output)
           (&/T ["" "foo"])                     output1
           (&/T ["test" "bar0123456789"])       output2
           (&/T ["lux" "b1a2z3"])               output3
           (&/T ["test" "quux"])                output4
           (&/T ["" "!_@$%^&*-+=.<>?/|\\~`':"]) output5)
      
      _
      (is false "Couldn't read.")
      )))

(do-template [<name> <tag> <open> <close>]
  (deftest <name>
    (let [input1 "yolo 123 \"lol\" #meme"]
      (|case (&/run-state &parser/parse
                          (make-state (str <open> input1 <close>)))
        (&/$Right state (&/$Cons [_ (<tag> (&/$Cons [_ (&/$Symbol symv)]
                                                    (&/$Cons [_ (&/$Int intv)]
                                                             (&/$Cons [_ (&/$Text textv)]
                                                                      (&/$Cons [_ (&/$Tag tagv)]
                                                                               (&/$Nil))))))]
                                 (&/$Nil)))
        (do (is (&/ident= (&/T ["" "yolo"]) symv))
          (is (= 123 intv))
          (is (= "lol" textv))
          (is (&/ident= (&/T ["" "meme"]) tagv)))
        
        _
        (is false "Couldn't read.")
        )))

  parse-form  &/$Form  "(" ")"
  parse-tuple &/$Tuple "[" "]"
  )

(deftest parse-record
  (let [input1 "yolo 123 \"lol\" #meme"]
    (|case (&/run-state &parser/parse
                        (make-state (str "{" input1 "}")))
      (&/$Right state (&/$Cons [_ (&/$Record (&/$Cons [[_ (&/$Symbol symv)] [_ (&/$Int intv)]]
                                                      (&/$Cons [[_ (&/$Text textv)] [_ (&/$Tag tagv)]]
                                                               (&/$Nil))))]
                               (&/$Nil)))
      (do (is (&/ident= (&/T ["" "yolo"]) symv))
        (is (= 123 intv))
        (is (= "lol" textv))
        (is (&/ident= (&/T ["" "meme"]) tagv)))
      
      _
      (is false "Couldn't read.")
      )))

(comment
  (run-all-tests)
  )
