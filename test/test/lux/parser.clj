;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns test.lux.parser
  (:use (clojure test
                 template))
  (:require (lux [base :as & :refer [deftags |do return* return fail fail* |let |case]]
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
      (&/$Right state (&/$Cons [_ (&/$BoolS output1)] (&/$Cons [_ (&/$BoolS output2)] (&/$Nil))))
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
      (&/$Right state (&/$Cons [_ (&/$IntS output1)] (&/$Cons [_ (&/$IntS output2)] (&/$Cons [_ (&/$IntS output3)] (&/$Nil)))))
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
      (&/$Right state (&/$Cons [_ (&/$RealS output1)] (&/$Cons [_ (&/$RealS output2)] (&/$Cons [_ (&/$RealS output3)] (&/$Nil)))))
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
      (&/$Right state (&/$Cons [_ (&/$CharS output1)]
                               (&/$Cons [_ (&/$CharS output2)]
                                        (&/$Cons [_ (&/$CharS output3)]
                                                 (&/$Cons [_ (&/$CharS output4)]
                                                          (&/$Cons [_ (&/$CharS output5)]
                                                                   (&/$Cons [_ (&/$CharS output6)]
                                                                            (&/$Cons [_ (&/$CharS output7)]
                                                                                     (&/$Cons [_ (&/$CharS output8)]
                                                                                              (&/$Cons [_ (&/$CharS output9)]
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
        input3 "yolo\\nlol\\tmeme"]
    (|case (&/run-state (|do [output1 &parser/parse
                              output2 &parser/parse
                              output3 &parser/parse]
                          (return (&/|++ output1 (&/|++ output2 output3))))
                        (make-state (str "\"" input1 "\"" "\n" "\"" input2 "\"" "\n" "\"" input3 "\"")))
      (&/$Right state (&/$Cons [_ (&/$TextS output1)] (&/$Cons [_ (&/$TextS output2)] (&/$Cons [_ (&/$TextS output3)] (&/$Nil)))))
      (are [input output] (= input output)
           input1            output1
           input2            output2
           "yolo\nlol\tmeme" output3)
      
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
                        (make-state (str input1 "\n" input2 "\n" input3 "\n" input4 "\n" input5)))
      (&/$Right state (&/$Cons [_ (&/$SymbolS output1)]
                               (&/$Cons [_ (&/$SymbolS output2)]
                                        (&/$Cons [_ (&/$SymbolS output3)]
                                                 (&/$Cons [_ (&/$SymbolS output4)]
                                                          (&/$Cons [_ (&/$SymbolS output5)]
                                                                   (&/$Nil)))))))
      (are [input output] (&/ident= input output)
           (&/T "" "foo")                     output1
           (&/T "test" "bar0123456789")       output2
           (&/T "lux" "b1a2z3")               output3
           (&/T "test" "quux")                output4
           (&/T "" "!_@$%^&*-+=.<>?/|\\~`':") output5)
      
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
                        (make-state (str "#" input1 "\n" "#" input2 "\n" "#" input3 "\n" "#" input4 "\n" "#" input5)))
      (&/$Right state (&/$Cons [_ (&/$TagS output1)]
                               (&/$Cons [_ (&/$TagS output2)]
                                        (&/$Cons [_ (&/$TagS output3)]
                                                 (&/$Cons [_ (&/$TagS output4)]
                                                          (&/$Cons [_ (&/$TagS output5)]
                                                                   (&/$Nil)))))))
      (are [input output] (&/ident= input output)
           (&/T "" "foo")                     output1
           (&/T "test" "bar0123456789")       output2
           (&/T "lux" "b1a2z3")               output3
           (&/T "test" "quux")                output4
           (&/T "" "!_@$%^&*-+=.<>?/|\\~`':") output5)
      
      _
      (is false "Couldn't read.")
      )))

(do-template [<name> <tag> <open> <close>]
  (deftest <name>
    (let [input1 "yolo 123 \"lol\" #meme"]
      (|case (&/run-state &parser/parse
                          (make-state (str <open> input1 <close>)))
        (&/$Right state (&/$Cons [_ (<tag> (&/$Cons [_ (&/$SymbolS symv)]
                                                    (&/$Cons [_ (&/$IntS intv)]
                                                             (&/$Cons [_ (&/$TextS textv)]
                                                                      (&/$Cons [_ (&/$TagS tagv)]
                                                                               (&/$Nil))))))]
                                 (&/$Nil)))
        (do (is (&/ident= (&/T "" "yolo") symv))
          (is (= 123 intv))
          (is (= "lol" textv))
          (is (&/ident= (&/T "" "meme") tagv)))
        
        _
        (is false "Couldn't read.")
        )))

  parse-form  &/$FormS  "(" ")"
  parse-tuple &/$TupleS "[" "]"
  )

(deftest parse-record
  (let [input1 "yolo 123 \"lol\" #meme"]
    (|case (&/run-state &parser/parse
                        (make-state (str "{" input1 "}")))
      (&/$Right state (&/$Cons [_ (&/$RecordS (&/$Cons [[_ (&/$SymbolS symv)] [_ (&/$IntS intv)]]
                                                       (&/$Cons [[_ (&/$TextS textv)] [_ (&/$TagS tagv)]]
                                                                (&/$Nil))))]
                               (&/$Nil)))
      (do (is (&/ident= (&/T "" "yolo") symv))
        (is (= 123 intv))
        (is (= "lol" textv))
        (is (&/ident= (&/T "" "meme") tagv)))
      
      _
      (is false "Couldn't read.")
      )))

(run-all-tests)
