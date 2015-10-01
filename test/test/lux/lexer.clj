;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns test.lux.lexer
  (:use clojure.test)
  (:require (lux [base :as & :refer [deftags |do return* return fail fail* |let |case]]
                 [reader :as &reader]
                 [lexer :as &lexer])
            [lux.analyser.module :as &a-module]
            :reload-all))

;; [Utils]
(def ^:private module-name "test")

(defn ^:private make-state [source-code]
  (&/set$ &/$source (&reader/from module-name source-code)
          (&/init-state nil)))

;; [Tests]
(deftest lex-white-space
  (let [input " \t"]
    (|case (&/run-state &lexer/lex (make-state input))
      (&/$Right state [cursor (&lexer/$White_Space output)])
      (is (= input output))

      _
      (is false "Couldn't read.")
      )))

(deftest lex-comment
  ;; Should be capable of recognizing both single-line & multi-line comments.
  (let [input1 " YOLO"
        input2 "\nLOL\n"
        input3 " NYAN\n#(\nCAT )#\n"]
    (|case (&/run-state (|do [[_ single-line] &lexer/lex
                              [_ multi-line] &lexer/lex
                              [_ multi-line-embedded] &lexer/lex]
                          (return (&/T single-line multi-line multi-line-embedded)))
                        (make-state (str "##" input1 "\n" "#(" input2 ")#" "\n" "#(" input3 ")#")))
      (&/$Right state [(&lexer/$Comment output1)
                       (&lexer/$Comment output2)
                       (&lexer/$Comment output3)])
      (are [input output] (= input output)
           input1 output1
           input2 output2
           input3 output3)
      
      _
      (is false "Couldn't read.")
      )))

(deftest lex-bool
  (let [input1 "true"
        input2 "false"]
    (|case (&/run-state (|do [[_ output1] &lexer/lex
                              [_ output2] &lexer/lex]
                          (return (&/T output1 output2)))
                        (make-state (str input1 "\n" input2)))
      (&/$Right state [(&lexer/$Bool output1)
                       (&lexer/$Bool output2)])
      (are [input output] (= input output)
           input1 output1
           input2 output2)
      
      _
      (is false "Couldn't read.")
      )))

(deftest lex-int
  (let [input1 "0"
        input2 "12"
        input3 "-123"]
    (|case (&/run-state (|do [[_ output1] &lexer/lex
                              [_ output2] &lexer/lex
                              [_ output3] &lexer/lex]
                          (return (&/T output1 output2 output3)))
                        (make-state (str input1 "\n" input2 "\n" input3)))
      (&/$Right state [(&lexer/$Int output1)
                       (&lexer/$Int output2)
                       (&lexer/$Int output3)])
      (are [input output] (= input output)
           input1 output1
           input2 output2
           input3 output3)
      
      _
      (is false "Couldn't read.")
      )))

(deftest lex-real
  (let [input1 "0.00123"
        input2 "12.01020300"
        input3 "-12.3"]
    (|case (&/run-state (|do [[_ output1] &lexer/lex
                              [_ output2] &lexer/lex
                              [_ output3] &lexer/lex]
                          (return (&/T output1 output2 output3)))
                        (make-state (str input1 "\n" input2 "\n" input3)))
      (&/$Right state [(&lexer/$Real output1)
                       (&lexer/$Real output2)
                       (&lexer/$Real output3)])
      (are [input output] (= input output)
           input1 output1
           input2 output2
           input3 output3)
      
      _
      (is false "Couldn't read.")
      )))

(deftest lex-char
  (let [input1 "a"
        input2 "\\n"
        input3 " "
        input4 "\\t"
        input5 "\\b"
        input6 "\\r"
        input7 "\\f"
        input8 "\\\""
        input9 "\\\\"]
    (|case (&/run-state (|do [[_ output1] &lexer/lex
                              [_ output2] &lexer/lex
                              [_ output3] &lexer/lex
                              [_ output4] &lexer/lex
                              [_ output5] &lexer/lex
                              [_ output6] &lexer/lex
                              [_ output7] &lexer/lex
                              [_ output8] &lexer/lex
                              [_ output9] &lexer/lex]
                          (return (&/T output1 output2 output3 output4 output5 output6 output7 output8 output9)))
                        (make-state (str "#\"" input1 "\"" "\n" "#\"" input2 "\"" "\n" "#\"" input3 "\""
                                         "\n" "#\"" input4 "\"" "\n" "#\"" input5 "\"" "\n" "#\"" input6 "\""
                                         "\n" "#\"" input7 "\"" "\n" "#\"" input8 "\"" "\n" "#\"" input9 "\"")))
      (&/$Right state [(&lexer/$Char output1)
                       (&lexer/$Char output2)
                       (&lexer/$Char output3)
                       (&lexer/$Char output4)
                       (&lexer/$Char output5)
                       (&lexer/$Char output6)
                       (&lexer/$Char output7)
                       (&lexer/$Char output8)
                       (&lexer/$Char output9)])
      (are [input output] (= input output)
           input1 output1
           "\n"   output2
           input3 output3
           "\t"   output4
           "\b"   output5
           "\r"   output6
           "\f"   output7
           "\""   output8
           "\\"   output9)
      
      _
      (is false "Couldn't read.")
      )))

(deftest lex-text
  (let [input1 ""
        input2 "abc"
        input3 "yolo\\nlol\\tmeme"]
    (|case (&/run-state (|do [[_ output1] &lexer/lex
                              [_ output2] &lexer/lex
                              [_ output3] &lexer/lex]
                          (return (&/T output1 output2 output3)))
                        (make-state (str "\"" input1 "\"" "\n" "\"" input2 "\"" "\n" "\"" input3 "\"")))
      (&/$Right state [(&lexer/$Text output1)
                       (&lexer/$Text output2)
                       (&lexer/$Text output3)])
      (are [input output] (= input output)
           input1 output1
           input2 output2
           "yolo\nlol\tmeme" output3)
      
      _
      (is false "Couldn't read.")
      )))

(deftest lex-symbol
  (let [input1 "foo"
        input2 "test;bar0123456789"
        input3 ";b1a2z3"
        input4 ";;quux"
        input5 "!_@$%^&*-+=.<>?/|\\~`':"]
    (|case (&/run-state (|do [_ (&a-module/enter-module module-name)
                              [_ output1] &lexer/lex
                              [_ output2] &lexer/lex
                              [_ output3] &lexer/lex
                              [_ output4] &lexer/lex
                              [_ output5] &lexer/lex]
                          (return (&/T output1 output2 output3 output4 output5)))
                        (make-state (str input1 "\n" input2 "\n" input3 "\n" input4 "\n" input5)))
      (&/$Right state [(&lexer/$Symbol output1)
                       (&lexer/$Symbol output2)
                       (&lexer/$Symbol output3)
                       (&lexer/$Symbol output4)
                       (&lexer/$Symbol output5)])
      (are [input output] (&/ident= input output)
           (&/T "" "foo")                     output1
           (&/T "test" "bar0123456789")       output2
           (&/T "lux" "b1a2z3")               output3
           (&/T "test" "quux")                output4
           (&/T "" "!_@$%^&*-+=.<>?/|\\~`':") output5)
      
      _
      (is false "Couldn't read.")
      )))

(deftest lex-tag
  (let [input1 "foo"
        input2 "test;bar0123456789"
        input3 ";b1a2z3"
        input4 ";;quux"
        input5 "!_@$%^&*-+=.<>?/|\\~`':"]
    (|case (&/run-state (|do [_ (&a-module/enter-module module-name)
                              [_ output1] &lexer/lex
                              [_ output2] &lexer/lex
                              [_ output3] &lexer/lex
                              [_ output4] &lexer/lex
                              [_ output5] &lexer/lex]
                          (return (&/T output1 output2 output3 output4 output5)))
                        (make-state (str "#" input1 "\n" "#" input2 "\n" "#" input3 "\n" "#" input4 "\n" "#" input5)))
      (&/$Right state [(&lexer/$Tag output1)
                       (&lexer/$Tag output2)
                       (&lexer/$Tag output3)
                       (&lexer/$Tag output4)
                       (&lexer/$Tag output5)])
      (are [input output] (&/ident= input output)
           (&/T "" "foo")                     output1
           (&/T "test" "bar0123456789")       output2
           (&/T "lux" "b1a2z3")               output3
           (&/T "test" "quux")                output4
           (&/T "" "!_@$%^&*-+=.<>?/|\\~`':") output5)
      
      _
      (is false "Couldn't read.")
      )))

(deftest lex-delimiter
  (let [input1 "("
        input2 ")"
        input3 "["
        input4 "]"
        input5 "{"
        input6 "}"]
    (|case (&/run-state (|do [_ (&a-module/enter-module module-name)
                              [_ output1] &lexer/lex
                              [_ output2] &lexer/lex
                              [_ output3] &lexer/lex
                              [_ output4] &lexer/lex
                              [_ output5] &lexer/lex
                              [_ output6] &lexer/lex]
                          (return (&/T output1 output2 output3 output4 output5 output6)))
                        (make-state (str input1 "\n" input2 "\n" input3 "\n" input4 "\n" input5 "\n" input6)))
      (&/$Right state [(&lexer/$Open_Paren)
                       (&lexer/$Close_Paren)
                       (&lexer/$Open_Bracket)
                       (&lexer/$Close_Bracket)
                       (&lexer/$Open_Brace)
                       (&lexer/$Close_Brace)])
      (is true)
      
      _
      (is false "Couldn't read.")
      )))

;; (run-all-tests)
