;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns test.lux.reader
  (:use clojure.test)
  (:require (lux [base :as & :refer [deftags |do return* return fail fail* |let |case]]
                 [reader :as &reader])
            :reload-all))

;; [Utils]
(def source (&reader/from "test" "lol\nmeme\nnyan cat\n\nlolcat"))
(def init-state (&/set$ &/$source source (&/init-state nil)))

;; [Tests]
(deftest test-source-code-reading
  (is (= 4 (&/|length source))))

(deftest test-text-reading
  ;; Should be capable of recognizing literal texts.
  (let [input "lo"]
    (|case (&/run-state (&reader/read-text input) init-state)
      (&/$Right state [cursor output])
      (is (= input output))

      _
      (is false "Couldn't read.")
      )))

(deftest test-regex-reading
  ;; Should be capable of matching simple, grouping regex-patterns.
  (|case (&/run-state (&reader/read-regex #"l(.)l") init-state)
    (&/$Right state [cursor output])
    (is (= "lol" "lol"))

    _
    (is false "Couldn't read.")
    ))

(deftest test-regex2-reading
  ;; Should be capable of matching double, grouping regex-patterns.
  (|case (&/run-state (&reader/read-regex2 #"(.)(..)") init-state)
    (&/$Right state [cursor [left right]])
    (is (and (= "l" left)
             (= "ol" right)))

    _
    (is false "Couldn't read.")
    ))

(deftest test-regex+-reading
  ;; Should be capable of matching multi-line regex-patterns.
  (|case (&/run-state (&reader/read-regex+ #"(?is)^(.*?)(cat|$)") init-state)
    (&/$Right state [cursor output])
    (is (= "lol\nmeme\nnyan " output))

    _
    (is false "Couldn't read.")
    ))

;; (run-all-tests)
