(ns test.lux.reader
  (:use clojure.test)
  (:require (lux [base :as & :refer [|do return* return fail fail* |let |case]]
                 [reader :as &reader])
            :reload-all))

;; [Utils]
(def source (&reader/from "test" "lol\nmeme\nnyan cat\n\nlolcat"))
(def init-state (&/set$ &/$source source (&/init-state nil)))

;; [Tests]
(deftest test-source-code-reading
  (is (= 5 (&/|length source))))

(deftest test-text-reading
  ;; Should be capable of recognizing literal texts.
  (let [input "lo"]
    (|case (&/run-state (&reader/read-text input) init-state)
      (&/$Right state [cursor end-line? output])
      (is (= input output))

      _
      (is false "Couldn't read.")
      )))

(deftest test-regex-reading
  ;; Should be capable of matching simple, grouping regex-patterns.
  (|case (&/run-state (&reader/read-regex #"l(.)l") init-state)
    (&/$Right state [cursor end-line? output])
    (is (= "lol" output))

    _
    (is false "Couldn't read.")
    ))

(deftest test-regex+-reading
  ;; Should be capable of matching multi-line regex-patterns.
  (|case (&/run-state (&reader/read-regex+ #"(?is)^((?!cat).)*") init-state)
    (&/$Right state [cursor output])
    (is (= "\nlol\nmeme\nnyan " output))

    _
    (is false "Couldn't read.")
    ))

(comment
  (run-all-tests)
  )
