;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.reader
  (:require [clojure.string :as string]
            clojure.core.match
            clojure.core.match.array
            [lux.base :as & :refer [deftags |do return* return fail fail* |let |case]]))

;; [Tags]
(deftags
  ["No"
   "Done"
   "Yes"])

;; [Utils]
(defn ^:private with-line [body]
  (fn [state]
    (|case (&/get$ &/$source state)
      (&/$Nil)
      (fail* "[Reader Error] EOF")

      (&/$Cons [[file-name line-num column-num] line]
               more)
      (|case (body file-name line-num column-num line)
        ($No msg)
        (fail* msg)

        ($Done output)
        (return* (&/set$ &/$source more state)
                 output)

        ($Yes output line*)
        (return* (&/set$ &/$source (&/Cons$ line* more) state)
                 output))
      )))

(defn ^:private with-lines [body]
  (fn [state]
    (|case (body (&/get$ &/$source state))
      (&/$Right reader* match)
      (return* (&/set$ &/$source reader* state)
               match)

      (&/$Left msg)
      (fail* msg)
      )))

(defn ^:private re-find! [^java.util.regex.Pattern regex column ^String line]
  (let [matcher (doto (.matcher regex line)
                  (.region column (.length line))
                  (.useAnchoringBounds true))]
    (when (.find matcher)
      (.group matcher 0))))

(defn ^:private re-find3! [^java.util.regex.Pattern regex column ^String line]
  (let [matcher (doto (.matcher regex line)
                  (.region column (.length line))
                  (.useAnchoringBounds true))]
    (when (.find matcher)
      (list (.group matcher 0)
            (.group matcher 1)
            (.group matcher 2)))))

;; [Exports]
(defn read-regex [regex]
  (with-line
    (fn [file-name line-num column-num ^String line]
      (if-let [^String match (re-find! regex column-num line)]
        (let [match-length (.length match)
              column-num* (+ column-num match-length)]
          (if (= column-num* (.length line))
            (&/V $Done (&/T (&/T file-name line-num column-num) match))
            (&/V $Yes (&/T (&/T (&/T file-name line-num column-num) match)
                           (&/T (&/T file-name line-num column-num*) line)))))
        (&/V $No (str "[Reader Error] Pattern failed: " regex))))))

(defn read-regex2 [regex]
  (with-line
    (fn [file-name line-num column-num ^String line]
      (if-let [[^String match tok1 tok2] (re-find3! regex column-num line)]
        (let [match-length (.length match)
              column-num* (+ column-num match-length)]
          (if (= column-num* (.length line))
            (&/V $Done (&/T (&/T file-name line-num column-num) (&/T tok1 tok2)))
            (&/V $Yes (&/T (&/T (&/T file-name line-num column-num) (&/T tok1 tok2))
                           (&/T (&/T file-name line-num column-num*) line)))))
        (&/V $No (str "[Reader Error] Pattern failed: " regex))))))

(defn read-regex+ [regex]
  (with-lines
    (fn [reader]
      (loop [prefix ""
             reader* reader]
        (|case reader*
          (&/$Nil)
          (&/V &/$Left "[Reader Error] EOF")

          (&/$Cons [[file-name line-num column-num] ^String line]
                   reader**)
          (if-let [^String match (re-find! regex column-num line)]
            (let [match-length (.length match)
                  column-num* (+ column-num match-length)
                  prefix* (if (= 0 column-num)
                            (str prefix "\n" match)
                            (str prefix match))]
              (if (= column-num* (.length line))
                (recur prefix* reader**)
                (&/V &/$Right (&/T (&/Cons$ (&/T (&/T file-name line-num column-num*) line)
                                            reader**)
                                   (&/T (&/T file-name line-num column-num) prefix*)))))
            (&/V &/$Left (str "[Reader Error] Pattern failed: " regex))))))))

(defn read-text [^String text]
  (with-line
    (fn [file-name line-num column-num ^String line]
      (if (.startsWith line text column-num)
        (let [match-length (.length text)
              column-num* (+ column-num match-length)]
          (if (= column-num* (.length line))
            (&/V $Done (&/T (&/T file-name line-num column-num) text))
            (&/V $Yes (&/T (&/T (&/T file-name line-num column-num) text)
                           (&/T (&/T file-name line-num column-num*) line)))))
        (&/V $No (str "[Reader Error] Text failed: " text))))))

(defn from [^String name ^String source-code]
  (->> source-code
       (string/split-lines)
       (&/->list)
       (&/enumerate)
       (&/|map (fn [line+line-num]
                 (|let [[line-num line] line+line-num]
                   (&/T (&/T name (inc line-num) 0)
                        line))))))
