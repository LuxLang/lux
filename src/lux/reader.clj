;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.reader
  (:require [clojure.string :as string]
            clojure.core.match
            clojure.core.match.array
            [lux.base :as & :refer [defvariant |do return* return fail* |let |case]]))

;; [Tags]
(defvariant
  ("No" 1)
  ("Done" 1)
  ("Yes" 2))

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
        ((&/fail-with-loc msg) state)

        ($Done output)
        (return* (&/set$ &/$source more state)
                 output)

        ($Yes output line*)
        (return* (&/set$ &/$source (&/$Cons line* more) state)
                 output))
      )))

(defn ^:private with-lines [body]
  (fn [state]
    (|case (body (&/get$ &/$source state))
      (&/$Right reader* match)
      (return* (&/set$ &/$source reader* state)
               match)

      (&/$Left msg)
      ((&/fail-with-loc msg) state)
      )))

(defn ^:private re-find! [^java.util.regex.Pattern regex column ^String line]
  (let [matcher (doto (.matcher regex line)
                  (.region column (.length line))
                  (.useAnchoringBounds true))]
    (when (.find matcher)
      (.group matcher 0))))

;; [Exports]
(defn read-regex [regex]
  (with-line
    (fn [file-name line-num column-num ^String line]
      (if-let [^String match (re-find! regex column-num line)]
        (let [match-length (.length match)
              column-num* (+ column-num match-length)]
          (if (= column-num* (.length line))
            ($Done (&/T [(&/T [file-name line-num column-num]) true match]))
            ($Yes (&/T [(&/T [file-name line-num column-num]) false match])
                  (&/T [(&/T [file-name line-num column-num*]) line]))))
        ($No (str "[Reader Error] Pattern failed: " regex))))))

(defn read-regex+ [regex]
  (with-lines
    (fn [reader]
      (loop [prefix ""
             reader* reader]
        (|case reader*
          (&/$Nil)
          (&/$Left "[Reader Error] EOF")

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
                (&/$Right (&/T [(&/$Cons (&/T [(&/T [file-name line-num column-num*]) line])
                                         reader**)
                                (&/T [(&/T [file-name line-num column-num]) prefix*])]))))
            (&/$Left (str "[Reader Error] Pattern failed: " regex))))))))

(defn read-text [^String text]
  "(-> Text (Reader Text))"
  (with-line
    (fn [file-name line-num column-num ^String line]
      (if (.startsWith line text column-num)
        (let [match-length (.length text)
              column-num* (+ column-num match-length)]
          (if (= column-num* (.length line))
            ($Done (&/T [(&/T [file-name line-num column-num]) true text]))
            ($Yes (&/T [(&/T [file-name line-num column-num]) false text])
                  (&/T [(&/T [file-name line-num column-num*]) line]))))
        ($No (str "[Reader Error] Text failed: " text))))))

(defn read-text? [^String text]
  "(-> Text (Reader (Maybe Text)))"
  (with-line
    (fn [file-name line-num column-num ^String line]
      (if (.startsWith line text column-num)
        (let [match-length (.length text)
              column-num* (+ column-num match-length)]
          (if (= column-num* (.length line))
            ($Done (&/T [(&/T [file-name line-num column-num]) true (&/$Some text)]))
            ($Yes (&/T [(&/T [file-name line-num column-num]) false (&/$Some text)])
                  (&/T [(&/T [file-name line-num column-num*]) line]))))
        ($Yes (&/T [(&/T [file-name line-num column-num]) false &/$None])
              (&/T [(&/T [file-name line-num column-num]) line]))))))

(defn from [^String name ^String source-code]
  (let [lines (string/split-lines source-code)
        indexed-lines (map (fn [line line-num]
                             (&/T [(&/T [name (inc line-num) 0])
                                   line]))
                           lines
                           (range (count lines)))]
    (reduce (fn [tail head] (&/$Cons head tail))
            &/$Nil
            (reverse indexed-lines))))

(defn with-source [name content body]
  (fn [state]
    (|let [old-source (&/get$ &/$source state)]
      (|case (body (&/set$ &/$source (from name content) state))
        (&/$Left error)
        (&/$Left error)

        (&/$Right state* output)
        (&/$Right (&/T [(&/set$ &/$source old-source state*) output]))))))
