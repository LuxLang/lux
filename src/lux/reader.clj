;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns lux.reader
  (:require [clojure.string :as string]
            clojure.core.match
            clojure.core.match.array
            [lux.base :as & :refer [|do return* return fail fail* |let |case]]))

;; [Utils]
(defn ^:private with-line [body]
  (fn [state]
    (|case (&/get$ &/$SOURCE state)
      ("lux;Nil")
      (fail* "[Reader Error] EOF")

      ("lux;Cons" [[file-name line-num column-num] line]
       more)
      (|case (body file-name line-num column-num line)
        ("No" msg)
        (fail* msg)

        ("Done" output)
        (return* (&/set$ &/$SOURCE more state)
                 output)

        ("Yes" output line*)
        (return* (&/set$ &/$SOURCE (&/|cons line* more) state)
                 output))
      )))

(defn ^:private with-lines [body]
  (fn [state]
    (|case (body (&/get$ &/$SOURCE state))
      ("lux;Right" reader* match)
      (return* (&/set$ &/$SOURCE reader* state)
               match)

      ("lux;Left" msg)
      (fail* msg)
      )))

;; [Exports]
(defn ^:private re-find! [^java.util.regex.Pattern regex column ^String line]
  (let [matcher (doto (.matcher regex line)
                  (.region column (.length line))
                  (.useAnchoringBounds true))]
    (when (.find matcher)
      (.group matcher 0))))

(defn ^:private re-find1! [^java.util.regex.Pattern regex column ^String line]
  (let [matcher (doto (.matcher regex line)
                  (.region column (.length line))
                  (.useAnchoringBounds true))]
    (when (.find matcher)
      (.group matcher 1))))

(defn ^:private re-find3! [^java.util.regex.Pattern regex column ^String line]
  (let [matcher (doto (.matcher regex line)
                  (.region column (.length line))
                  (.useAnchoringBounds true))]
    (when (.find matcher)
      (list (.group matcher 0)
            (.group matcher 1)
            (.group matcher 2)))))

(defn read-regex [regex]
  (with-line
    (fn [file-name line-num column-num ^String line]
      ;; (prn 'read-regex [file-name line-num column-num regex line])
      (if-let [^String match (do ;; (prn '[regex line] [regex line])
                                 (re-find! regex column-num line))]
        (let [;; _ (prn 'match match)
              match-length (.length match)
              column-num* (+ column-num match-length)]
          (if (= column-num* (.length line))
            (&/V "Done" (&/T (&/T file-name line-num column-num) match))
            (&/V "Yes" (&/T (&/T (&/T file-name line-num column-num) match)
                            (&/T (&/T file-name line-num column-num*) line)))))
        (&/V "No" (str "[Reader Error] Pattern failed: " regex))))))

(defn read-regex2 [regex]
  (with-line
    (fn [file-name line-num column-num ^String line]
      ;; (prn 'read-regex2 [file-name line-num column-num regex line])
      (if-let [[^String match tok1 tok2] (re-find3! regex column-num line)]
        (let [match-length (.length match)
              column-num* (+ column-num match-length)]
          (if (= column-num* (.length line))
            (&/V "Done" (&/T (&/T file-name line-num column-num) (&/T tok1 tok2)))
            (&/V "Yes" (&/T (&/T (&/T file-name line-num column-num) (&/T tok1 tok2))
                            (&/T (&/T file-name line-num column-num*) line)))))
        (&/V "No" (str "[Reader Error] Pattern failed: " regex))))))

(defn read-regex+ [regex]
  (with-lines
    (fn [reader]
      (loop [prefix ""
             reader* reader]
        (|case reader*
          ("lux;Nil")
          (&/V "lux;Left" "[Reader Error] EOF")

          ("lux;Cons" [[file-name line-num column-num] ^String line]
           reader**)
          (if-let [^String match (do ;; (prn 'read-regex+ regex line)
                                     (re-find1! regex column-num line))]
            (let [match-length (.length match)
                  column-num* (+ column-num match-length)]
              (if (= column-num* (.length line))
                (recur (str prefix match "\n") reader**)
                (&/V "lux;Right" (&/T (&/|cons (&/T (&/T file-name line-num column-num*) line)
                                               reader**)
                                      (&/T (&/T file-name line-num column-num) (str prefix match))))))
            (&/V "lux;Left" (str "[Reader Error] Pattern failed: " regex))))))))

(defn read-text [^String text]
  (with-line
    (fn [file-name line-num column-num ^String line]
      ;; (prn 'read-text [file-name line-num column-num text line])
      (if (.startsWith line text column-num)
        (let [match-length (.length text)
              column-num* (+ column-num match-length)]
          (if (= column-num* (.length line))
            (&/V "Done" (&/T (&/T file-name line-num column-num) text))
            (&/V "Yes" (&/T (&/T (&/T file-name line-num column-num) text)
                            (&/T (&/T file-name line-num column-num*) line)))))
        (&/V "No" (str "[Reader Error] Text failed: " text))))))

(def ^:private ^String +source-dir+ "input/")
(defn from [^String file-name ^String file-content]
  (let [lines (&/->list (string/split-lines file-content))
        file-name (.substring file-name (.length +source-dir+))]
    (&/|map (fn [line+line-num]
              (|let [[line-num line] line+line-num]
                (&/T (&/T file-name (inc line-num) 0)
                     line)))
            (&/|filter (fn [line+line-num]
                         (|let [[line-num line] line+line-num]
                           (not= "" line)))
                       (&/enumerate lines)))))
