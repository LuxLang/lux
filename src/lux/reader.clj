(ns lux.reader
  (:require [clojure.string :as string]
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            [lux.base :as & :refer [|do return* return fail fail* |let]]))

;; [Utils]
(defn ^:private with-line [body]
  (fn [state]
    (matchv ::M/objects [(&/get$ &/$SOURCE state)]
      [["lux;Nil" _]]
      (fail* "[Reader Error] EOF")

      [["lux;Cons" [["lux;Meta" [[file-name line-num column-num] line]]
                    more]]]
      (matchv ::M/objects [(body file-name line-num column-num line)]
        [["No" msg]]
        (fail* msg)

        [["Done" output]]
        (return* (&/set$ &/$SOURCE more state)
                 output)

        [["Yes" [output line*]]]
        (return* (&/set$ &/$SOURCE (&/|cons line* more) state)
                 output))
      )))

;; [Exports]
(defn ^:private re-find! [^java.util.regex.Pattern regex line]
  (let [matcher (.matcher regex line)]
    (when (.find matcher)
      (.group matcher 0))))

(defn ^:private re-find3! [^java.util.regex.Pattern regex line]
  (let [matcher (.matcher regex line)]
    (when (.find matcher)
      (list (.group matcher 0)
            (.group matcher 1)
            (.group matcher 2)))))

(defn read-regex [regex]
  (with-line
    (fn [file-name line-num column-num ^String line]
      (if-let [^String match (re-find! regex line)]
        (let [match-length (.length match)
              line* (.substring line match-length)]
          (if (empty? line*)
            (&/V "Done" (&/V "lux;Meta" (&/T (&/T file-name line-num column-num) match)))
            (&/V "Yes" (&/T (&/V "lux;Meta" (&/T (&/T file-name line-num column-num) match))
                            (&/V "lux;Meta" (&/T (&/T file-name line-num (+ column-num match-length)) line*))))))
        (&/V "No" (str "[Reader Error] Pattern failed: " regex))))))

(defn read-regex2 [regex]
  (with-line
    (fn [file-name line-num column-num ^String line]
      (if-let [[^String match tok1 tok2] (re-find3! regex line)]
        (let [match-length (.length match)
              line* (.substring line match-length)]
          (if (empty? line*)
            (&/V "Done" (&/V "lux;Meta" (&/T (&/T file-name line-num column-num) (&/T tok1 tok2))))
            (&/V "Yes" (&/T (&/V "lux;Meta" (&/T (&/T file-name line-num column-num) (&/T tok1 tok2)))
                            (&/V "lux;Meta" (&/T (&/T file-name line-num (+ column-num match-length)) line*))))))
        (&/V "No" (str "[Reader Error] Pattern failed: " regex))))))

(defn read-text [^String text]
  (with-line
    (fn [file-name line-num column-num ^String line]
      (if (.startsWith line text)
        (let [match-length (.length text)
              line* (.substring line match-length)]
          (if (empty? line*)
            (&/V "Done" (&/V "lux;Meta" (&/T (&/T file-name line-num column-num) text)))
            (&/V "Yes" (&/T (&/V "lux;Meta" (&/T (&/T file-name line-num column-num) text))
                            (&/V "lux;Meta" (&/T (&/T file-name line-num (+ column-num match-length)) line*))))))
        (&/V "No" (str "[Reader Error] Text failed: " text))))))

(defn from [file-name]
  (let [lines (&/->list (string/split-lines (slurp file-name)))]
    (&/|map (fn [line+line-num]
              (|let [[line-num line] line+line-num]
                (&/V "lux;Meta" (&/T (&/T file-name line-num 0)
                                     line))))
            (&/|filter (fn [line+line-num]
                         (|let [[line-num line] line+line-num]
                           (not= "" line)))
                       (&/enumerate lines)))))
