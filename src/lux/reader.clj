(ns lux.reader
  (:require [clojure.string :as string]
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            [lux.base :as & :refer [|do return* return fail fail* |let]]))

;; [Utils]
(defn ^:private with-line [body]
  (fn [state]
    (matchv ::M/objects [(&/get$ &/$SOURCE state)]
      [["lux;None" _]]
      (fail* "[Reader Error] No source code.")

      [["lux;Some" ["lux;Nil" _]]]
      (fail* "[Reader Error] EOF")

      [["lux;Some" ["lux;Cons" [["lux;Meta" [[file-name line-num column-num] line]]
                                more]]]]
      (matchv ::M/objects [(body file-name line-num column-num line)]
        [["No" msg]]
        (fail* msg)

        [["Yes" [meta ["lux;None" _]]]]
        (return* (&/set$ &/$SOURCE (&/V "lux;Some" more) state)
                 meta)

        [["Yes" [meta ["lux;Some" line-meta]]]]
        (return* (&/set$ &/$SOURCE (&/V "lux;Some" (&/|cons line-meta more)) state)
                 meta))
      )))

;; [Exports]
(defn read-regex [regex]
  (with-line
    (fn [file-name line-num column-num line]
      (if-let [[match] (re-find regex line)]
        (let [match-length (.length match)
              line* (.substring line match-length)
              ;; _ (prn 'with-line line*)
              ]
          (&/V "Yes" (&/T (&/V "lux;Meta" (&/T (&/T file-name line-num column-num) match))
                          (if (empty? line*)
                            (&/V "lux;None" nil)
                            (&/V "lux;Some" (&/V "lux;Meta" (&/T (&/T file-name line-num (+ column-num match-length)) line*)))))))
        (&/V "No" (str "[Reader Error] Pattern failed: " regex))))))

(defn read-regex2 [regex]
  (with-line
    (fn [file-name line-num column-num line]
      (if-let [[match tok1 tok2] (re-find regex line)]
        (let [match-length (.length match)
              line* (.substring line match-length)
              ;; _ (prn 'with-line line*)
              ]
          (&/V "Yes" (&/T (&/V "lux;Meta" (&/T (&/T file-name line-num column-num) [tok1 tok2]))
                          (if (empty? line*)
                            (&/V "lux;None" nil)
                            (&/V "lux;Some" (&/V "lux;Meta" (&/T (&/T file-name line-num (+ column-num match-length)) line*)))))))
        (&/V "No" (str "[Reader Error] Pattern failed: " regex))))))

(defn read-text [text]
  (with-line
    (fn [file-name line-num column-num line]
      ;; (prn 'read-text text line)
      (if (.startsWith line text)
        (let [match-length (.length text)
              line* (.substring line match-length)
              ;; _ (prn 'with-line line*)
              ]
          (&/V "Yes" (&/T (&/V "lux;Meta" (&/T (&/T file-name line-num column-num) text))
                          (if (empty? line*)
                            (&/V "lux;None" nil)
                            (&/V "lux;Some" (&/V "lux;Meta" (&/T (&/T file-name line-num (+ column-num match-length)) line*)))))))
        (&/V "No" (str "[Reader Error] Text failed: " text))))))

(defn from [file-name]
  (let [lines (&/->list (string/split-lines (slurp file-name)))]
    (&/|map (fn [line+line-num]
              (|let [[line line-num] line+line-num]
                (&/V "lux;Meta" (&/T (&/T file-name line-num 0)
                                     line))))
            (&/|filter (fn [line+line-num]
                         (|let [[line line-num] line+line-num]
                           (not (empty? line))))
                       (&/zip2 lines
                               (&/|range (&/|length lines)))))))

(def current-line
  (fn [state]
    (matchv ::M/objects [(&/get$ &/$SOURCE state)]
      [["lux;None" _]]
      (fail* "[Reader Error] No source code.")

      [["lux;Some" ["lux;Nil" _]]]
      (fail* "[Reader Error] EOF")

      [["lux;Some" ["lux;Cons" [["lux;Meta" [_ line]]
                                more]]]]
      (return* state line)
      )))
