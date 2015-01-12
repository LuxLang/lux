(ns lux.parser
  (:require [clojure.template :refer [do-template]]
            [clojure.core.match :refer [match]]
            (lux [util :as &util :refer [exec return* return fail fail*
                                         repeat-m try-m try-all-m map-m
                                         apply-m]]
                 [lexer :as &lexer]
                 [type :as &type])))

(declare parse-token)

;; [Utils]
(defmacro ^:private defparser [name match return]
  `(def ~name
     (fn [[token# & left#]]
       (match token#
         ~match
         (~return left#)
         _#
         (fail* (str "Unmatched token: " token#))))))

;; [Parsers]
(let [first-char #(.charAt % 0)]
  (do-template [<name> <input-tag> <output-tag> <method>]
    (defparser <name>
      [<input-tag> ?value]
      (return [<output-tag> (<method> ?value)]))

    ^:private parse-bool  ::&lexer/bool  ::bool  Boolean/parseBoolean
    ^:private parse-int   ::&lexer/int   ::int   Integer/parseInt
    ^:private parse-real  ::&lexer/real  ::real  Float/parseFloat
    ^:private parse-char  ::&lexer/char  ::char  first-char
    ^:private parse-text  ::&lexer/text  ::text  identity
    ^:private parse-ident ::&lexer/ident ::ident identity
    ))

(defparser ^:private parse-tuple
  [::&lexer/tuple ?parts]
  (exec [=parts (map-m (fn [arg] (apply-m parse-token (list arg)))
                       ?parts)]
    (return [::tuple =parts])))

(defparser ^:private parse-record
  [::&lexer/record ?parts]
  (exec [=kvs (do (assert (even? (count ?parts)))
                (map-m #(match %
                          ([[::&lexer/tag ?label] ?value] :seq)
                          (exec [=value (apply-m parse-token (list ?value))]
                            (return [?label =value])))
                       (partition 2 ?parts)))]
    (return [::record =kvs])))

(defparser ^:private parse-tag
  [::&lexer/tag ?tag]
  (return [::tag ?tag]))

(defparser ^:private parse-form
  [::&lexer/list ?elems]
  (exec [=elems (map-m (fn [arg] (apply-m parse-token (list arg)))
                       ?elems)]
    (return [::form =elems])))

;; (defparser ^:private parse-get
;;   [::&lexer/list ([[::&lexer/ident "get@"] [::&lexer/tag ?tag] ?record] :seq)]
;;   (exec [=record (apply-m parse-token (list ?record))]
;;     (return [::get ?tag =record])))

;; (defparser ^:private parse-remove
;;   [::&lexer/list ([[::&lexer/ident "remove@"] [::&lexer/tag ?tag] ?record] :seq)]
;;   (exec [=record (apply-m parse-token (list ?record))]
;;     (return [::remove ?tag =record])))

;; (defparser ^:private parse-set
;;   [::&lexer/list ([[::&lexer/ident "set@"] [::&lexer/tag ?tag] ?value ?record] :seq)]
;;   (exec [=value (apply-m parse-token (list ?value))
;;          =record (apply-m parse-token (list ?record))]
;;     (return [::set ?tag =value =record])))

(def ^:private parse-token
  (try-all-m [parse-bool
              parse-int
              parse-real
              parse-char
              parse-text
              parse-ident
              parse-tuple
              parse-record
              parse-tag
              parse-form]))

;; [Interface]
(defn parse [text]
  (match ((repeat-m parse-token) text)
    [::&util/ok [?state ?forms]]
    (if (empty? ?state)
      ?forms
      (assert false (str "Unconsumed input: " (pr-str ?state))))
    
    [::&util/failure ?message]
    (assert false ?message)))
