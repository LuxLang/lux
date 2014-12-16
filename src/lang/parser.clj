(ns lang.parser
  (:require [clojure.template :refer [do-template]]
            [clojure.core.match :refer [match]]
            (lang [util :as &util :refer [exec return* return fail fail*
                                          repeat-m try-m try-all-m map-m
                                          apply-m]]
                  [lexer :as &lexer]
                  [type :as &type])))

(declare parse-form)

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
(do-template [<name> <input-tag> <output-tag> <method>]
  (defparser <name>
    [<input-tag> ?value]
    (return [<output-tag> (<method> ?value)]))

  
  ^:private parse-boolean ::&lexer/boolean ::boolean Boolean/parseBoolean
  ^:private parse-int     ::&lexer/int     ::int     Integer/parseInt
  ^:private parse-float   ::&lexer/float   ::float   Float/parseFloat
  )

(defparser ^:private parse-char
    [::&lexer/char ?value]
    (return [::char (.charAt ?value 0)]))

(defn ident->string [ident]
  (match ident
    [::&lexer/ident ?ident]
    ?ident))

(defparser ^:private parse-ident
  [::&lexer/ident ?ident]
  (return [::ident ?ident]))

(defparser ^:private parse-tuple
  [::&lexer/tuple ?parts]
  (exec [=parts (map-m (fn [arg] (apply-m parse-form (list arg)))
                       ?parts)]
    (return [::tuple =parts])))

(defparser ^:private parse-record
  [::&lexer/record ?parts]
  (exec [=kvs (do (assert (even? (count ?parts)))
                (map-m #(match %
                          ([[::&lexer/tag ?label] ?value] :seq)
                          (exec [=value (apply-m parse-form (list ?value))]
                            (return [?label =value])))
                       (partition 2 ?parts)))]
    (return [::record =kvs])))

(defparser ^:private parse-lambda
  [::&lexer/list ([[::&lexer/ident "lambda"] [::&lexer/tuple ?args] ?body] :seq)]
  (exec [=body (apply-m parse-form (list ?body))]
    (return [::lambda (mapv ident->string ?args) =body])))

(defparser ^:private parse-def
  [::&lexer/list ([[::&lexer/ident "def"] ?name ?body] :seq)]
  (exec [=name (apply-m parse-form (list ?name))
         =body (apply-m parse-form (list ?body))]
    (return [::def =name =body])))

(defparser ^:private parse-defdata
  [::&lexer/list ([[::&lexer/ident "defdata"] ?type & ?cases] :seq)]
  (exec [=type (apply-m parse-form (list ?type))
         =cases (map-m (fn [arg]
                         (match arg
                           [::&lexer/list ([[::&lexer/tag ?tag] ?data] :seq)]
                           (exec [=data (apply-m parse-form (list ?data))]
                             (return [::tagged ?tag =data]))
                           ))
                       ?cases)]
    (return [::defdata =type =cases])))

(defparser ^:private parse-if
  [::&lexer/list ([[::&lexer/ident "if"] ?test ?then ?else] :seq)]
  (exec [=test (apply-m parse-form (list ?test))
         =then (apply-m parse-form (list ?then))
         =else (apply-m parse-form (list ?else))]
    (return [::if =test =then =else])))

(defparser ^:private parse-do
  [::&lexer/list ([[::&lexer/ident "do"] & ?exprs] :seq)]
  (exec [=exprs (map-m #(apply-m parse-form (list %))
                       ?exprs)]
    (return [::do =exprs])))

(defparser ^:private parse-case
  [::&lexer/list ([[::&lexer/ident "case"] ?variant & cases] :seq)]
  (exec [=variant (apply-m parse-form (list ?variant))
         =branches (do (assert (even? (count cases)))
                     (map-m (fn [[destruct expr]]
                              (exec [=destruct (apply-m parse-form (list destruct))
                                     =expr (apply-m parse-form (list expr))]
                                (return [::case-branch =destruct =expr])))
                            (partition 2 cases)))]
    (return [::case =variant =branches])))

(defparser ^:private parse-let
  [::&lexer/list ([[::&lexer/ident "let"] [::&lexer/ident ?label] ?value ?body] :seq)]
  (exec [=value (apply-m parse-form (list ?value))
         =body (apply-m parse-form (list ?body))]
    (return [::let ?label =value =body])))

(defparser ^:private parse-import
  [::&lexer/list ([[::&lexer/ident "import"] [::&lexer/ident ?class]] :seq)]
  (return [::import ?class]))

(defparser ^:private parse-require
  [::&lexer/list ([[::&lexer/ident "require"] [::&lexer/string ?file] [::&lexer/ident "as"] [::&lexer/ident ?alias]] :seq)]
  (return [::require ?file ?alias]))

(defparser ^:private parse-defclass
  [::&lexer/list ([[::&lexer/ident "defclass"] [::&lexer/ident ?name] [::&lexer/tuple ?fields]] :seq)]
  (let [fields (for [field ?fields]
                 (match field
                   [::&lexer/tuple ([[::&lexer/ident ?class] [::&lexer/ident ?field]] :seq)]
                   [?class ?field]))]
    (return [::defclass ?name fields])))

(defparser ^:private parse-definterface
  [::&lexer/list ([[::&lexer/ident "definterface"] [::&lexer/ident ?name] & ?members] :seq)]
  (let [members (for [field ?members]
                  (match field
                    [::&lexer/list ([[::&lexer/ident ":"] [::&lexer/ident ?member] [::&lexer/list ([[::&lexer/ident "->"] [::&lexer/tuple ?inputs] ?output] :seq)]] :seq)]
                    [?member [(map ident->string ?inputs) (ident->string ?output)]]))]
    (return [::definterface ?name members])))

(defparser ^:private parse-tagged
  [::&lexer/list ([[::&lexer/tag ?tag] ?data] :seq)]
  (exec [=data (apply-m parse-form (list ?data))]
    (return [::tagged ?tag =data])))

(defparser ^:private parse-get
  [::&lexer/list ([[::&lexer/ident "get@"] [::&lexer/tag ?tag] ?record] :seq)]
  (exec [=record (apply-m parse-form (list ?record))]
    (return [::get ?tag =record])))

(defparser ^:private parse-remove
  [::&lexer/list ([[::&lexer/ident "remove@"] [::&lexer/tag ?tag] ?record] :seq)]
  (exec [=record (apply-m parse-form (list ?record))]
    (return [::remove ?tag =record])))

(defparser ^:private parse-set
  [::&lexer/list ([[::&lexer/ident "set@"] [::&lexer/tag ?tag] ?value ?record] :seq)]
  (exec [=value (apply-m parse-form (list ?value))
         =record (apply-m parse-form (list ?record))]
    (return [::set ?tag =value =record])))

(defparser ^:private parse-static-access
  [::&lexer/list ([[::&lexer/ident "_.."] [::&lexer/ident ?class] [::&lexer/ident ?member]] :seq)]
  (return [::static-access ?class ?member]))

(defparser ^:private parse-dynamic-access
  [::&lexer/list ([[::&lexer/ident "_."] ?object ?call] :seq)]
  (exec [=object (apply-m parse-form (list ?object))
         =call (apply-m parse-form (list ?call))]
    (return [::dynamic-access =object =call])))

(defparser ^:private parse-ann-class
  [::&lexer/list ([[::&lexer/ident "ann-class"] [::&lexer/ident ?class] & ?decl] :seq)]
  (let [[_ class-data] (reduce (fn [[mode data] event]
                                 (match event
                                   [::&lexer/ident "methods"]
                                   [:methods data]

                                   [::&lexer/ident "fields"]
                                   [:fields data]

                                   [::&lexer/list ([[::&lexer/ident ":"] [::&lexer/ident ?field-name] [::&lexer/ident ?field-class]] :seq)]
                                   [mode (assoc-in data [mode ?field-name] [::&type/object ?field-class []])]
                                   
                                   [::&lexer/list ([[::&lexer/ident ":"] [::&lexer/ident ?method-name] [::&lexer/list ([[::&lexer/ident "->"] [::&lexer/tuple ?args*] [::&lexer/ident ?return]] :seq)]] :seq)]
                                   [mode (assoc-in data [mode ?method-name] [::&type/fn (map ident->string ?args*) ?return])]
                                   ))
                               [nil {}]
                               ?decl)]
    (return [::ann-class ?class class-data])))

(defparser ^:private parse-string
  [::&lexer/string ?string]
  (return [::string ?string]))

(defparser ^:private parse-fn-call
  [::&lexer/list ([?f & ?args] :seq)]
  (exec [=f (apply-m parse-form (list ?f))
         =args (map-m (fn [arg] (apply-m parse-form (list arg)))
                      ?args)]
    (return [::fn-call =f =args])))

(def ^:private parse-form
  (try-all-m [parse-boolean
              parse-int
              parse-float
              parse-char
              parse-string
              parse-ident
              parse-tuple
              parse-record
              parse-lambda
              parse-def
              parse-defdata
              parse-if
              parse-do
              parse-case
              parse-let
              parse-tagged
              parse-get
              parse-set
              parse-remove
              parse-static-access
              parse-dynamic-access
              parse-ann-class
              parse-defclass
              parse-definterface
              parse-import
              parse-require
              parse-fn-call]))

;; [Interface]
(defn parse [text]
  (match ((repeat-m parse-form) text)
    [::&util/ok [?state ?forms]]
    (if (empty? ?state)
      ?forms
      (assert false (str "Unconsumed input: " (pr-str ?state))))
    
    [::&util/failure ?message]
    (assert false ?message)))
