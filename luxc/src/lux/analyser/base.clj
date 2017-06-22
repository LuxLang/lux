(ns lux.analyser.base
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [defvariant |let |do return* return |case]]
                 [type :as &type])))

;; [Tags]
(defvariant
  ("bool" 1)
  ("nat" 1)
  ("int" 1)
  ("deg" 1)
  ("real" 1)
  ("char" 1)
  ("text" 1)
  ("variant" 3)
  ("tuple" 1)
  ("apply" 2)
  ("case" 2)
  ("function" 4)
  ("ann" 2)
  ("def" 1)
  ("var" 1)
  ("captured" 1)
  ("proc" 3)
  )

;; [Exports]
(defn expr-meta [analysis]
  (|let [[meta _] analysis]
    meta))

(defn expr-type* [analysis]
  (|let [[[type _] _] analysis]
    type))

(defn expr-term [analysis]
  (|let [[[type _] term] analysis]
    term))

(defn with-type [new-type analysis]
  (|let [[[type cursor] adt] analysis]
    (&/T [(&/T [new-type cursor]) adt])))

(defn clean-analysis [$var an]
  "(-> Type Analysis (Lux Analysis))"
  (|do [=an-type (&type/clean $var (expr-type* an))]
    (return (with-type =an-type an))))

(def jvm-this "_jvm_this")

(defn cap-1 [action]
  (|do [result action]
    (|case result
      (&/$Cons x (&/$Nil))
      (return x)

      _
      (&/fail-with-loc "[Analyser Error] Macro cannot expand to more than 1 output."))))

(defn analyse-1 [analyse exo-type elem]
  (&/with-expected-type exo-type
    (cap-1 (analyse exo-type elem))))

(defn analyse-1+ [analyse ?token]
  (&type/with-var
    (fn [$var]
      (|do [=expr (analyse-1 analyse $var ?token)]
        (clean-analysis $var =expr)))))

(defn resolved-ident [ident]
  (|do [:let [[?module ?name] ident]
        module* (if (.equals "" ?module)
                  &/get-module-name
                  (return ?module))]
    (return (&/T [module* ?name]))))

(let [tag-names #{"Host" "Void" "Unit" "Sum" "Product" "Function" "Bound" "Var" "Ex" "UnivQ" "ExQ" "Apply" "Named"}]
  (defn type-tag? [module name]
    (and (= "lux" module)
         (contains? tag-names name))))

(defn |meta [type cursor analysis]
  (&/T [(&/T [type cursor]) analysis]))

(defn de-meta
  "(-> Analysis Analysis)"
  [analysis]
  (|let [[meta analysis-] analysis]
    (|case analysis-
      ($variant idx is-last? value)
      ($variant idx is-last? (de-meta value))
      
      ($tuple elems)
      ($tuple (&/|map de-meta elems))
      
      ($apply func args)
      ($apply (de-meta func)
              (&/|map de-meta args))
      
      ($case value branches)
      ($case (de-meta value)
             (&/|map (fn [branch]
                       (|let [[_pattern _body] branch]
                         (&/T [_pattern (de-meta _body)])))
                     branches))
      
      ($function _register-offset scope captured body)
      ($function _register-offset scope
                 (&/|map (fn [branch]
                           (|let [[_name _captured] branch]
                             (&/T [_name (de-meta _captured)])))
                         captured)
                 (de-meta body))

      ($ann value-expr type-expr)
      (de-meta value-expr)
      
      ($captured scope idx source)
      ($captured scope idx (de-meta source))

      ($proc proc-ident args special-args)
      ($proc proc-ident (&/|map de-meta args) special-args)
      
      _
      analysis-
      )))
