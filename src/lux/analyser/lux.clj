(ns lux.analyser.lux
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :refer [match]]
            (lux [base :as & :refer [exec return fail
                                     if-m try-all-m map-m mapcat-m reduce-m
                                     assert!]]
                 [parser :as &parser]
                 [type :as &type]
                 [macro :as &macro]
                 [host :as &host])
            (lux.analyser [base :as &&]
                          [lambda :as &&lambda]
                          [case :as &&case]
                          [env :as &&env]
                          [def :as &&def])))

;; [Resources]
(defn analyse-tuple [analyse ?elems]
  (exec [=elems (mapcat-m analyse ?elems)
         =elems-types (map-m &&/expr-type =elems)
         ;; :let [_ (prn 'analyse-tuple =elems)]
         ]
    (return (list [::&&/Expression [::&&/tuple =elems] [::&type/Tuple =elems-types]]))))

(defn analyse-ident [analyse ident]
  (fn [state]
    (let [[top & stack*] (::&/local-envs state)]
      (if-let [=bound (or (get-in top [:locals  :mappings ident])
                          (get-in top [:closure :mappings ident]))]
        [::&/ok [state (list =bound)]]
        (let [no-binding? #(and (-> % :locals  :mappings (contains? ident) not)
                                (-> % :closure :mappings (contains? ident) not))
              [inner outer] (split-with no-binding? stack*)]
          (if (empty? outer)
            (if-let [global (get-in state [::&/global-env ident])]
              [::&/ok [state (list global)]]
              [::&/failure (str "[Analyser Error] Unresolved identifier: " ident)])
            (let [[=local inner*] (reduce (fn [[register new-inner] frame]
                                            (let [[register* frame*] (&&lambda/close-over (:name frame) ident register frame)]
                                              [register* (cons frame* new-inner)]))
                                          [(or (get-in (first outer) [:locals  :mappings ident])
                                               (get-in (first outer) [:closure :mappings ident]))
                                           '()]
                                          (reverse (cons top inner)))]
              [::&/ok [(assoc state ::&/local-envs (concat inner* outer)) (list =local)]])
            ))
        ))
    ))

(defn analyse-call [analyse ?fn ?args]
  (exec [=fn (&&/analyse-1 analyse ?fn)
         loader &/loader]
    (match =fn
      [::&&/Expression =fn-form =fn-type]
      (match =fn-form
        [::&&/global ?module ?name]
        (exec [macro? (&&def/macro? ?module ?name)]
          (if macro?
            (let [macro-class (&host/location (list ?module ?name))
                  [macro-expansion state*] (&macro/expand loader macro-class ?args)
                  _ (prn 'macro-expansion)
                  _ (doseq [ast macro-expansion]
                      (prn '=> ast))]
              (mapcat-m analyse macro-expansion))
            (exec [=args (mapcat-m analyse ?args)
                   :let [[needs-num =return-type] (match =fn-type
                                                    [::&type/function ?fargs ?freturn]
                                                    (let [needs-num (count ?fargs)
                                                          provides-num (count =args)]
                                                      (if (> needs-num provides-num)
                                                        [needs-num [::&type/function (drop provides-num ?fargs) ?freturn]]
                                                        [needs-num &type/+dont-care-type+])))]]
              (return (list [::&&/Expression [::&&/static-call needs-num =fn =args] =return-type])))))

        _
        (exec [=args (mapcat-m analyse ?args)]
          (return (list [::&&/Expression [::&&/call =fn =args] &type/+dont-care-type+]))))

      :else
      (fail "Can't call something without a type."))
    ))

(defn analyse-case [analyse ?variant ?branches]
  (prn 'analyse-case ?variant ?branches)
  (exec [:let [num-branches (count ?branches)]
         _ (assert! (and (> num-branches 0) (even? num-branches))
                    "Unbalanced branches in \"case'\" expression.")
         :let [branches (partition 2 ?branches)
               locals-per-branch (map (comp &&case/locals first) branches)
               max-locals (reduce max 0 (map count locals-per-branch))]
         :let [_ (prn '[branches locals-per-branch max-locals] [branches locals-per-branch max-locals])]
         base-register &&env/next-local-idx
         :let [_ (prn 'base-register base-register)]
         =variant (reduce (fn [body* _] (&&env/with-local "#" :local &type/+dont-care-type+ body*))
                          (&&/analyse-1 analyse ?variant)
                          (range max-locals))
         :let [_ (prn '=variant =variant)]
         =bodies (map-m (partial &&case/analyse-branch analyse max-locals)
                        (map vector locals-per-branch (map second branches)))
         :let [_ (prn '=bodies =bodies)]
         ;; :let [_ (prn 'analyse-case/=bodies =bodies)]
         =body-types (map-m &&/expr-type =bodies)
         =case-type (return [::&type/Any]) ;; (reduce-m &type/merge [::&type/Nothing] =body-types)
         :let [=branches (map vector (map first branches) =bodies)]]
    (return (list [::&&/Expression [::&&/case =variant base-register max-locals =branches] =case-type]))))

(defn analyse-lambda [analyse ?self ?arg ?body]
  (exec [[_ =arg =return :as =lambda-type] &type/fresh-function
         [=scope =captured =body] (&&lambda/with-lambda ?self =lambda-type
                                    ?arg =arg
                                    (&&/analyse-1 analyse ?body))
         =body-type (&&/expr-type =body)
         =lambda-type (exec [_ (&type/solve =return =body-type)]
                        (&type/clean =lambda-type))
         :let [=lambda-form (match =body
                              [::&&/Expression [::&&/lambda ?sub-scope ?sub-captured ?sub-args ?sub-body] _]
                              [::&&/lambda =scope =captured (cons ?arg ?sub-args) (&&lambda/raise-expr ?arg ?sub-body)]

                              _
                              [::&&/lambda =scope =captured (list ?arg) =body])
               _ (prn '=lambda-form =lambda-form)]]
    (return (list [::&&/Expression =lambda-form =lambda-type]))))

(defn analyse-def [analyse ?name ?value]
  ;; (prn 'analyse-def ?name ?value)
  (exec [module-name &/get-module-name]
    (if-m (&&def/defined? module-name ?name)
          (fail (str "Can't redefine " ?name))
          (exec [=value (&&/analyse-1 analyse ?value)
                 =value (match =value
                          [::&&/Expression =value-form =value-type]
                          (return (match =value-form
                                    [::&&/lambda ?old-scope ?env ?args ?body]
                                    [::&&/Expression [::&&/lambda (list module-name ?name) ?env ?args ?body] =value-type]
                                    
                                    _
                                    =value))

                          _
                          (fail ""))
                 =value-type (&&/expr-type =value)
                 _ (&&def/define module-name ?name =value-type)]
            (return (list [::&&/Statement [::&&/def ?name =value]]))))))

(defn analyse-declare-macro [?ident]
  (exec [module-name &/get-module-name
         _ (&&def/declare-macro module-name ?ident)]
    (return (list))))

(defn analyse-require [analyse ?path]
  (assert false)
  (return (list)))
