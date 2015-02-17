(ns lux.analyser.lux
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :refer [match]]
            (lux [util :as &util :refer [exec return fail
                                         try-all-m map-m mapcat-m reduce-m
                                         assert!]]
                 [parser :as &parser]
                 [type :as &type]
                 [macro :as &macro]
                 [host :as &host])
            (lux.analyser [base :as &]
                          [lambda :as &&lambda]
                          [case :as &&case]
                          [env :as &&env])))

;; [Resources]
(defn analyse-tuple [analyse ?elems]
  (exec [=elems (mapcat-m analyse ?elems)
         =elems-types (map-m &/expr-type =elems)
         ;; :let [_ (prn 'analyse-tuple =elems)]
         ]
    (return (list [::&/Expression [::tuple =elems] [::&type/Tuple =elems-types]]))))

(defn analyse-ident [analyse ident]
  (fn [state]
    (let [[top & stack*] (::local-envs state)]
      (if-let [=bound (or (get-in top [:locals  :mappings ident])
                          (get-in top [:closure :mappings ident]))]
        [::&util/ok [state (list =bound)]]
        (let [no-binding? #(and (-> % :locals  :mappings (contains? ident) not)
                                (-> % :closure :mappings (contains? ident) not))
              [inner outer] (split-with no-binding? stack*)]
          (if (empty? outer)
            (if-let [global (get-in state [::&util/global-env ident])]
              [::&util/ok [state (list global)]]
              [::&util/failure (str "[Analyser Error] Unresolved identifier: " ident)])
            (let [[=local inner*] (reduce (fn [[register new-inner] frame]
                                            (let [[register* frame*] (&&lambda/close-over (:name frame) ident register frame)]
                                              [register* (cons frame* new-inner)]))
                                          [(or (get-in (first outer) [:locals  :mappings ident])
                                               (get-in (first outer) [:closure :mappings ident]))
                                           '()]
                                          (reverse (cons top inner)))]
              [::&util/ok [(assoc state ::&util/local-envs (concat inner* outer)) (list =local)]])
            ))
        ))
    ))

(defn analyse-call [analyse ?fn ?args]
  (exec [=fn (&/analyse-1 analyse ?fn)
         loader &util/loader]
    (match =fn
      [::&/Expression =fn-form =fn-type]
      (match =fn-form
        [::global ?module ?name]
        (exec [macro? (&&env/macro? ?module ?name)]
          (if macro?
            (let [macro-class (&host/location (list ?name ?module))
                  [macro-expansion state*] (&macro/expand loader macro-class ?args)]
              (mapcat-m analyse macro-expansion))
            (exec [=args (mapcat-m analyse ?args)
                   :let [[needs-num =return-type] (match =fn-type
                                                    [::&type/function ?fargs ?freturn]
                                                    (let [needs-num (count ?fargs)
                                                          provides-num (count =args)]
                                                      (if (> needs-num provides-num)
                                                        [needs-num [::&type/function (drop provides-num ?fargs) ?freturn]]
                                                        [needs-num &type/+dont-care-type+])))]]
              (return (list [::&/Expression [::static-call needs-num =fn =args] =return-type])))))

        _
        (exec [=args (mapcat-m analyse ?args)]
          (return (list [::&/Expression [::call =fn =args] &type/+dont-care-type+]))))

      :else
      (fail "Can't call something without a type."))
    ))

(defn analyse-case [analyse ?variant ?branches]
  (exec [=variant (&/analyse-1 analyse ?variant)
         _ (assert! (and (> (count ?branches) 0) (even? (count ?branches)))
                    "Unbalanced branches in \"case'\" expression.")
         :let [branches (partition 2 ?branches)
               locals-per-branch (map &&case/locals (map first branches))
               max-locals (reduce max 0 (map count locals-per-branch))]
         base-register &&env/next-local-idx
         =bodies (map-m (partial &&case/analyse-branch &/analyse-1 max-locals)
                        (map vector locals-per-branch (map second branches)))
         =body-types (map-m &/expr-type =bodies)
         =case-type (reduce-m &type/merge [::&type/Nothing] =body-types)
         :let [=branches (map vector (map first branches) =bodies)]]
    (return (list [::&/Expression [::case =variant base-register max-locals =branches] =case-type]))))

(defn analyse-lambda [analyse ?self ?arg ?body]
  (exec [[_ =arg =return :as =function] &type/fresh-function
         [=scope =captured =body] (&&lambda/with-lambda ?self =function
                                    ?arg =arg
                                    (&/analyse-1 analyse ?body))
         =body-type (&/expr-type =body)
         =function (exec [_ (&type/solve =return =body-type)]
                     (&type/clean =function))
         :let [=lambda (match =body
                    [::&/Expression [::lambda ?sub-scope ?sub-captured ?sub-args ?sub-body] =body-type]
                    [::&/Expression [::lambda =scope =captured (cons ?arg ?sub-args) (&&lambda/raise-expr ?arg ?sub-body)] =body-type]

                    _
                    [::&/Expression [::lambda =scope =captured (list ?arg) =body] =body-type])]]
    (return (list [::&/Expression =lambda =function]))))

(defn analyse-def [analyse ?name ?value]
  ;; (prn 'analyse-def ?name ?value)
  (exec [def?? (&&env/defined? ?name)]
    (if def??
      (fail (str "Can't redefine " ?name))
      (exec [ann?? (&&env/annotated? ?name)
             $module &util/get-module-name
             =value (&/analyse-1 analyse ?value)
             =value (match =value
                      [::&/Expression =value-form =value-type]
                      (return (match =value-form
                                [::lambda ?old-scope ?env ?args ?body]
                                [::&/Expression [::lambda (list ?name $module) ?env ?args ?body] =value-type]
                                
                                _
                                =value))

                      _
                      (fail ""))
             =value-type (&/expr-type =value)
             _ (if ann??
                 (return nil)
                 (&&env/annotate ?name ::public false =value-type))
             _ (&&env/define ?name)]
        (return (list [::&/Statement [::def ?name =value]]))))))

(defn analyse-declare-macro [?ident]
  (exec [_ (&&env/annotate ?ident ::public true [::&type/Any])]
    (return (list))))

(defn analyse-require [analyse ?path]
  (assert false)
  (return (list)))
