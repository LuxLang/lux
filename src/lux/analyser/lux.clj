(ns lux.analyser.lux
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [exec return return* fail fail*]]
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
  (exec [=elems (&/flat-map% analyse ?elems)
         =elems-types (&/map% &&/expr-type =elems)
         ;; :let [_ (prn 'analyse-tuple =elems)]
         ]
    (return (&/|list (&/V "Expression" (&/T (&/V "tuple" =elems) (&/V "Tuple" =elems-types)))))))

(defn analyse-record [analyse ?elems]
  (exec [=elems (&/map% (fn [kv]
                         (matchv ::M/objects [kv]
                           [[k v]]
                           (exec [=v (&&/analyse-1 analyse v)]
                             (return (to-array [k =v])))))
                       ?elems)
         =elems-types (&/map% (fn [kv]
                               (matchv ::M/objects [kv]
                                 [[k v]]
                                 (exec [=v (&&/expr-type v)]
                                   (return (to-array [k =v])))))
                             =elems)
         ;; :let [_ (prn 'analyse-tuple =elems)]
         ]
    (return (&/|list (&/V "Expression" (&/T (&/V "record" =elems) (&/V "Record" =elems-types)))))))

(defn analyse-ident [analyse ident]
  (exec [module-name &/get-module-name]
    (fn [state]
      (let [[top & stack*] (&/get$ "local-envs" state)]
        (if-let [=bound (or (->> top (&/get$ "locals")  (&/get$ "mappings") (&/|get ident))
                            (->> top (&/get$ "closure") (&/get$ "mappings") (&/|get ident)))]
          (return* state (&/|list =bound))
          (let [no-binding? #(and (->> % (&/get$ "locals")  (&/get$ "mappings") (&/|contains? ident) not)
                                  (->> % (&/get$ "closure") (&/get$ "mappings") (&/|contains? ident) not))
                [inner outer] (split-with no-binding? stack*)]
            (if (empty? outer)
              (if-let [global (->> state (&/get$ "global-env") (&/|get ident))]
                (return* state (&/|list global))
                (fail* (str "[Analyser Error] Unresolved identifier: " ident)))
              (let [in-stack (cons top inner)
                    scopes (rest (reductions #(cons (&/get$ "name" %2) %1) (map #(&/get$ "name" %) outer) (reverse in-stack)))
                    _ (prn 'in-stack module-name ident (map #(&/get$ "name" %) in-stack) scopes)
                    [=local inner*] (reduce (fn [[register new-inner] [frame in-scope]]
                                              (let [[register* frame*] (&&lambda/close-over (cons module-name (reverse in-scope)) ident register frame)]
                                                [register* (cons frame* new-inner)]))
                                            [(or (->> outer &/|head (&/get$ "locals")  (&/get$ "mappings") (&/|get ident))
                                                 (->> outer &/|head (&/get$ "closure") (&/get$ "mappings") (&/|get ident)))
                                             '()]
                                            (map vector (reverse in-stack) scopes)
                                            )]
                (return* (&/set$ "local-envs" (&/|concat inner* outer) state) (&/|list =local)))
              ))
          ))
      )))

(defn ^:private analyse-apply* [analyse =fn ?args]
  (exec [=args (&/flat-map% analyse ?args)
         =fn-type (&&/expr-type =fn)
         =apply+=apply-type (&/fold (fn [[=fn =fn-type] =input]
                                     (exec [=input-type (&&/expr-type =input)
                                            =output-type (&type/apply-lambda =fn-type =input-type)]
                                       (return [(&/V "apply" (&/T =fn =input)) =output-type])))
                                   [=fn =fn-type]
                                   =args)
         :let [[=apply =apply-type] (matchv ::M/objects [=apply+=apply-type]
                                      [[=apply =apply-type]]
                                      [=apply =apply-type])]]
    (return (&/|list (&/V "Expression" (&/T =apply =apply-type))))))

(defn analyse-apply [analyse =fn ?args]
  (exec [loader &/loader]
    (matchv ::M/objects [=fn]
      [["Expression" [=fn-form =fn-type]]]
      (matchv ::M/objects [=fn-form]
        [["global" [?module ?name]]]
        (exec [macro? (&&def/macro? ?module ?name)]
          (if macro?
            (let [macro-class (&host/location (list ?module ?name))]
              (exec [macro-expansion (&macro/expand loader macro-class ?args)
                     output (&/flat-map% analyse macro-expansion)]
                (return output)))
            (analyse-apply* analyse =fn ?args)))
        
        [_]
        (analyse-apply* analyse =fn ?args))

      [_]
      (fail "[Analyser Error] Can't call a statement!"))
    ))

(defn analyse-case [analyse ?value ?branches]
  ;; (prn 'analyse-case ?value ?branches)
  (exec [:let [num-branches (count ?branches)]
         _ (&/assert! (and (> num-branches 0) (even? num-branches))
                      "[Analyser Error] Unbalanced branches in \"case'\" expression.")
         :let [branches (partition 2 ?branches)
               locals-per-branch (map (comp &&case/locals first) branches)
               max-locals (reduce max 0 (map count locals-per-branch))]
         ;; :let [_ (prn '[branches locals-per-branch max-locals] [branches locals-per-branch max-locals])]
         base-register &&env/next-local-idx
         ;; :let [_ (prn 'base-register base-register)]
         =value (&&/analyse-1 analyse ?value)
         ;; :let [_ (prn '=value =value)]
         =bodies (&/map% (partial &&case/analyse-branch analyse max-locals)
                         (map vector locals-per-branch (map second branches)))
         ;; :let [_ (prn '=bodies =bodies)]
         ;; :let [_ (prn 'analyse-case/=bodies =bodies)]
         =body-types (&/map% &&/expr-type =bodies)
         :let [=case-type (&/fold &type/merge (&/|table) =body-types)]
         :let [=branches (map vector (map first branches) =bodies)]]
    (return (&/|list (&/V "Expression" (&/T (&/V "case" (&/T =value base-register max-locals =branches)) =case-type))))))

(defn analyse-lambda [analyse ?self ?arg ?body]
  (exec [=lambda-type* &type/fresh-lambda]
    (matchv ::M/objects [=lambda-type*]
      [["Lambda" [=arg =return]]]
      (exec [[=scope =captured =body] (&&lambda/with-lambda ?self =lambda-type*
                                        ?arg =arg
                                        (&&/analyse-1 analyse ?body))
             =body-type (&&/expr-type =body)
             =lambda-type (exec [_ (&type/solve =return =body-type)
                                 =lambda-type** (&type/clean =return =lambda-type*)]
                            (&type/clean =arg =lambda-type**))]
        (return (&/|list (&/V "Expression" (&/T (&/V "lambda" (&/T =scope =captured ?arg =body)) =lambda-type))))))))

(defn analyse-get [analyse ?slot ?record]
  (exec [=record (&&/analyse-1 analyse ?record)
         =record-type (&&/expr-type =record)
         =slot-type (&type/slot-type =record-type ?slot)]
    (return (&/|list (&/V "Expression" (&/T (&/V "get" (?slot =record)) =slot-type))))))

(defn analyse-set [analyse ?slot ?value ?record]
  (exec [=value (&&/analyse-1 analyse ?value)
         =record (&&/analyse-1 analyse ?record)
         =record-type (&&/expr-type =record)
         =slot-type (&type/slot-type =record-type ?slot)
         _ (&type/solve =slot-type =value)]
    (return (&/|list (&/V "Expression" (&/T (&/V "set" (&/T ?slot =value =record)) =slot-type))))))

(defn analyse-def [analyse ?name ?value]
  ;; (prn 'analyse-def ?name ?value)
  (exec [module-name &/get-module-name]
    (&/if% (&&def/defined? module-name ?name)
           (fail (str "[Analyser Error] Can't redefine " ?name))
           (exec [=value (&&/analyse-1 analyse ?value)
                  =value-type (&&/expr-type =value)
                  _ (&&def/define module-name ?name =value-type)]
             (return (&/|list (&/V "Statement" (&/V "def" (&/T ?name =value)))))))))

(defn analyse-declare-macro [?ident]
  (exec [module-name &/get-module-name
         _ (&&def/declare-macro module-name ?ident)]
    (return (&/|list))))

(defn analyse-import [analyse ?path]
  (assert false)
  (return (&/|list)))

(defn analyse-check [analyse eval! ?type ?value]
  (exec [=type (&&/analyse-1 analyse ?type)
         =type-type (&&/expr-type =type)
         _ (&type/solve &type/+type+ =type-type)
         ==type (eval! =type)
         =value (&&/analyse-1 analyse ?value)]
    (matchv ::M/objects [=value]
      [["Expression" [?expr ?expr-type]]]
      (exec [_ (&type/solve ==type ?expr-type)]
        (return (&/V "Expression" (&/T ?expr ==type)))))))

(defn analyse-coerce [analyse eval! ?type ?value]
  (exec [=type (&&/analyse-1 analyse ?type)
         =type-type (&&/expr-type =type)
         _ (&type/solve &type/+type+ =type-type)
         ==type (eval! =type)
         =value (&&/analyse-1 analyse ?value)]
    (matchv ::M/objects [=value]
      [["Expression" [?expr ?expr-type]]]
      (return (&/V "Expression" (&/T ?expr ==type))))))
