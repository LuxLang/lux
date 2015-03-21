(ns lux.type
  (:refer-clojure :exclude [deref apply merge])
  (:require [clojure.core.match :as M :refer [match matchv]]
            clojure.core.match.array
            [lux.base :as & :refer [exec return* return fail fail* assert!]]))

;; [Util]
(def ^:private success (return nil))

(defn ^:private deref [id]
  (fn [state]
    (if-let [type* (->> state (&/get$ "lux;types") (&/get$ "lux;mappings") (&/|get id))]
      (matchv ::M/objects [type*]
        [["lux;Some" type]]
        (return* state type)
        
        [["lux;None" _]]
        (fail* (str "Unbound type-var: " id)))
      (fail* (str "Unknown type-var: " id)))))

(defn ^:private reset [id type]
  (fn [state]
    (if-let [_ (->> state (&/get$ "lux;types") (&/get$ "lux;mappings") (&/|get id))]
      (return* (&/update$ "lux;types" (fn [ts] (&/update$ "lux;mappings" #(&/|put id (&/V "lux;Some" type) %)
                                                         ts))
                          state)
               nil)
      (fail* (str "Unknown type-var: " id)))))

;; [Exports]
(def fresh-var
  (fn [state]
    (let [id (->> state (&/get$ "lux;types") (&/get$ "lux;counter"))]
      (return* (&/update$ "lux;types" #(->> %
                                            (&/update$ "lux;counter" inc)
                                            (&/update$ "lux;mappings" (fn [ms] (&/|put id (&/V "lux;None" nil) ms))))
                          state)
               (&/V "lux;TVar" id)))))

(def fresh-lambda
  (exec [=arg fresh-var
         =return fresh-var]
    (return (&/V "lux;TLambda" (&/T =arg =return)))))

(defn ^:private ->type [pseudo-type]
  (match pseudo-type
    [::Any]
    (&/V "lux;TAny" nil)

    [::Nothing]
    (&/V "lux;TNothing" nil)

    [::Data ?name ?elems]
    (&/V "lux;TData" (&/T ?name ?elems))

    [::Tuple ?members]
    (&/V "lux;TTuple" (&/|map ->type ?members))
    
    [::Variant ?members]
    (&/V "lux;TVariant" (&/|map (fn [[k v]] (&/T k (->type v)))
                                ?members))

    [::Record ?members]
    (&/V "lux;TRecord" (&/|map (fn [[k v]] (&/T k (->type v)))
                               ?members))

    [::Lambda ?input ?output]
    (&/V "lux;TLambda" (&/T (->type ?input) (->type ?output)))
    
    [::App ?lambda ?param]
    (&/V "lux;TApp" (&/T (->type ?lambda) (->type ?param)))

    [::Bound ?name]
    (&/V "lux;TBound" ?name)

    [::Var ?id]
    (&/V "lux;TVar" ?id)

    [::All ?env ?name ?arg ?body]
    (&/V "lux;TAll" (&/T (&/|map (fn [[k v]] (&/T k (->type v)))
                                 ?env)
                         ?name
                         ?arg
                         (->type ?body)))
    ))

(def +list+
  [::All (&/|list) "List" "a"
   [::Variant (&/|list ["lux;Cons" [::Tuple (&/|list [::Bound "a"] [::App [::Bound "List"] [::Bound "a"]])]]
                       ["lux;Nil" [::Tuple (&/|list)]])]])

(def +type+
  (let [text [::Data "java.lang.String" (&/|list)]
        type [::App [::Bound "Type"] [::Any]]
        list-of-types [::App +list+ type]
        string=>type [::App +list+ [::Tuple (&/|list text type)]]]
    (->type [::All (&/|list) "Type" "_"
             [::Variant (&/|list ["lux;TAny"  [::Tuple (&/|list)]]
                                 ["lux;TNothing"  [::Tuple (&/|list)]]
                                 ["lux;TData" [::Tuple (&/|list text list-of-types)]]
                                 ["lux;TTuple" list-of-types]
                                 ["lux;TVariant" string=>type]
                                 ["lux;TRecord" string=>type]
                                 ["lux;TLambda" [::Tuple (&/|list type
                                                                  type)]]
                                 ["lux;TApp" [::Tuple (&/|list type
                                                               type)]]
                                 ["lux;TBound" text]
                                 ["lux;TVar" [::Data "java.lang.Long" (&/|list)]]
                                 ["lux;TAll" [::Tuple (&/|list string=>type text text type)]]
                                 )]])))

(defn clean [tvar type]
  (matchv ::M/objects [tvar]
    [["lux;TVar" ?tid]]
    (matchv ::M/objects [type]
      [["lux;TVar" ?id]]
      (if (= ?tid ?id)
        (&/try-all% (&/|list (exec [=type (deref ?id)]
                               (clean tvar =type))
                             (return type)))
        (return type))
      
      [["lux;TLambda" [?arg ?return]]]
      (exec [=arg (clean tvar ?arg)
             =return (clean tvar ?return)]
        (return (&/V "lux;TLambda" (to-array [=arg =return]))))

      [["lux;TApp" [?lambda ?param]]]
      (exec [=lambda (clean tvar ?lambda)
             =param (clean tvar ?param)]
        (return (&/V "lux;TApp" (to-array [=lambda =param]))))

      [["lux;TTuple" ?members]]
      (exec [=members (&/map% (partial clean tvar) ?members)]
        (return (&/V "lux;TTuple" =members)))
      
      [["lux;TVariant" ?members]]
      (exec [=members (&/map% (fn [[k v]]
                                (exec [=v (clean tvar v)]
                                  (return (to-array [k =v]))))
                              ?members)]
        (return (&/V "lux;TVariant" =members)))

      [["lux;TRecord" ?members]]
      (exec [=members (&/map% (fn [[k v]]
                                (exec [=v (clean tvar v)]
                                  (return (to-array [k =v]))))
                              ?members)]
        (return (&/V "lux;TRecord" =members)))

      [["lux;TAll" [?env ?name ?arg ?body]]]
      (exec [=env (&/map% (fn [[k v]]
                            (exec [=v (clean tvar v)]
                              (return (to-array [k =v]))))
                          ?env)]
        (return (&/V "lux;TAll" (to-array [=env ?name ?arg ?body]))))

      [_]
      (return type)
      )))

(defn show-type [type]
  (prn 'show-type (aget type 0))
  (matchv ::M/objects [type]
    [["lux;TAny" _]]
    "Any"

    [["lux;TNothing" _]]
    "Nothing"

    [["lux;TData" [name params]]]
    (str "(^ " name " [" (->> params (&/|map show-type) (&/|interpose " ") (&/fold str "")) "])")

    [["lux;TTuple" elems]]
    (str "(, " (->> elems (&/|map show-type) (&/|interpose " ") (&/fold str "")) ")")

    [["lux;TVariant" cases]]
    (str "(| " (->> cases
                    (&/|map (fn [kv]
                              (matchv ::M/objects [kv]
                                [[k ["Tuple" ["Nil" _]]]]
                                (str "#" k)

                                [[k v]]
                                (str "(#" k " " (show-type v) ")"))))
                    (&/|interpose " ")
                    (&/fold str "")) ")")
    

    [["lux;TRecord" fields]]
    (str "(& " (->> fields
                    (&/|map (fn [kv]
                              (matchv ::M/objects [kv]
                                [[k v]]
                                (str "(#" k " " (show-type v) ")"))))
                    (&/|interpose " ")
                    (&/fold str "")) ")")

    [["lux;TLambda" [input output]]]
    (str "(-> " (show-type input) " " (show-type output) ")")

    [["lux;TVar" id]]
    (str "⌈" id "⌋")

    [["lux;TBound" name]]
    name

    [["lux;TApp" [?lambda ?param]]]
    (str "(" (show-type ?lambda) " " (show-type ?param) ")")
    
    [["lux;TAll" [?env ?name ?arg ?body]]]
    (str "(All " ?name " " ?arg " " (show-type ?body) ")")
    ))

(defn ^:private solve-error [expected actual]
  (str "Type " (show-type expected) " does not subsume type " (show-type actual)))

(defn solve [expected actual]
  ;; (prn 'solve expected actual)
  ;; (prn 'solve (aget expected 0) (aget actual 0))
  success
  ;; (matchv ::M/objects [expected actual]
  ;;   [["Any" _] _]
  ;;   success

  ;;   [_ ["Nothing" _]]
  ;;   success

  ;;   [["Data" [e!name e!params]] ["Data" [a!name a!params]]]
  ;;   (if (or (= e!name a!name)
  ;;           (.isAssignableFrom (Class/forName e!name) (Class/forName a!name)))
  ;;     success
  ;;     (fail (str "not (" actual " <= " expected ")")))
  
  ;;   [["Tuple" e!elems] ["Tuple" a!elems]]
  ;;   (exec [_ (assert! (= (&/|length e!elems) (&/|length a!elems))
  ;;                     "Tuples must have matching element sizes.")
  ;;          _ (&/map% (fn [n g] (solve n g))
  ;;                  (&/zip2 e!elems a!elems))]
  ;;     success)

  ;;   [["Variant" e!cases] ["Variant" a!cases]]
  ;;   (exec [_ (&/map% (fn [slot]
  ;;                    (solve (&/|get e!cases slot) (&/|get a!cases slot)))
  ;;                  (&/|keys a!cases))]
  ;;     success)

  ;;   [["Record" e!fields] ["Record" a!fields]]
  ;;   (exec [_ (&/map% (fn [slot]
  ;;                    (solve (&/|get e!fields slot) (&/|get a!fields slot)))
  ;;                  (&/|keys e!fields))]
  ;;     success)

  ;;   [["Lambda" [e!input e!output]] ["Lambda" [a!input a!output]]]
  ;;   (exec [_ (solve a!input e!input)]
  ;;     (solve e!output a!output))

  ;;   [["Var" e!id] _]
  ;;   (&/try-all% (&/|list (exec [=e!type (deref e!id)
  ;;                               _ (solve =e!type actual)
  ;;                               _ (reset e!id =e!type)]
  ;;                          success)
  ;;                        (exec [_ (reset e!id actual)]
  ;;                          success)))

  ;;   [_ ["Var" a!id]]
  ;;   (&/try-all% (&/|list (exec [=a!type (deref a!id)
  ;;                               _ (solve expected =a!type)
  ;;                               _ (reset a!id =a!type)]
  ;;                          success)
  ;;                        (exec [_ (reset a!id expected)]
  ;;                          success)))

  ;;   [_ _]
  ;;   (solve-error expected actual)
  ;;   )
  )

(let [&& #(and %1 %2)]
  (defn merge [x y]
    (matchv ::M/objects [x y]
      [_ ["lux;TAny" _]]
      (return y)

      [["lux;TAny" _] _]
      (return x)

      [_ ["lux;TNothing" _]]
      (return x)

      [["lux;TNothing" _] _]
      (return y)

      ;;;
      
      [_ _]
      (return x)

      ;; [["Variant" x!cases] ["Variant" y!cases]]
      ;; (if (and (reduce && true
      ;;                  (for [[xslot xtype] (keys x!cases)]
      ;;                    (if-let [ytype (get y!cases xslot)]
      ;;                      (= xtype ytype)
      ;;                      true)))
      ;;          (reduce && true
      ;;                  (for [[yslot ytype] (keys y!cases)]
      ;;                    (if-let [xtype (get x!cases yslot)]
      ;;                      (= xtype ytype)
      ;;                      true))))
      ;;   (return (&/V "Variant" (clojure.core/merge x!cases y!cases)))
      ;;   (fail (str "Incompatible variants: " (pr-str x) " and " (pr-str y))))

      ;; [["Record" x!fields] ["Record" y!fields]]
      ;; (if (and (= (keys x!fields) (keys y!fields))
      ;;          (->> (keys x!fields)
      ;;               (map #(= (get x!fields %) (get y!fields %)))
      ;;               (reduce && true)))
      ;;   (return x)
      ;;   (fail (str "Incompatible records: " (pr-str x) " and " (pr-str y))))
      
      [_ _]
      (fail (str "[Type System] Can't merge types: " (pr-str x) " and " (pr-str y))))))

(defn apply-lambda [func param]
  (matchv ::M/objects [func]
    [["lux;TLambda" [input output]]]
    (exec [_ (solve input param)]
      (return output))

    [_]
    (return (&/V "lux;TAny" nil))
    ;; (fail (str "[Type System] Can't apply type " (str func) " to type " (str param)))
    ))

(defn slot-type [record slot]
  (fn [state]
    (matchv ::M/objects [(&/|get record slot)]
      [["lux;Left" msg]]
      (fail* msg)

      [["lux;Right" type]]
      (return* state type))))

(def +dont-care+ (&/V "lux;TAny" nil))
