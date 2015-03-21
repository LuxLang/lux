(ns lux.type
  (:refer-clojure :exclude [deref apply merge])
  (:require [clojure.core.match :as M :refer [match matchv]]
            clojure.core.match.array
            [lux.base :as & :refer [exec return* return fail fail* assert!]]))

;; [Util]
(def ^:private success (return nil))

(defn ^:private deref [id]
  (fn [state]
    (if-let [type* (->> state (&/get$ "types") (&/get$ "mappings") (&/|get id))]
      (matchv ::M/objects [type*]
        [["Some" type]]
        (return* state type)
        
        [["None" _]]
        (fail* (str "Unbound type-var: " id)))
      (fail* (str "Unknown type-var: " id)))))

(defn ^:private reset [id type]
  (fn [state]
    (if-let [_ (->> state (&/get$ "types") (&/get$ "mappings") (&/|get id))]
      (return* (&/update$ "types" (fn [ts] (&/update$ "mappings" #(&/|put id (&/V "Some" type) %)
                                                     ts))
                          state)
               nil)
      (fail* (str "Unknown type-var: " id)))))

;; [Exports]
(def fresh-var
  (fn [state]
    (let [id (->> state (&/get$ "types") (&/get$ "counter"))]
      (return* (&/update$ "types" #(->> %
                                        (&/update$ "counter" inc)
                                        (&/update$ "mappings" (fn [ms] (&/|put id (&/V "None" nil) ms))))
                          state)
               (&/V "Var" id)))))

(def fresh-lambda
  (exec [=arg fresh-var
         =return fresh-var]
    (return (&/V "Lambda" (to-array [=arg =return])))))

(defn ^:private ->type [pseudo-type]
  (match pseudo-type
    [::Any]
    (&/V "Any" nil)

    [::Nothing]
    (&/V "Nothing" nil)

    [::Data ?name ?elems]
    (&/V "Data" (to-array [?name ?elems]))

    [::Tuple ?members]
    (&/V "Tuple" (&/|map ->type ?members))
    
    [::Variant ?members]
    (&/V "Variant" (&/|map (fn [[k v]] (to-array [k (->type v)]))
                           ?members))

    [::Record ?members]
    (&/V "Record" (&/|map (fn [[k v]] (to-array [k (->type v)]))
                          ?members))

    [::Lambda ?input ?output]
    (&/V "Lambda" (to-array [(->type ?input) (->type ?output)]))
    
    [::App ?lambda ?param]
    (&/V "App" (to-array [(->type ?lambda) (->type ?param)]))

    [::Bound ?name]
    (&/V "Bound" ?name)

    [::Var ?id]
    (&/V "Var" ?id)

    [::All ?env ?name ?arg ?body]
    (&/V "All" (to-array [(&/|map (fn [[k v]] (to-array [k (->type v)]))
                                  ?env)
                          ?name
                          ?arg
                          (->type ?body)]))
    ))

(def +list+
  [::All (&/|list) "List" "a"
   [::Variant (&/|list ["Cons" [::Tuple (&/|list [::Bound "a"] [::App [::Bound "List"] [::Bound "a"]])]]
                       ["Nil" [::Tuple (&/|list)]])]])

(def +type+
  (let [text [::Data "java.lang.String" (&/|list)]
        type [::App [::Bound "Type"] [::Any]]
        list-of-types [::App +list+ type]
        string=>type [::App +list+ [::Tuple (&/|list text type)]]]
    (->type [::All (&/|list) "Type" "_"
             [::Variant (&/|list ["Any"  [::Tuple (&/|list)]]
                                 ["Nothing"  [::Tuple (&/|list)]]
                                 ["Data" [::Tuple (&/|list text list-of-types)]]
                                 ["Tuple" list-of-types]
                                 ["Variant" string=>type]
                                 ["Record" string=>type]
                                 ["Lambda" [::Tuple (&/|list type
                                                             type)]]
                                 ["App" [::Tuple (&/|list type
                                                          type)]]
                                 ["Bound" text]
                                 ["Var" [::Data "java.lang.Long" (&/|list)]]
                                 ["All" [::Tuple (&/|list string=>type text text type)]]
                                 )]])))

(defn clean [tvar type]
  (matchv ::M/objects [tvar]
    [["Var" ?tid]]
    (matchv ::M/objects [type]
      [["Var" ?id]]
      (if (= ?tid ?id)
        (&/try-all% (&/|list (exec [=type (deref ?id)]
                               (clean tvar =type))
                             (return type)))
        (return type))
      
      [["Lambda" [?arg ?return]]]
      (exec [=arg (clean tvar ?arg)
             =return (clean tvar ?return)]
        (return (&/V "Lambda" (to-array [=arg =return]))))

      [["App" [?lambda ?param]]]
      (exec [=lambda (clean tvar ?lambda)
             =param (clean tvar ?param)]
        (return (&/V "App" (to-array [=lambda =param]))))

      [["Tuple" ?members]]
      (exec [=members (&/map% (partial clean tvar) ?members)]
        (return (&/V "Tuple" =members)))
      
      [["Variant" ?members]]
      (exec [=members (&/map% (fn [[k v]]
                                (exec [=v (clean tvar v)]
                                  (return (to-array [k =v]))))
                              ?members)]
        (return (&/V "Variant" =members)))

      [["Record" ?members]]
      (exec [=members (&/map% (fn [[k v]]
                                (exec [=v (clean tvar v)]
                                  (return (to-array [k =v]))))
                              ?members)]
        (return (&/V "Record" =members)))

      [["All" [?env ?name ?arg ?body]]]
      (exec [=env (&/map% (fn [[k v]]
                            (exec [=v (clean tvar v)]
                              (return (to-array [k =v]))))
                          ?env)]
        (return (&/V "All" (to-array [=env ?name ?arg ?body]))))

      [_]
      (return type)
      )))

(defn show-type [type]
  (prn 'show-type (aget type 0))
  (matchv ::M/objects [type]
    [["Any" _]]
    "Any"

    [["Nothing" _]]
    "Nothing"

    [["Data" [name params]]]
    (str "(^ " name " [" (->> params (&/|map show-type) (&/|interpose " ") (&/fold str "")) "])")

    [["Tuple" elems]]
    (str "(, " (->> elems (&/|map show-type) (&/|interpose " ") (&/fold str "")) ")")

    [["Variant" cases]]
    (str "(| " (->> cases
                    (&/|map (fn [kv]
                            (matchv ::M/objects [kv]
                              [[k ["Tuple" ["Nil" _]]]]
                              (str "#" k)

                              [[k v]]
                              (str "(#" k " " (show-type v) ")"))))
                    (&/|interpose " ")
                    (&/fold str "")) ")")
    

    [["Record" fields]]
    (str "(& " (->> fields
                    (&/|map (fn [kv]
                            (matchv ::M/objects [kv]
                              [[k v]]
                              (str "(#" k " " (show-type v) ")"))))
                    (&/|interpose " ")
                    (&/fold str "")) ")")

    [["Lambda" [input output]]]
    (str "(-> " (show-type input) " " (show-type output) ")")

    [["Var" id]]
    (str "⌈" id "⌋")

    [["Bound" name]]
    name

    [["App" [?lambda ?param]]]
    (str "(" (show-type ?lambda) " " (show-type ?param) ")")
    
    [["All" [?env ?name ?arg ?body]]]
    (str "(All " ?name " " ?arg " " (show-type ?body) ")")
    ))

(defn ^:private solve-error [expected actual]
  (str "Type " (show-type expected) " does not subsume type " (show-type actual)))

(defn solve [expected actual]
  ;; (prn 'solve expected actual)
  ;; (prn 'solve (aget expected 0) (aget actual 0))
  (matchv ::M/objects [expected actual]
    [["Any" _] _]
    success

    [_ ["Nothing" _]]
    success

    [["Data" [e!name e!params]] ["Data" [a!name a!params]]]
    (if (or (= e!name a!name)
            (.isAssignableFrom (Class/forName e!name) (Class/forName a!name)))
      success
      (fail (str "not (" actual " <= " expected ")")))
    
    [["Tuple" e!elems] ["Tuple" a!elems]]
    (exec [_ (assert! (= (&/|length e!elems) (&/|length a!elems))
                      "Tuples must have matching element sizes.")
           _ (&/map% (fn [n g] (solve n g))
                   (&/zip2 e!elems a!elems))]
      success)

    [["Variant" e!cases] ["Variant" a!cases]]
    (exec [_ (&/map% (fn [slot]
                     (solve (&/|get e!cases slot) (&/|get a!cases slot)))
                   (&/|keys a!cases))]
      success)

    [["Record" e!fields] ["Record" a!fields]]
    (exec [_ (&/map% (fn [slot]
                     (solve (&/|get e!fields slot) (&/|get a!fields slot)))
                   (&/|keys e!fields))]
      success)

    [["Lambda" [e!input e!output]] ["Lambda" [a!input a!output]]]
    (exec [_ (solve a!input e!input)]
      (solve e!output a!output))

    [["Var" e!id] _]
    (&/try-all% (&/|list (exec [=e!type (deref e!id)
                                _ (solve =e!type actual)
                                _ (reset e!id =e!type)]
                           success)
                         (exec [_ (reset e!id actual)]
                           success)))

    [_ ["Var" a!id]]
    (&/try-all% (&/|list (exec [=a!type (deref a!id)
                                _ (solve expected =a!type)
                                _ (reset a!id =a!type)]
                           success)
                         (exec [_ (reset a!id expected)]
                           success)))

    [_ _]
    (solve-error expected actual)
    ))

(let [&& #(and %1 %2)]
  (defn merge [x y]
    (matchv ::M/objects [x y]
      [_ ["Any" _]]
      (return y)

      [["Any" _] _]
      (return x)

      [_ ["Nothing" _]]
      (return x)

      [["Nothing" _] _]
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
    [["Lambda" [input output]]]
    (exec [_ (solve input param)]
      (return output))

    [_]
    (return (&/V "Any" nil))
    ;; (fail (str "[Type System] Can't apply type " (str func) " to type " (str param)))
    ))

(defn slot-type [record slot]
  (fn [state]
    (matchv ::M/objects [(&/|get record slot)]
      [["Left" msg]]
      (fail* msg)

      [["Right" type]]
      (return* state type))))

(def +dont-care+ (&/V "Any" nil))
