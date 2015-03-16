(ns lux.type
  (:refer-clojure :exclude [deref apply merge])
  (:require [clojure.core.match :as M :refer [match matchv]]
            clojure.core.match.array
            [lux.base :as & :refer [exec return* return fail fail*
                                    |get
                                    repeat-m try-m try-all-m map-m
                                    sequence-m
                                    apply-m assert!]]))

;; [Util]
(def ^:private success (return nil))

(defn ^:private deref [id]
  (fn [state]
    (if-let [type (get-in state [::&/types :mappings id])]
      [::&/ok [state type]]
      [::&/failure (str "Unknown type-var: " id)])))

(defn ^:private reset [id type]
  (fn [state]
    (if-let [_ (get-in state [::&/types :mappings id])]
      [::&/ok [(assoc-in state [::&/types :mappings id] (&/V "Some" type)) nil]]
      [::&/failure (str "Unknown type-var: " id)])))

;; [Exports]
(def fresh-var
  (fn [state]
    (let [id (-> state ::&/types :counter)]
      [::&/ok [(update-in state [::&/types]
                          #(-> %
                               (update-in [:counter] inc)
                               (assoc-in [:mappings id] (&/V "None" nil))))
               (&/V "Var" id)]])))

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
  [::All (&/|->list (list)) "List" "a"
   [::Variant (&/|->list (list ["Cons" [::Tuple (&/|->list (list [::Bound "a"] [::App [::Bound "List"] [::Bound "a"]]))]]
                               ["Nil" [::Tuple (&/|->list (list))]]
                               ))]])

(def +type+
  (let [text [::Data "java.lang.String" (&/|->list (list))]
        type [::App [::Bound "Type"] [::Any]]
        list-of-types [::App +list+ type]
        string=>type [::App +list+ [::Tuple (&/|->list (list text type))]]]
    (->type [::All (&/|->list (list)) "Type" "_"
             [::Variant (&/|->list (list ["Any"  [::Tuple (&/|->list (list))]]
                                         ["Nothing"  [::Tuple (&/|->list (list))]]
                                         ["Data" [::Tuple (&/|->list (list text list-of-types))]]
                                         ["Tuple" list-of-types]
                                         ["Variant" string=>type]
                                         ["Record" string=>type]
                                         ["Lambda" [::Tuple (&/|->list (list type
                                                                             type))]]
                                         ["App" [::Tuple (&/|->list (list type
                                                                          type))]]
                                         ["Bound" text]
                                         ["Var" [::Data "java.lang.Long" (&/|->list (list))]]
                                         ["All" [::Tuple (&/|->list (list string=>type text text type))]]
                                         ))]])))

(defn clean [type]
  (matchv ::M/objects [type]
    [["Var" ?id]]
    (exec [=type (deref ?id)]
      (clean =type))

    [["Lambda" [?arg ?return]]]
    (exec [=arg (clean ?arg)
           =return (clean ?return)]
      (return (&/V "Lambda" (to-array [=arg =return]))))

    [["App" [?lambda ?param]]]
    (exec [=lambda (clean ?lambda)
           =param (clean ?param)]
      (return (&/V "App" (to-array [=lambda =param]))))

    [["Tuple" ?members]]
    (exec [=members (&/|map% clean ?members)]
      (return (&/V "Tuple" =members)))
    
    [["Variant" ?members]]
    (exec [=members (&/|map% (fn [[k v]]
                               (exec [=v (clean v)]
                                 (return (to-array [k =v]))))
                             ?members)]
      (return (&/V "Variant" =members)))

    [["Record" ?members]]
    (exec [=members (&/|map% (fn [[k v]]
                               (exec [=v (clean v)]
                                 (return (to-array [k =v]))))
                             ?members)]
      (return (&/V "Record" =members)))

    [["All" [?env ?name ?arg ?body]]]
    (exec [=env (&/|map% (fn [[k v]]
                           (exec [=v (clean v)]
                             (return (to-array [k =v]))))
                         ?env)]
      (return (&/V "All" (to-array [=env ?name ?arg ?body]))))

    [_]
    (return type)
    ))

(defn solve [expected actual]
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
    (exec [:let [e!elems (&/->seq e!elems)
                 a!elems (&/->seq a!elems)]
           _ (assert! (= (count e!elems) (count a!elems))
                      "Tuples must have matching element sizes.")
           _ (map-m (fn [n g] (solve n g))
                    (map vector e!elems a!elems))]
      success)

    [["Variant" e!cases] ["Variant" a!cases]]
    (exec [:let [e!cases (reduce #(assoc %1 (aget %2 0) (aget %2 1)) {} (&/->seq e!cases))
                 a!cases (reduce #(assoc %1 (aget %2 0) (aget %2 1)) {} (&/->seq a!cases))]
           _ (assert! (every? (partial contains? e!cases) (keys a!cases))
                      "The given variant contains unhandled cases.")
           _ (map-m (fn [label]
                      (solve (get e!cases label) (get a!cases label)))
                    (keys a!cases))]
      success)

    [["Record" e!fields] ["Record" a!fields]]
    (exec [:let [e!fields (reduce #(assoc %1 (aget %2 0) (aget %2 1)) {} (&/->seq e!fields))
                 a!fields (reduce #(assoc %1 (aget %2 0) (aget %2 1)) {} (&/->seq a!fields))]
           _ (assert! (every? (partial contains? a!fields) (keys e!fields))
                      "The given record lacks necessary fields.")
           _ (map-m (fn [label]
                      (solve (get e!fields label) (get a!fields label)))
                    (keys e!fields))]
      success)

    [["Lambda" [e!input e!output]] ["Lambda" [a!input a!output]]]
    (exec [_ (solve a!input e!input)]
      (solve e!output a!output))

    [["Var" e!id] _]
    (exec [=e!type (deref e!id)
           _ (solve =e!type actual)
           _ (reset e!id =e!type)]
      success)

    [_ ["Var" a!id]]
    (exec [=a!type (deref a!id)
           _ (solve expected =a!type)
           _ (reset a!id =a!type)]
      success)
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
      (fail (str "Can't merge types: " (pr-str x) " and " (pr-str y))))))

(defn apply-lambda [func param]
  (matchv ::M/objects [func]
    [["Lambda" [input output]]]
    (exec [_ (solve input param)]
      (return output))

    [_]
    (fail (str "Can't apply type " (str func) " to type " (str param)))))

(defn slot-type [record slot]
  (fn [state]
    (matchv ::M/objects [(|get record slot)]
      [["Error" msg]]
      (fail* msg)

      [["Ok" type]]
      (return* state type))))

(def +dont-care+ (&/V "Any" nil))
