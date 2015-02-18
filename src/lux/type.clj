(ns lux.type
  (:refer-clojure :exclude [deref apply merge])
  (:require [clojure.core.match :refer [match]]
            [lux.util :as &util :refer [exec return* return fail fail*
                                        repeat-m try-m try-all-m map-m
                                        sequence-m
                                        apply-m assert!]]))

;; [Util]
(def ^:private success (return nil))

(defn ^:private deref [id]
  (fn [state]
    (if-let [top+bottom (get-in state [::&util/types :mappings id])]
      [::&util/ok [state top+bottom]]
      [::&util/failure (str "Unknown type-var: " id)])))

(defn ^:private update [id top bottom]
  (fn [state]
    (if-let [top+bottom (get-in state [::&util/types :mappings id])]
      [::&util/ok [(assoc-in state [::&util/types :mappings id] [top bottom]) nil]]
      [::&util/failure (str "Unknown type-var: " id)])))

;; [Interface]
(def fresh-var
  (fn [state]
    (let [id (-> state ::&util/types :counter)]
      [::&util/ok [(update-in state [::&util/types]
                              #(-> %
                                   (update-in [:counter] inc)
                                   (assoc-in [:mappings id] [[::Any] [::Nothing]])))
                   [::Var id]]])))

(def fresh-function
  (exec [=arg fresh-var
         =return fresh-var]
    (return [::Lambda =arg =return])))

;; (defn solve [expected actual]
;;   ;; (prn 'solve expected actual)
;;   (match [expected actual]
;;     [::any _]
;;     success

;;     [_ ::nothing]
;;     success

;;     [_ [::var ?id]]
;;     (exec [[=top =bottom] (deref ?id)]
;;       (try-all-m [(exec [_ (solve expected =top)]
;;                     success)
;;                   (exec [_ (solve =top expected)
;;                          _ (solve expected =bottom)
;;                          _ (update ?id expected =bottom)]
;;                     success)]))

;;     [[::var ?id] _]
;;     (exec [[=top =bottom] (deref ?id)]
;;       (try-all-m [(exec [_ (solve =bottom actual)]
;;                     success)
;;                   (exec [_ (solve actual =bottom)
;;                          _ (solve =top actual)
;;                          _ (update ?id =top actual)]
;;                     success)]))

;;     ;; [[::primitive ?prim] _]
;;     ;; (let [as-obj (case ?prim
;;     ;;                "boolean" [:lang.type/object "java.lang.Boolean" []]
;;     ;;                "int"     [:lang.type/object "java.lang.Integer" []]
;;     ;;                "long"    [:lang.type/object "java.lang.Long" []]
;;     ;;                "char"    [:lang.type/object "java.lang.Character" []]
;;     ;;                "float"   [:lang.type/object "java.lang.Float" []]
;;     ;;                "double"  [:lang.type/object "java.lang.Double" []])]
;;     ;;   (solve as-obj actual))

;;     [[::primitive ?e-prim] [::primitive ?a-prim]]
;;     (if (= ?e-prim ?a-prim)
;;       success
;;       (fail (str "Can't solve types: " (pr-str expected actual))))

;;     [[::object ?eclass []] [::object ?aclass []]]
;;     (if (.isAssignableFrom (Class/forName ?eclass) (Class/forName ?aclass))
;;       success
;;       (fail (str "Can't solve types: " (pr-str expected actual))))

;;     [_ _]
;;     (fail (str "Can't solve types: " (pr-str expected actual)))
;;     ))

;; (defn pick-matches [methods args]
;;   (if (empty? methods)
;;     (fail "No matches.")
;;     (try-all-m [(match (-> methods first second)
;;                   [::function ?args ?return]
;;                   (exec [_ (assert! (= (count ?args) (count args)) "Args-size doesn't match.")
;;                          _ (map-m (fn [[e a]] (solve e a)) (map vector ?args args))]
;;                     (return (first methods))))
;;                 (pick-matches (rest methods) args)])))

(defn clean [type]
  (match type
    [::Var ?id]
    (exec [[=top =bottom] (deref ?id)]
      (clean =top))

    [::Lambda ?arg ?return]
    (exec [=arg (clean ?arg)
           =return (clean ?return)]
      (return [::Lambda =arg =return]))

    _
    (return type)
    ))

;; Java Reflection
(def success (return nil))

(defn solve [needed given]
  (match [needed given]
    [[::Any] _]
    success

    [_ [::Nothing]]
    success

    [[::Data n!name] [::Data g!name]]
    (cond (or (= n!name g!name)
              (.isAssignableFrom (Class/forName n!name) (Class/forName g!name)))
          success

          :else
          (fail (str "not (" given " <= " needed ")")))
    
    [[::Tuple n!elems] [::Tuple g!elems]]
    (exec [_ (assert! (= (count n!elems) (count g!elems))
                      "Tuples must have matching element sizes.")
           _ (map-m (fn [n g] (solve n g))
                    (map vector n!elems g!elems))]
      success)

    [[::Variant n!cases] [::Variant g!cases]]
    (exec [_ (assert! (every? (partial contains? n!cases) (keys g!cases))
                      "The given variant contains unhandled cases.")
           _ (map-m (fn [label]
                      (solve (get n!cases label) (get g!cases label)))
                    (keys g!cases))]
      success)

    [[::Record n!fields] [::Record g!fields]]
    (exec [_ (assert! (every? (partial contains? g!fields) (keys n!fields))
                      "The given record lacks necessary fields.")
           _ (map-m (fn [label]
                      (solve (get n!fields label) (get g!fields label)))
                    (keys n!fields))]
      success)

    [[::Lambda n!input n!output] [::Lambda g!input g!output]]
    (exec [_ (solve g!input n!input)]
      (solve n!output g!output))

    [[::Var n!id] _]
    (exec [[n!top n!bottom] (deref n!id)
           _ (solve n!top given)
           _ (solve given n!bottom)
           _ (update n!id n!top given)]
      success)
    ))

(let [&& #(and %1 %2)]
  (defn merge [x y]
    (match [x y]
      [_ [::Nothing]]
      (return x)

      [[::Nothing] _]
      (return y)

      [[::Variant x!cases] [::Variant y!cases]]
      (if (and (reduce && true
                       (for [[xslot xtype] (keys x!cases)]
                         (if-let [ytype (get y!cases xslot)]
                           (= xtype ytype)
                           true)))
               (reduce && true
                       (for [[yslot ytype] (keys y!cases)]
                         (if-let [xtype (get x!cases yslot)]
                           (= xtype ytype)
                           true))))
        (return [::Variant (clojure.core/merge x!cases y!cases)])
        (fail (str "Incompatible variants: " (pr-str x) " and " (pr-str y))))

      [[::Record x!fields] [::Record y!fields]]
      (if (and (= (keys x!fields) (keys y!fields))
               (->> (keys x!fields)
                    (map #(= (get x!fields %) (get y!fields %)))
                    (reduce && true)))
        (return x)
        (fail (str "Incompatible records: " (pr-str x) " and " (pr-str y))))
      
      :else
      (fail (str "Can't merge types: " (pr-str x) " and " (pr-str y))))))

(def +dont-care-type+ [::Any])

(comment
  ;; Types
  [::Any]
  [::Nothing]
  [::Tuple (list)]
  [::Lambda input output]
  [::Variant {}]
  [::Record {}]
  [::Data name]
  [::All self {} arg body]
  [::Exists evar body]
  [::Bound name]

  ;; ???
  [::Alias name args type]
  [::Var id]
  

  ;; (deftype #rec Type
  ;;   (| #Any
  ;;     #Nothing
  ;;     (#Tuple (List Type))
  ;;     (#Lambda Type Type)
  ;;     (#Variant (List [Text Type]))
  ;;     (#Record (List [Text Type]))
  ;;     (#Data Text)))
  
  
  
  ;; (deftype #rec Kind
  ;;   (| (#Type Type)
  ;;     (#All Text (List [Text Kind]) Text Kind)))

  ;; (deftype (Higher lower)
  ;;     (| (#Lower lower)
  ;;        (#Apply (Higher lower) (Higher lower))
  ;;        (#All Text (List [Text lower]) Text (Higher lower))
  ;;        (#Exists (List [Text lower]) Text (Higher lower))))

  ;; (deftype Kind (Higher Type))
  ;; (deftype Sort (Higher Kind))
  
  
  
  ;; (deftype HList (| (#Cons (Exists x x) HList)
  ;;                   #Nil))

  ;; (def success (return nil))

  ;; (defn apply [type-lambda input]
  ;;   (match type-lambda
  ;;     [::All ?self ?env ?arg ?body]
  ;;     (let [env* (-> ?env
  ;;                    (assoc ?arg input)
  ;;                    (assoc ?self type-lambda))]
  ;;       (match ?body
  ;;         [::All ?sub-self _ ?sub-arg ?sub-body]
  ;;         [::All ?sub-self env* ?sub-arg ?sub-body]

  ;;         _
  ;;         (beta-reduce env* ?body)))))
  
  ;; (defn solve [needed given]
  ;;   (match [needed given]
  ;;     [[::Any] _]
  ;;     success

  ;;     [_ [::Nothing]]
  ;;     success
  
  ;;     [[::Tuple n!elems] [::Tuple g!elems]]
  ;;     (exec [_ (assert! (= (count n!elems) (count g!elems))
  ;;                       "Tuples must have matching element sizes.")
  ;;            _ (map-m (fn [[n g]] (solve n g))
  ;;                     (map vector n!elems g!elems))]
  ;;       success)
  
  ;;     [[::Variant n!cases] [::Variant g!cases]]
  ;;     (exec [_ (assert! (every? (partial contains? n!cases) (keys g!cases))
  ;;                       "The given variant contains unhandled cases.")
  ;;            _ (map-m (fn [label]
  ;;                       (solve (get n!cases label) (get g!cases label)))
  ;;                     (keys g!cases))]
  ;;       success)

  ;;     [[::Record n!fields] [::Record g!fields]]
  ;;     (exec [_ (assert! (every? (partial contains? g!fields) (keys n!fields))
  ;;                       "The given record lacks necessary fields.")
  ;;            _ (map-m (fn [label]
  ;;                       (solve (get n!fields label) (get g!fields label)))
  ;;                     (keys n!fields))]
  ;;       success)

  ;;     [[::Lambda n!input n!output] [::Lambda g!input g!output]]
  ;;     (exec [_ (solve g!input n!input)
  ;;            _ (solve n!output g!output)]
  ;;       success)
  ;;     ))

  ;; (deftype (List x)
  ;;     (| (#Cons x (List x))
  ;;        #Nil))

  ;; (deftype List
  ;;     (All List [x]
  ;;          (| (#Cons x (List x))
  ;;             #Nil)))

  ;; (def List
  ;;   [::All "List" {} x
  ;;    [::Variant {"Cons" [::Tuple (list [::Local x] [::Apply {} [::Local "List"] [::Local x]])]
  ;;                "Nil" [::Tuple (list)]}]])

  ;; (deftype User
  ;;     {#name Text
  ;;      #email Text
  ;;      #password Text
  ;;      #joined Time
  ;;      #last-login Time})

  ;; (deftype (Pair x y)
  ;;     [x y])

  ;; (deftype (State s a)
  ;;     (-> s [a s]))

  ;; (: + (-> Int Int Int))
  ;; (def (+ x y)
  ;;   (jvm:ladd x y))

  
  )
