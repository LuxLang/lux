(ns lux.analyser.lambda
  (:require [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return fail]]
                 [host :as &host])
            (lux.analyser [base :as &&]
                          [env :as &env])))

;; [Resource]
(defn with-lambda [self self-type arg arg-type body]
  ;; (prn 'with-lambda (&/|length self) (&/|length arg))
  ;; (prn 'with-lambda [(aget self 0) (aget self 1)] [(aget arg 0) (aget arg 1)] (alength self) (alength arg))
  (|let [[?module1 ?name1] self
         [?module2 ?name2] arg]
    (&/with-closure
      (|do [scope-name &/get-scope-name]
        (&env/with-local (str ?module1 ";" ?name1) self-type
          (&env/with-local (str ?module2 ";" ?name2) arg-type
            (|do [=return body
                  =captured &env/captured-vars]
              (return (&/T scope-name =captured =return)))))))))

(defn close-over [scope ident register frame]
  ;; (prn 'close-over
  ;;      (&host/location scope)
  ;;      (&host/location (&/|list ident))
  ;;      register
  ;;      (->> frame (&/get$ &/$CLOSURE) (&/get$ &/$COUNTER)))
  (matchv ::M/objects [register]
    [[_ register-type]]
    (|let [register* (&/T (&/V "captured" (&/T scope
                                               (->> frame (&/get$ &/$CLOSURE) (&/get$ &/$COUNTER))
                                               register))
                          register-type)
           [?module ?name] ident
           full-name (str ?module ";" ?name)]
      (&/T register* (&/update$ &/$CLOSURE #(->> %
                                                 (&/update$ &/$COUNTER inc)
                                                 (&/update$ &/$MAPPINGS (fn [mps] (&/|put full-name register* mps))))
                                frame)))))
