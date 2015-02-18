(ns lux.analyser.lambda
  (:require [clojure.core.match :refer [match]]
            (lux [base :as & :refer [exec return fail
                                     try-all-m map-m mapcat-m reduce-m
                                     assert!]])
            (lux.analyser [base :as &&]
                          [env :as &env])))

;; [Resource]
(defn with-lambda [self self-type arg arg-type body]
  (&/with-closure
    (exec [scope-name &/get-scope-name]
      (&env/with-local self :self self-type
        (&env/with-local arg :local arg-type
          (exec [=return body
                 =captured &env/captured-vars]
            (return [scope-name =captured =return])))))))

(defn close-over [scope ident register frame]
  (match register
    [::&&/Expression _ register-type]
    (let [register* [::&&/Expression [::&&/captured scope (get-in frame [:closure :counter]) register] register-type]]
      [register* (update-in frame [:closure] #(-> %
                                                  (update-in [:counter] inc)
                                                  (assoc-in [:mappings ident] register*)))])))

(defn raise-expr [arg syntax]
  (match syntax
    [::&&/Expression ?form ?type]
    (match ?form
      [::&&/bool ?value]
      syntax

      [::&&/int ?value]
      syntax

      [::&&/real ?value]
      syntax

      [::&&/char ?value]
      syntax

      [::&&/text ?value]
      syntax
      
      [::&&/tuple ?members]
      [::&&/Expression [::&&/tuple (map (partial raise-expr arg) ?members)] ?type]

      [::&&/variant ?tag ?members]
      [::&&/Expression [::&&/variant ?tag (map (partial raise-expr arg) ?members)] ?type]
      
      [::&&/local ?idx]
      [::&&/Expression [::&&/local (inc ?idx)] ?type]
      
      [::&&/captured _ _ ?source]
      ?source

      [::&&/self ?curried]
      [::&&/Expression [::&&/self (cons arg (map (partial raise-expr arg) ?curried))] ?type]

      [::&&/global _ _]
      syntax

      [::&&/case ?variant ?base ?num-bindings ?branches]
      [::&&/Expression [::&&/case (raise-expr arg ?variant) (inc ?base) ?num-bindings
                        (for [[?pattern ?body] ?branches]
                          [?pattern (raise-expr arg ?body)])]
       ?type]

      [::&&/lambda ?scope ?captured ?args ?value]
      [::&&/Expression [::&&/lambda (pop ?scope)
                        (into {} (for [[?name ?sub-syntax] ?captured]
                                   [?name (raise-expr arg ?sub-syntax)]))
                        ?args
                        ?value]
       ?type]

      [::&&/call ?func ?args]
      [::&&/Expression [::&&/call (raise-expr arg ?func) (map (partial raise-expr arg) ?args)] ?type]

      [::&&/exec ?asts]
      [::&&/Expression [::&&/exec (map (partial raise-expr arg) ?asts)] ?type]

      [::&&/jvm-getstatic _ _]
      syntax
      
      [::&&/jvm-invokevirtual ?class ?method ?arg-classes ?obj ?args]
      [::&&/Expression [::&&/jvm-invokevirtual ?class ?method ?arg-classes
                        (raise-expr arg ?obj)
                        (map (partial raise-expr arg) ?args)]
       ?type]

      ;; Integer arithmetic
      [::&&/jvm-iadd ?x ?y]
      [::&&/Expression [::&&/jvm-iadd (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::&&/jvm-isub ?x ?y]
      [::&&/Expression [::&&/jvm-isub (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::&&/jvm-imul ?x ?y]
      [::&&/Expression [::&&/jvm-imul (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::&&/jvm-idiv ?x ?y]
      [::&&/Expression [::&&/jvm-idiv (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::&&/jvm-irem ?x ?y]
      [::&&/Expression [::&&/jvm-irem (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      ;; Long arithmetic
      [::&&/jvm-ladd ?x ?y]
      [::&&/Expression [::&&/jvm-ladd (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::&&/jvm-lsub ?x ?y]
      [::&&/Expression [::&&/jvm-lsub (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::&&/jvm-lmul ?x ?y]
      [::&&/Expression [::&&/jvm-lmul (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::&&/jvm-ldiv ?x ?y]
      [::&&/Expression [::&&/jvm-ldiv (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::&&/jvm-lrem ?x ?y]
      [::&&/Expression [::&&/jvm-lrem (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      ;; Float arithmetic
      [::&&/jvm-fadd ?x ?y]
      [::&&/Expression [::&&/jvm-fadd (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::&&/jvm-fsub ?x ?y]
      [::&&/Expression [::&&/jvm-fsub (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::&&/jvm-fmul ?x ?y]
      [::&&/Expression [::&&/jvm-fmul (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::&&/jvm-fdiv ?x ?y]
      [::&&/Expression [::&&/jvm-fdiv (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::&&/jvm-frem ?x ?y]
      [::&&/Expression [::&&/jvm-frem (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      ;; Double arithmetic
      [::&&/jvm-dadd ?x ?y]
      [::&&/Expression [::&&/jvm-dadd (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::&&/jvm-dsub ?x ?y]
      [::&&/Expression [::&&/jvm-dsub (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::&&/jvm-dmul ?x ?y]
      [::&&/Expression [::&&/jvm-dmul (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::&&/jvm-ddiv ?x ?y]
      [::&&/Expression [::&&/jvm-ddiv (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::&&/jvm-drem ?x ?y]
      [::&&/Expression [::&&/jvm-drem (raise-expr arg ?x) (raise-expr arg ?y)] ?type]
      )))
