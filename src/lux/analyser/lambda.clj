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
      (&env/with-local self [:self scope-name] self-type
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

(defn raise-expr [out-scope arg syntax]
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
      [::&&/Expression [::&&/tuple (map (partial raise-expr out-scope arg) ?members)] ?type]

      [::&&/variant ?tag ?value]
      [::&&/Expression [::&&/variant ?tag (raise-expr out-scope arg ?value)] ?type]
      
      [::&&/local ?idx]
      [::&&/Expression [::&&/local (inc ?idx)] ?type]
      
      [::&&/captured _ _ ?source]
      ?source

      [::&&/self ?scope ?curried]
      [::&&/Expression [::&&/self out-scope (cons arg (map (partial raise-expr out-scope arg) ?curried))] ?type]

      [::&&/global _ _]
      syntax

      [::&&/case ?variant ?base ?num-bindings ?branches]
      [::&&/Expression [::&&/case (raise-expr out-scope arg ?variant) (inc ?base) ?num-bindings
                        (for [[?pattern ?body] ?branches]
                          [?pattern (raise-expr out-scope arg ?body)])]
       ?type]

      [::&&/lambda ?scope ?captured ?args ?value]
      [::&&/Expression [::&&/lambda (rest ?scope)
                        (into {} (for [[?name ?sub-syntax] ?captured]
                                   [?name (raise-expr out-scope arg ?sub-syntax)]))
                        ?args
                        ?value]
       ?type]

      [::&&/call ?func ?args]
      [::&&/Expression [::&&/call (raise-expr out-scope arg ?func) (map (partial raise-expr out-scope arg) ?args)] ?type]

      [::&&/exec ?asts]
      [::&&/Expression [::&&/exec (map (partial raise-expr out-scope arg) ?asts)] ?type]

      [::&&/jvm-getstatic _ _]
      syntax
      
      [::&&/jvm-invokevirtual ?class ?method ?arg-classes ?obj ?args]
      [::&&/Expression [::&&/jvm-invokevirtual ?class ?method ?arg-classes
                        (raise-expr out-scope arg ?obj)
                        (map (partial raise-expr out-scope arg) ?args)]
       ?type]

      ;; Integer arithmetic
      [::&&/jvm-iadd ?x ?y]
      [::&&/Expression [::&&/jvm-iadd (raise-expr out-scope arg ?x) (raise-expr out-scope arg ?y)] ?type]

      [::&&/jvm-isub ?x ?y]
      [::&&/Expression [::&&/jvm-isub (raise-expr out-scope arg ?x) (raise-expr out-scope arg ?y)] ?type]

      [::&&/jvm-imul ?x ?y]
      [::&&/Expression [::&&/jvm-imul (raise-expr out-scope arg ?x) (raise-expr out-scope arg ?y)] ?type]

      [::&&/jvm-idiv ?x ?y]
      [::&&/Expression [::&&/jvm-idiv (raise-expr out-scope arg ?x) (raise-expr out-scope arg ?y)] ?type]

      [::&&/jvm-irem ?x ?y]
      [::&&/Expression [::&&/jvm-irem (raise-expr out-scope arg ?x) (raise-expr out-scope arg ?y)] ?type]

      ;; Long arithmetic
      [::&&/jvm-ladd ?x ?y]
      [::&&/Expression [::&&/jvm-ladd (raise-expr out-scope arg ?x) (raise-expr out-scope arg ?y)] ?type]

      [::&&/jvm-lsub ?x ?y]
      [::&&/Expression [::&&/jvm-lsub (raise-expr out-scope arg ?x) (raise-expr out-scope arg ?y)] ?type]

      [::&&/jvm-lmul ?x ?y]
      [::&&/Expression [::&&/jvm-lmul (raise-expr out-scope arg ?x) (raise-expr out-scope arg ?y)] ?type]

      [::&&/jvm-ldiv ?x ?y]
      [::&&/Expression [::&&/jvm-ldiv (raise-expr out-scope arg ?x) (raise-expr out-scope arg ?y)] ?type]

      [::&&/jvm-lrem ?x ?y]
      [::&&/Expression [::&&/jvm-lrem (raise-expr out-scope arg ?x) (raise-expr out-scope arg ?y)] ?type]

      ;; Float arithmetic
      [::&&/jvm-fadd ?x ?y]
      [::&&/Expression [::&&/jvm-fadd (raise-expr out-scope arg ?x) (raise-expr out-scope arg ?y)] ?type]

      [::&&/jvm-fsub ?x ?y]
      [::&&/Expression [::&&/jvm-fsub (raise-expr out-scope arg ?x) (raise-expr out-scope arg ?y)] ?type]

      [::&&/jvm-fmul ?x ?y]
      [::&&/Expression [::&&/jvm-fmul (raise-expr out-scope arg ?x) (raise-expr out-scope arg ?y)] ?type]

      [::&&/jvm-fdiv ?x ?y]
      [::&&/Expression [::&&/jvm-fdiv (raise-expr out-scope arg ?x) (raise-expr out-scope arg ?y)] ?type]

      [::&&/jvm-frem ?x ?y]
      [::&&/Expression [::&&/jvm-frem (raise-expr out-scope arg ?x) (raise-expr out-scope arg ?y)] ?type]

      ;; Double arithmetic
      [::&&/jvm-dadd ?x ?y]
      [::&&/Expression [::&&/jvm-dadd (raise-expr out-scope arg ?x) (raise-expr out-scope arg ?y)] ?type]

      [::&&/jvm-dsub ?x ?y]
      [::&&/Expression [::&&/jvm-dsub (raise-expr out-scope arg ?x) (raise-expr out-scope arg ?y)] ?type]

      [::&&/jvm-dmul ?x ?y]
      [::&&/Expression [::&&/jvm-dmul (raise-expr out-scope arg ?x) (raise-expr out-scope arg ?y)] ?type]

      [::&&/jvm-ddiv ?x ?y]
      [::&&/Expression [::&&/jvm-ddiv (raise-expr out-scope arg ?x) (raise-expr out-scope arg ?y)] ?type]

      [::&&/jvm-drem ?x ?y]
      [::&&/Expression [::&&/jvm-drem (raise-expr out-scope arg ?x) (raise-expr out-scope arg ?y)] ?type]
      )))

(defn re-scope [out-scope syntax]
  (let [partial-f (partial re-scope out-scope)]
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
        [::&&/Expression [::&&/tuple (map partial-f ?members)] ?type]

        [::&&/variant ?tag ?value]
        [::&&/Expression [::&&/variant ?tag (partial-f ?value)] ?type]
        
        [::&&/local ?idx]
        [::&&/Expression [::&&/local ?idx] ?type]
        
        [::&&/captured _ _ ?source]
        ?source

        [::&&/self ?scope ?curried]
        [::&&/Expression [::&&/self out-scope (map partial-f ?curried)] ?type]

        [::&&/global _ _]
        syntax

        [::&&/case ?variant ?base ?num-bindings ?branches]
        [::&&/Expression [::&&/case (partial-f ?variant) ?base ?num-bindings
                          (for [[?pattern ?body] ?branches]
                            [?pattern (partial-f ?body)])]
         ?type]

        [::&&/lambda ?scope ?captured ?args ?value]
        [::&&/Expression [::&&/lambda (rest ?scope)
                          (into {} (for [[?name ?sub-syntax] ?captured]
                                     [?name (partial-f ?sub-syntax)]))
                          ?args
                          ?value]
         ?type]

        [::&&/call ?func ?args]
        [::&&/Expression [::&&/call (partial-f ?func) (map partial-f ?args)] ?type]

        [::&&/exec ?asts]
        [::&&/Expression [::&&/exec (map partial-f ?asts)] ?type]

        [::&&/jvm-getstatic _ _]
        syntax
        
        [::&&/jvm-invokevirtual ?class ?method ?arg-classes ?obj ?args]
        [::&&/Expression [::&&/jvm-invokevirtual ?class ?method ?arg-classes
                          (partial-f ?obj)
                          (map partial-f ?args)]
         ?type]

        ;; Integer arithmetic
        [::&&/jvm-iadd ?x ?y]
        [::&&/Expression [::&&/jvm-iadd (partial-f ?x) (partial-f ?y)] ?type]

        [::&&/jvm-isub ?x ?y]
        [::&&/Expression [::&&/jvm-isub (partial-f ?x) (partial-f ?y)] ?type]

        [::&&/jvm-imul ?x ?y]
        [::&&/Expression [::&&/jvm-imul (partial-f ?x) (partial-f ?y)] ?type]

        [::&&/jvm-idiv ?x ?y]
        [::&&/Expression [::&&/jvm-idiv (partial-f ?x) (partial-f ?y)] ?type]

        [::&&/jvm-irem ?x ?y]
        [::&&/Expression [::&&/jvm-irem (partial-f ?x) (partial-f ?y)] ?type]

        ;; Long arithmetic
        [::&&/jvm-ladd ?x ?y]
        [::&&/Expression [::&&/jvm-ladd (partial-f ?x) (partial-f ?y)] ?type]

        [::&&/jvm-lsub ?x ?y]
        [::&&/Expression [::&&/jvm-lsub (partial-f ?x) (partial-f ?y)] ?type]

        [::&&/jvm-lmul ?x ?y]
        [::&&/Expression [::&&/jvm-lmul (partial-f ?x) (partial-f ?y)] ?type]

        [::&&/jvm-ldiv ?x ?y]
        [::&&/Expression [::&&/jvm-ldiv (partial-f ?x) (partial-f ?y)] ?type]

        [::&&/jvm-lrem ?x ?y]
        [::&&/Expression [::&&/jvm-lrem (partial-f ?x) (partial-f ?y)] ?type]

        ;; Float arithmetic
        [::&&/jvm-fadd ?x ?y]
        [::&&/Expression [::&&/jvm-fadd (partial-f ?x) (partial-f ?y)] ?type]

        [::&&/jvm-fsub ?x ?y]
        [::&&/Expression [::&&/jvm-fsub (partial-f ?x) (partial-f ?y)] ?type]

        [::&&/jvm-fmul ?x ?y]
        [::&&/Expression [::&&/jvm-fmul (partial-f ?x) (partial-f ?y)] ?type]

        [::&&/jvm-fdiv ?x ?y]
        [::&&/Expression [::&&/jvm-fdiv (partial-f ?x) (partial-f ?y)] ?type]

        [::&&/jvm-frem ?x ?y]
        [::&&/Expression [::&&/jvm-frem (partial-f ?x) (partial-f ?y)] ?type]

        ;; Double arithmetic
        [::&&/jvm-dadd ?x ?y]
        [::&&/Expression [::&&/jvm-dadd (partial-f ?x) (partial-f ?y)] ?type]

        [::&&/jvm-dsub ?x ?y]
        [::&&/Expression [::&&/jvm-dsub (partial-f ?x) (partial-f ?y)] ?type]

        [::&&/jvm-dmul ?x ?y]
        [::&&/Expression [::&&/jvm-dmul (partial-f ?x) (partial-f ?y)] ?type]

        [::&&/jvm-ddiv ?x ?y]
        [::&&/Expression [::&&/jvm-ddiv (partial-f ?x) (partial-f ?y)] ?type]

        [::&&/jvm-drem ?x ?y]
        [::&&/Expression [::&&/jvm-drem (partial-f ?x) (partial-f ?y)] ?type]
        ))))
