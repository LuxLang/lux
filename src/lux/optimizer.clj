;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.
(ns lux.optimizer
  (:require (lux [base :as & :refer [|let |do return fail return* fail* |case defvariant]])
            (lux.analyser [base :as &a]
                          [case :as &a-case])))

;; [Tags]
(defvariant
  ("bool" 1)
  ("int" 1)
  ("real" 1)
  ("char" 1)
  ("text" 1)
  ("variant" 3)
  ("tuple" 1)
  ("apply" 2)
  ("case" 2)
  ("function" 4)
  ("ann" 3)
  ("var" 1)
  ("captured" 3)
  ("proc" 3)

  ;; Purely for optimizations
  ("loop" 1)
  )

;; For pattern-matching
(defvariant
  ("ExecPM" 1)
  ("AltPM" 2)
  ("BindPM" 2)
  ("BoolPM" 2)
  ("IntPM" 2)
  ("RealPM" 2)
  ("CharPM" 2)
  ("TextPM" 2)
  ("UnitPM" 1)
  ("VariantPM" 2)
  ("TuplePM" 1)
  ("SeqPM" 2)
  ("InnerPM" 1))

;; [Utils]
(defn ^:private transform-pm [test next-pm]
  (|case test
    (&a-case/$NoTestAC)
    ($UnitPM next-pm)

    (&a-case/$StoreTestAC _register)
    ($BindPM _register next-pm)

    (&a-case/$BoolTestAC _value)
    ($BoolPM _value next-pm)

    (&a-case/$IntTestAC _value)
    ($IntPM _value next-pm)

    (&a-case/$RealTestAC _value)
    ($RealPM _value next-pm)

    (&a-case/$CharTestAC _value)
    ($CharPM _value next-pm)

    (&a-case/$TextTestAC _value)
    ($TextPM _value next-pm)

    (&a-case/$VariantTestAC _idx _num-options _sub-test)
    (|let [idx+ (if (= _idx (dec _num-options))
                  (&/$Right _idx)
                  (&/$Left _idx))]
      ($VariantPM idx+ (transform-pm _sub-test next-pm)))

    (&a-case/$TupleTestAC _sub-tests)
    (|case _sub-tests
      (&/$Nil)
      ($UnitPM next-pm)

      (&/$Cons _only-test (&/$Nil))
      (transform-pm _only-test next-pm)

      _
      (|let [tuple-size (&/|length _sub-tests)]
        ($TuplePM (&/fold (fn [next-pm* idx+test*]
                            (|let [[idx test*] idx+test*]
                              ($SeqPM (if (< idx (dec tuple-size))
                                        (&/$Left idx)
                                        (&/$Right idx))
                                      (transform-pm test* next-pm*))))
                          ($InnerPM next-pm)
                          (&/zip2 (&/|reverse (&/|range tuple-size))
                                  (&/|reverse _sub-tests))))))
    ))

(defn ^:private fuse-pms [pre post]
  (|case (&/T [pre post])
    [($UnitPM _pre) ($UnitPM _post)]
    ($UnitPM (fuse-pms _pre _post))

    [($InnerPM _pre) ($InnerPM _post)]
    ($InnerPM (fuse-pms _pre _post))

    [($BindPM _pre-var-id _pre-next-pm) ($BindPM _post-var-id _post-next-pm)]
    (if (= _pre-var-id _post-var-id)
      ($BindPM _pre-var-id (fuse-pms _pre-next-pm _post-next-pm))
      ($AltPM pre post))

    [($BoolPM _pre-value _pre-next) ($BoolPM _post-value _post-next)]
    (if (= _pre-value _post-value)
      ($BoolPM _pre-value (fuse-pms _pre-next _post-next))
      ($AltPM pre post))

    [($IntPM _pre-value _pre-next) ($IntPM _post-value _post-next)]
    (if (= _pre-value _post-value)
      ($IntPM _pre-value (fuse-pms _pre-next _post-next))
      ($AltPM pre post))

    [($RealPM _pre-value _pre-next) ($RealPM _post-value _post-next)]
    (if (= _pre-value _post-value)
      ($RealPM _pre-value (fuse-pms _pre-next _post-next))
      ($AltPM pre post))

    [($CharPM _pre-value _pre-next) ($CharPM _post-value _post-next)]
    (if (= _pre-value _post-value)
      ($CharPM _pre-value (fuse-pms _pre-next _post-next))
      ($AltPM pre post))

    [($TextPM _pre-value _pre-next) ($TextPM _post-value _post-next)]
    (if (= _pre-value _post-value)
      ($TextPM _pre-value (fuse-pms _pre-next _post-next))
      ($AltPM pre post))

    [($TuplePM _pre-next-pm) ($TuplePM _post-next-pm)]
    ($TuplePM (fuse-pms _pre-next-pm _post-next-pm))
    
    [($SeqPM (&/$Left _pre-idx) _pre-next-pm) ($SeqPM (&/$Left _post-idx) _post-next-pm)]
    (if (= _pre-idx _post-idx)
      ($SeqPM (&/$Left _pre-idx) (fuse-pms _pre-next-pm _post-next-pm))
      ($AltPM pre post))

    [($SeqPM (&/$Right _pre-idx) _pre-next-pm) ($SeqPM (&/$Right _post-idx) _post-next-pm)]
    (if (= _pre-idx _post-idx)
      ($SeqPM (&/$Right _pre-idx) (fuse-pms _pre-next-pm _post-next-pm))
      ($AltPM pre post))

    [($VariantPM (&/$Left _pre-idx) _pre-next-pm) ($VariantPM (&/$Left _post-idx) _post-next-pm)]
    (if (= _pre-idx _post-idx)
      ($VariantPM (&/$Left _pre-idx) (fuse-pms _pre-next-pm _post-next-pm))
      ($AltPM pre post))

    [($VariantPM (&/$Right _pre-idx) _pre-next-pm) ($VariantPM (&/$Right _post-idx) _post-next-pm)]
    (if (= _pre-idx _post-idx)
      ($VariantPM (&/$Right _pre-idx) (fuse-pms _pre-next-pm _post-next-pm))
      ($AltPM pre post))
    
    _
    ($AltPM pre post)
    ))

(defn ^:private optimize-pm [branches]
  (|let [;; branches (&/|reverse branches*)
         bodies (&/|map &/|second branches)
         bodies-ids (&/|range (&/|length bodies))
         pms (&/|map (fn [branch]
                       (|let [[[_pattern _] _body-id] branch]
                         (transform-pm _pattern ($ExecPM _body-id))))
                     (&/zip2 branches
                             bodies-ids))
         _ (prn 'pms (&/|length bodies) (&/->seq bodies-ids))
         _ (&/|map (comp prn &/adt->text) pms)]
    (|case (&/|reverse pms)
      (&/$Nil)
      (assert false)

      (&/$Cons _head-pm _tail-pms)
      (do (prn 'pms-FUSED (&/adt->text (&/fold fuse-pms _head-pm _tail-pms)))
        (&/T [(&/fold fuse-pms _head-pm _tail-pms)
              bodies]))

      ;; (&/$Cons _last-pm _rev-pms)
      ;; (do (prn 'pms-FUSED (&/adt->text (&/fold (fn [post pre] (fuse-pms pre post)) _last-pm _rev-pms)))
      ;;   (&/T [(&/fold (fn [post pre] (fuse-pms pre post)) _last-pm _rev-pms)
      ;;         bodies]))
      )))

(defn ^:private shift-pattern [pattern]
  (|case pattern
    ($UnitPM _next-pm)
    ($UnitPM (shift-pattern _next-pm))

    ($InnerPM _next-pm)
    ($InnerPM (shift-pattern _next-pm))

    ($BindPM _var-id _next-pm)
    ($BindPM (inc _var-id) (shift-pattern _next-pm))

    ($BoolPM _value _next-pm)
    ($BoolPM _value (shift-pattern _next-pm))

    ($IntPM _value _next-pm)
    ($IntPM _value (shift-pattern _next-pm))

    ($RealPM _value _next-pm)
    ($RealPM _value (shift-pattern _next-pm))

    ($CharPM _value _next-pm)
    ($CharPM _value (shift-pattern _next-pm))

    ($TextPM _value _next-pm)
    ($TextPM _value (shift-pattern _next-pm))

    ($TuplePM _idx+ _next-pm)
    ($TuplePM _idx+ (shift-pattern _next-pm))

    ($VariantPM _idx+ _next-pm)
    ($VariantPM _idx+ (shift-pattern _next-pm))
    
    ($AltPM _left-pm _right-pm)
    ($AltPM (shift-pattern _left-pm) (shift-pattern _right-pm))
    
    ))

(defn ^:private drop-scope [source]
  (|case source
    [meta ($captured scope idx source*)]
    (&/T [meta ($captured (&/|but-last scope) idx (drop-scope source*))])

    _
    source))

(defn ^:private de-scope [scope]
  "(-> Scope Scope)"
  (|case scope
    (&/$Cons _module (&/$Cons _def (&/$Cons _level-to-remove _levels-to-keep)))
    (&/$Cons _module (&/$Cons _def _levels-to-keep))))

(defn shift-function-body [own-body? body]
  "(-> Optimized Optimized)"
  (|let [[meta body-] body]
    (|case body-
      ($variant idx is-last? value)
      (&/T [meta ($variant idx is-last? (shift-function-body own-body? value))])
      
      ($tuple elems)
      (&/T [meta ($tuple (&/|map (partial shift-function-body own-body?) elems))])
      
      ($case value [_pm _bodies])
      (&/T [meta ($case (shift-function-body own-body? value)
                        (&/T [(if own-body?
                                (shift-pattern _pm)
                                _pm)
                              (&/|map (partial shift-function-body own-body?) _bodies)]))])
      
      ($function arity scope captured body*)
      (&/T [meta ($function arity
                            (de-scope scope)
                            (&/|map (fn [capture]
                                      (|let [[_name [_meta ($captured _scope _idx _source)]] capture]
                                        (&/T [_name (&/T [_meta ($captured (de-scope _scope) _idx (shift-function-body own-body? _source))])])))
                                    captured)
                            (shift-function-body false body*))])
      
      ($ann value-expr type-expr type-type)
      (&/T [meta ($ann (shift-function-body own-body? value-expr)
                       type-expr
                       type-type)])
      
      ($var var-kind)
      (if own-body?
        (|case var-kind
          (&/$Local 0)
          (&/T [meta ($apply body
                             (&/|list [meta ($var (&/$Local 1))]))])
          
          (&/$Local idx)
          (&/T [meta ($var (&/$Local (inc idx)))])
          
          (&/$Global ?module ?name)
          body)
        body)

      ($apply [meta-0 ($var (&/$Local 0))] args)
      (if own-body?
        (&/T [meta ($apply (&/T [meta-0 ($var (&/$Local 0))])
                           (&/$Cons (&/T [meta-0 ($var (&/$Local 1))])
                                    (&/|map (partial shift-function-body own-body?) args)))])
        (&/T [meta ($apply (&/T [meta-0 ($var (&/$Local 0))])
                           (&/|map (partial shift-function-body own-body?) args))]))

      ($apply func args)
      (&/T [meta ($apply (shift-function-body own-body? func)
                         (&/|map (partial shift-function-body own-body?) args))])
      
      ($captured scope idx source)
      (if own-body?
        source
        (|case scope
          (&/$Cons _ (&/$Cons _ (&/$Nil)))
          source

          _
          (&/T [meta ($captured (de-scope scope) idx (shift-function-body own-body? source))])))
      
      ($proc proc-ident args special-args)
      (&/T [meta ($proc proc-ident (&/|map (partial shift-function-body own-body?) args) special-args)])

      ($loop args)
      (&/T [meta ($loop (&/|map (partial shift-function-body own-body?) args))])
      
      _
      body
      )))

(defn ^:private optimize-loop [arity optim]
  "(-> Int Optimized [Optimized Bool])"
  (|let [[meta optim-] optim]
    (|case optim-
      ($apply [meta-0 ($var (&/$Local 0))] _args)
      (if (= arity (&/|length _args))
        (&/T [meta-0 ($loop (&/|map (partial optimize-loop -1) _args))])
        optim)

      ($apply func args)
      (&/T [meta ($apply (optimize-loop -1 func)
                         (&/|map (partial optimize-loop -1) args))])

      ($case _value [_pattern _bodies])
      (&/T [meta ($case _value
                        (&/T [_pattern
                              (&/|map (partial optimize-loop arity)
                                      _bodies)]))])

      ($function _arity _scope _captured _body)
      (&/T [meta ($function _arity _scope _captured (optimize-loop _arity _body))])
      
      ($ann _value-expr _type-expr _type-type)
      (&/T [meta ($ann (optimize-loop arity _value-expr) _type-expr _type-type)])

      ($variant idx is-last? value)
      (&/T [meta ($variant idx is-last? (optimize-loop -1 value))])

      ($tuple elems)
      (&/T [meta ($tuple (&/|map (partial optimize-loop -1) elems))])
      
      _
      optim
      )))

(let [optimize-closure (fn [optimize closure]
                         (&/|map (fn [capture]
                                   (|let [[_name _analysis] capture]
                                     (&/T [_name (optimize _analysis)])))
                                 closure))]
  (defn ^:private pass-0 [analysis]
    "(-> Analysis Optimized)"
    (|let [[meta analysis-] analysis]
      (|case analysis-
        (&a/$bool value)
        (&/T [meta ($bool value)])
        
        (&a/$int value)
        (&/T [meta ($int value)])
        
        (&a/$real value)
        (&/T [meta ($real value)])
        
        (&a/$char value)
        (&/T [meta ($char value)])
        
        (&a/$text value)
        (&/T [meta ($text value)])
        
        (&a/$variant idx is-last? value)
        (&/T [meta ($variant idx is-last? (pass-0 value))])
        
        (&a/$tuple elems)
        (&/T [meta ($tuple (&/|map pass-0 elems))])
        
        (&a/$apply func args)
        (&/T [meta ($apply (pass-0 func) (&/|map pass-0 args))])
        
        (&a/$case value branches)
        (&/T [meta ($case (pass-0 value)
                          (optimize-pm (&/|map (fn [branch]
                                                 (|let [[_pattern _body] branch]
                                                   (&/T [_pattern (pass-0 _body)])))
                                               branches)))])
        
        (&a/$lambda scope captured body)
        (|case (pass-0 body)
          [_ ($function _arity _scope _captured _body)]
          (&/T [meta ($function (inc _arity) scope (optimize-closure pass-0 captured) (shift-function-body true _body))])

          =body
          (&/T [meta ($function 1 scope (optimize-closure pass-0 captured) =body)]))
        
        (&a/$ann value-expr type-expr type-type)
        (&/T [meta ($ann (pass-0 value-expr) type-expr type-type)])
        
        (&a/$var var-kind)
        (&/T [meta ($var var-kind)])
        
        (&a/$captured scope idx source)
        (&/T [meta ($captured scope idx (pass-0 source))])

        (&a/$proc proc-ident args special-args)
        (&/T [meta ($proc proc-ident (&/|map pass-0 args) special-args)])
        
        _
        (assert false (prn-str 'pass-0 (&/adt->text analysis)))
        ))))

;; [Exports]
(defn optimize [analysis]
  "(-> Analysis Optimized)"
  (->> analysis pass-0 (optimize-loop -1)))
