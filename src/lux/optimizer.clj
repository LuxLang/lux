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
  ("nat" 1)
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
  ("let" 3)
  ("record-get" 2)
  )

;; For pattern-matching
(defvariant
  ("PopPM" 0)
  ("BindPM" 1)
  ("BoolPM" 1)
  ("NatPM" 1)
  ("IntPM" 1)
  ("RealPM" 1)
  ("CharPM" 1)
  ("TextPM" 1)
  ("VariantPM" 1)
  ("TuplePM" 1)
  ("AltPM" 2)
  ("SeqPM" 2)
  ("ExecPM" 1))

;; [Utils]
(defn ^:private transform-pm* [test]
  (|case test
    (&a-case/$NoTestAC)
    (&/|list $PopPM)

    (&a-case/$StoreTestAC _register)
    (&/|list ($BindPM _register)
             $PopPM)

    (&a-case/$BoolTestAC _value)
    (&/|list ($BoolPM _value)
             $PopPM)

    (&a-case/$NatTestAC _value)
    (&/|list ($NatPM _value)
             $PopPM)

    (&a-case/$IntTestAC _value)
    (&/|list ($IntPM _value)
             $PopPM)

    (&a-case/$RealTestAC _value)
    (&/|list ($RealPM _value)
             $PopPM)

    (&a-case/$CharTestAC _value)
    (&/|list ($CharPM _value)
             $PopPM)

    (&a-case/$TextTestAC _value)
    (&/|list ($TextPM _value)
             $PopPM)

    (&a-case/$VariantTestAC _idx _num-options _sub-test)
    (&/|++ (&/|list ($VariantPM (if (= _idx (dec _num-options))
                                  (&/$Right _idx)
                                  (&/$Left _idx))))
           (&/|++ (transform-pm* _sub-test)
                  (&/|list $PopPM)))

    (&a-case/$TupleTestAC _sub-tests)
    (|case _sub-tests
      (&/$Nil)
      (&/|list $PopPM)

      (&/$Cons _only-test (&/$Nil))
      (transform-pm* _only-test)

      _
      (|let [tuple-size (&/|length _sub-tests)]
        (&/|++ (&/flat-map (fn [idx+test*]
                             (|let [[idx test*] idx+test*]
                               (&/$Cons ($TuplePM (if (< idx (dec tuple-size))
                                                    (&/$Left idx)
                                                    (&/$Right idx)))
                                        (transform-pm* test*))))
                           (&/zip2 (&/|range tuple-size)
                                   _sub-tests))
               (&/|list $PopPM))))))

(defn ^:private clean-unnecessary-pops [steps]
  (|case steps
    (&/$Cons ($PopPM) _steps)
    (clean-unnecessary-pops _steps)

    _
    steps))

(defn ^:private transform-pm [test body-id]
  (&/fold (fn [right left] ($SeqPM left right))
          ($ExecPM body-id)
          (clean-unnecessary-pops (&/|reverse (transform-pm* test)))))

(defn ^:private fuse-pms [pre post]
  (|case (&/T [pre post])
    [($PopPM) ($PopPM)]
    $PopPM

    [($BindPM _pre-var-id) ($BindPM _post-var-id)]
    (if (= _pre-var-id _post-var-id)
      ($BindPM _pre-var-id)
      ($AltPM pre post))

    [($BoolPM _pre-value) ($BoolPM _post-value)]
    (if (= _pre-value _post-value)
      ($BoolPM _pre-value)
      ($AltPM pre post))

    [($NatPM _pre-value) ($NatPM _post-value)]
    (if (= _pre-value _post-value)
      ($NatPM _pre-value)
      ($AltPM pre post))

    [($IntPM _pre-value) ($IntPM _post-value)]
    (if (= _pre-value _post-value)
      ($IntPM _pre-value)
      ($AltPM pre post))

    [($RealPM _pre-value) ($RealPM _post-value)]
    (if (= _pre-value _post-value)
      ($RealPM _pre-value)
      ($AltPM pre post))

    [($CharPM _pre-value) ($CharPM _post-value)]
    (if (= _pre-value _post-value)
      ($CharPM _pre-value)
      ($AltPM pre post))

    [($TextPM _pre-value) ($TextPM _post-value)]
    (if (= _pre-value _post-value)
      ($TextPM _pre-value)
      ($AltPM pre post))

    [($TuplePM (&/$Left _pre-idx)) ($TuplePM (&/$Left _post-idx))]
    (if (= _pre-idx _post-idx)
      ($TuplePM (&/$Left _pre-idx))
      ($AltPM pre post))

    [($TuplePM (&/$Right _pre-idx)) ($TuplePM (&/$Right _post-idx))]
    (if (= _pre-idx _post-idx)
      ($TuplePM (&/$Right _pre-idx))
      ($AltPM pre post))

    [($VariantPM (&/$Left _pre-idx)) ($VariantPM (&/$Left _post-idx))]
    (if (= _pre-idx _post-idx)
      ($VariantPM (&/$Left _pre-idx))
      ($AltPM pre post))

    [($VariantPM (&/$Right _pre-idx)) ($VariantPM (&/$Right _post-idx))]
    (if (= _pre-idx _post-idx)
      ($VariantPM (&/$Right _pre-idx))
      ($AltPM pre post))

    [($SeqPM _pre-pre _pre-post) ($SeqPM _post-pre _post-post)]
    (|case (fuse-pms _pre-pre _post-pre)
      ($AltPM _ _)
      ($AltPM pre post)

      fused-pre
      ($SeqPM fused-pre (fuse-pms _pre-post _post-post)))

    _
    ($AltPM pre post)
    ))

(defn ^:private optimize-pm [branches]
  (|let [;; branches (&/|reverse branches*)
         bodies (&/|map &/|second branches)
         bodies-ids (&/|range (&/|length bodies))
         pms (&/|map (fn [branch]
                       (|let [[[_pattern _] _body-id] branch]
                         (transform-pm _pattern _body-id)))
                     (&/zip2 branches
                             bodies-ids))]
    (|case (&/|reverse pms)
      (&/$Nil)
      (assert false)

      (&/$Cons _head-pm _tail-pms)
      (&/T [(&/fold fuse-pms _head-pm _tail-pms)
            bodies])
      )))

(defn ^:private shift-pattern [pattern]
  (|case pattern
    ($BindPM _var-id)
    ($BindPM (inc _var-id))

    ($SeqPM _left-pm _right-pm)
    ($SeqPM (shift-pattern _left-pm) (shift-pattern _right-pm))

    ($AltPM _left-pm _right-pm)
    ($AltPM (shift-pattern _left-pm) (shift-pattern _right-pm))

    _
    pattern
    ))

(defn ^:private de-scope [old-scope new-scope scope]
  "(-> Scope Scope Scope Scope)"
  (if (identical? new-scope scope)
    old-scope
    scope))

(defn shift-function-body [old-scope new-scope own-body? body]
  "(-> Scope Scope Bool Optimized Optimized)"
  (|let [[meta body-] body]
    (|case body-
      ($variant idx is-last? value)
      (&/T [meta ($variant idx is-last? (shift-function-body old-scope new-scope own-body? value))])
      
      ($tuple elems)
      (&/T [meta ($tuple (&/|map (partial shift-function-body old-scope new-scope own-body?) elems))])
      
      ($case value [_pm _bodies])
      (&/T [meta ($case (shift-function-body old-scope new-scope own-body? value)
                        (&/T [(if own-body?
                                (shift-pattern _pm)
                                _pm)
                              (&/|map (partial shift-function-body old-scope new-scope own-body?) _bodies)]))])
      
      ($function arity scope captured body*)
      (|let [scope* (de-scope old-scope new-scope scope)]
        (&/T [meta ($function arity
                              scope*
                              (&/|map (fn [capture]
                                        (|let [[_name [_meta ($captured _scope _idx _source)]] capture]
                                          (&/T [_name (&/T [_meta ($captured scope* _idx (shift-function-body old-scope new-scope own-body? _source))])])))
                                      captured)
                              (shift-function-body old-scope new-scope false body*))]))
      
      ($ann value-expr type-expr type-type)
      (&/T [meta ($ann (shift-function-body old-scope new-scope own-body? value-expr)
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
                                    (&/|map (partial shift-function-body old-scope new-scope own-body?) args)))])
        (&/T [meta ($apply (&/T [meta-0 ($var (&/$Local 0))])
                           (&/|map (partial shift-function-body old-scope new-scope own-body?) args))]))

      ($apply func args)
      (&/T [meta ($apply (shift-function-body old-scope new-scope own-body? func)
                         (&/|map (partial shift-function-body old-scope new-scope own-body?) args))])
      
      ($captured scope idx source)
      (if own-body?
        source
        (|case scope
          (&/$Cons _ (&/$Cons _ (&/$Nil)))
          source

          _
          (&/T [meta ($captured (de-scope old-scope new-scope scope) idx (shift-function-body old-scope new-scope own-body? source))])))
      
      ($proc proc-ident args special-args)
      (&/T [meta ($proc proc-ident (&/|map (partial shift-function-body old-scope new-scope own-body?) args) special-args)])

      ($loop args)
      (&/T [meta ($loop (&/|map (partial shift-function-body old-scope new-scope own-body?) args))])

      ($let _value _register _body)
      (&/T [meta ($let (shift-function-body old-scope new-scope own-body? _value)
                       (if own-body?
                         (inc _register)
                         _register)
                       (shift-function-body old-scope new-scope own-body? _body))])

      ($record-get _value _path)
      (&/T [meta ($record-get (shift-function-body old-scope new-scope own-body? _value)
                              _path)])
      
      _
      body
      )))

(defn ^:private record-read-path [pms member-idx]
  "(-> (List PM) Idx (List Idx))"
  (loop [current-idx 0
         pms pms]
    (|case pms
      (&/$Nil)
      &/$None
      
      (&/$Cons _pm _pms)
      (|case _pm
        (&a-case/$NoTestAC)
        (recur (inc current-idx)
               _pms)
        
        (&a-case/$StoreTestAC _register)
        (if (= member-idx _register)
          (&/|list (&/T [current-idx (&/|empty? _pms)]))
          (recur (inc current-idx)
                 _pms))

        (&a-case/$TupleTestAC _sub-tests)
        (let [sub-path (record-read-path _sub-tests member-idx)]
          (if (not (&/|empty? sub-path))
            (&/$Cons (&/T [current-idx (&/|empty? _pms)]) sub-path)
            (recur (inc current-idx)
                   _pms)
            ))
        
        _
        (&/|list))
      )))

(defn ^:private optimize-loop [arity optim]
  "(-> Int Optimized Optimized)"
  (|let [[meta optim-] optim]
    (|case optim-
      ($apply [meta-0 ($var (&/$Local 0))] _args)
      (if (= arity (&/|length _args))
        (&/T [meta-0 ($loop (&/|map (partial optimize-loop -1) _args))])
        optim)

      ($case _value [_pattern _bodies])
      (&/T [meta ($case _value
                        (&/T [_pattern
                              (&/|map (partial optimize-loop arity)
                                      _bodies)]))])

      ($function _arity _scope _captured _body)
      (&/T [meta ($function _arity _scope _captured (optimize-loop _arity _body))])
      
      ($ann _value-expr _type-expr _type-type)
      (&/T [meta ($ann (optimize-loop arity _value-expr) _type-expr _type-type)])

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
        
        (&a/$nat value)
        (&/T [meta ($nat value)])

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
        (|case branches
          (&/$Cons [(&a-case/$StoreTestAC _register) _body] (&/$Nil))
          (&/T [meta ($let (pass-0 value) _register (pass-0 _body))])

          (&/$Cons [(&a-case/$TupleTestAC _sub-tests) [_ (&a/$var (&/$Local _member-idx))]] (&/$Nil))
          (|let [_path (record-read-path _sub-tests _member-idx)]
            (if (&/|empty? _path)
              (&/T [meta ($case (pass-0 value)
                                (optimize-pm (&/|map (fn [branch]
                                                       (|let [[_pattern _body] branch]
                                                         (&/T [_pattern (pass-0 _body)])))
                                                     branches)))])
              (&/T [meta ($record-get (pass-0 value) _path)])))

          _
          (&/T [meta ($case (pass-0 value)
                            (optimize-pm (&/|map (fn [branch]
                                                   (|let [[_pattern _body] branch]
                                                     (&/T [_pattern (pass-0 _body)])))
                                                 branches)))]))
        
        (&a/$lambda scope captured body)
        (|case (pass-0 body)
          [_ ($function _arity _scope _captured _body)]
          (&/T [meta ($function (inc _arity) scope (optimize-closure pass-0 captured) (shift-function-body scope _scope true _body))])

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
