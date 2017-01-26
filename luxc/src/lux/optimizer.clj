;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.
(ns lux.optimizer
  (:require (lux [base :as & :refer [|let |do return return* |case defvariant]])
            (lux.analyser [base :as &a]
                          [case :as &a-case])))

;; [Tags]
(defvariant
  ;; These tags just have a one-to-one correspondence with Analysis data-structures.
  ("bool" 1)
  ("nat" 1)
  ("int" 1)
  ("deg" 1)
  ("real" 1)
  ("char" 1)
  ("text" 1)
  ("variant" 3)
  ("tuple" 1)
  ("apply" 2)
  ("case" 2)
  ("function" 5)
  ("ann" 2)
  ("var" 1)
  ("captured" 3)
  ("proc" 3)

  ;; These other tags represent higher-order constructs that manifest
  ;; themselves as patterns in the code.
  ;; Lux doesn't formally provide these features, but some macros
  ;; expose ways to implement them in terms of the other (primitive)
  ;; features.
  ;; The optimizer looks for those usage patterns and transforms them
  ;; into explicit constructs, which are then subject to specialized optimizations.

  ;; Loop scope, for doing loop inlining
  ("loop" 3) ;; {register-offset Int, inits (List Optimized), body Optimized}
  ;; This is loop iteration, as expected in imperative programming.
  ("iter" 2) ;; {register-offset Int, vals (List Optimized)}
  ;; This is a simple let-expression, as opposed to the more general pattern-matching.
  ("let" 3)
  ;; This is an access to a record's member. It can be multi-level:
  ;; e.g. record.l1.l2.l3
  ;; The record-get token stores the path, for simpler compilation.
  ("record-get" 2)
  ;; Regular, run-of-the-mill if expressions.
  ("if" 3)
  )

;; [Utils]

;; [[Pattern-Matching Traversal Optimization]]

;; This represents an alternative way to view pattern-matching.
;; The PM that Lux provides has declarative semantics, with the user
;; specifying how his data is shaped, but not how to traverse it.
;; The optimizer's PM is operational in nature, and relies on
;; specifying a path of traversal, with a variety of operations that
;; can be done along the way.
;; The algorithm relies on looking at pattern-matching as traversing a
;; (possibly) branching path, where each step along the path
;; corresponds to a value, the ends of the path are the jumping-off
;; points for the bodies of branches, and branching decisions can be
;; backtracked, if they don't result in a valid jump.
(defvariant
  ;; Throw away the current data-node (CDN). It's useless.
  ("PopPM" 0)
  ;; Store the CDN in a register.
  ("BindPM" 1)
  ;; Compare the CDN with a boolean value.
  ("BoolPM" 1)
  ;; Compare the CDN with a natural value.
  ("NatPM" 1)
  ;; Compare the CDN with an integer value.
  ("IntPM" 1)
  ;; Compare the CDN with a degree value.
  ("DegPM" 1)
  ;; Compare the CDN with a real value.
  ("RealPM" 1)
  ;; Compare the CDN with a character value.
  ("CharPM" 1)
  ;; Compare the CDN with a text value.
  ("TextPM" 1)
  ;; Compare the CDN with a variant value. If valid, proceed to test
  ;; the variant's inner value.
  ("VariantPM" 1)
  ;; Access a tuple value at a given index, for further examination.
  ("TuplePM" 1)
  ;; Creates an instance of the backtracking info, as a preparatory
  ;; step to exploring one of the branching paths.
  ("AltPM" 2)
  ;; Allows to test the CDN, while keeping a copy of it for more
  ;; tasting later on.
  ;; If necessary when doing multiple tests on a single value, like
  ;; when testing multiple parts of a tuple.
  ("SeqPM" 2)
  ;; This is the jumping-off point for the PM part, where the PM
  ;; data-structure is thrown away and the program jumps to the
  ;; branch's body.
  ("ExecPM" 1))

(defn de-meta
  "(-> Optimized Optimized)"
  [optim]
  (|let [[meta optim-] optim]
    (|case optim-
      ($variant idx is-last? value)
      ($variant idx is-last? (de-meta value))
      
      ($tuple elems)
      ($tuple (&/|map de-meta elems))
      
      ($case value [_pm _bodies])
      ($case (de-meta value)
             (&/T [_pm (&/|map de-meta _bodies)]))
      
      ($function _register-offset arity scope captured body*)
      ($function _register-offset
                 arity
                 scope
                 (&/|map (fn [capture]
                           (|let [[_name [_meta ($captured _scope _idx _source)]] capture]
                             (&/T [_name ($captured _scope _idx (de-meta _source))])))
                         captured)
                 (de-meta body*))

      ($ann value-expr type-expr)
      (de-meta value-expr)
      
      ($apply func args)
      ($apply (de-meta func)
              (&/|map de-meta args))
      
      ($captured scope idx source)
      ($captured scope idx (de-meta source))
      
      ($proc proc-ident args special-args)
      ($proc proc-ident (&/|map de-meta args) special-args)

      ($loop _register-offset _inits _body)
      ($loop _register-offset
             (&/|map de-meta _inits)
             (de-meta _body))
      
      ($iter _iter-register-offset args)
      ($iter _iter-register-offset
             (&/|map de-meta args))

      ($let _value _register _body)
      ($let (de-meta _value)
            _register
            (de-meta _body))

      ($record-get _value _path)
      ($record-get (de-meta _value)
                   _path)

      ($if _test _then _else)
      ($if (de-meta _test)
           (de-meta _then)
           (de-meta _else))
      
      _
      optim-
      )))

;; This function does a simple transformation from the declarative
;; model of PM of the analyser, to the operational model of PM of the
;; optimizer.
;; You may notice that all branches end in PopPM.
;; The reason is that testing does not immediately imply throwing away
;; the data to be tested, which is why a popping step must immediately follow.
(defn ^:private transform-pm* [test]
  (|case test
    (&a-case/$NoTestAC)
    (&/|list $PopPM)

    (&a-case/$StoreTestAC _register)
    (&/|list ($BindPM _register))

    (&a-case/$BoolTestAC _value)
    (&/|list ($BoolPM _value)
             $PopPM)

    (&a-case/$NatTestAC _value)
    (&/|list ($NatPM _value)
             $PopPM)

    (&a-case/$IntTestAC _value)
    (&/|list ($IntPM _value)
             $PopPM)

    (&a-case/$DegTestAC _value)
    (&/|list ($DegPM _value)
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
      ;; An empty tuple corresponds to unit, which can't be tested in
      ;; any meaningful way, so it's just popped.
      (&/$Nil)
      (&/|list $PopPM)

      ;; A tuple of a single element is equivalent to the element
      ;; itself, to the element's PM is generated.
      (&/$Cons _only-test (&/$Nil))
      (transform-pm* _only-test)

      ;; Single tuple PM features the tests of each tuple member
      ;; inlined, it's operational equivalent is interleaving the
      ;; access to each tuple member, followed by the testing of said
      ;; member.
      ;; That is way each sequence of access+subtesting gets generated
      ;; and later they all get concatenated.
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

;; It will be common for pattern-matching on a very nested
;; data-structure to require popping all the intermediate
;; data-structures that were visited once it's all done.
;; However, the PM infrastructure employs a single data-stack to keep
;; all data nodes in the trajectory, and that data-stack can just be
;; thrown again entirely, in just one step.
;; Because of that, any ending POPs prior to throwing away the
;; data-stack would be completely useless.
;; This function cleans them all up, to avoid wasteful computation later.
(defn ^:private clean-unnecessary-pops [steps]
  (|case steps
    (&/$Cons ($PopPM) _steps)
    (clean-unnecessary-pops _steps)

    _
    steps))

;; This transforms a single branch of a PM tree into it's operational
;; equivalent, while also associating the PM of the branch with the
;; jump to the branch's body.
(defn ^:private transform-pm [test body-id]
  (&/fold (fn [right left] ($SeqPM left right))
          ($ExecPM body-id)
          (clean-unnecessary-pops (&/|reverse (transform-pm* test)))))

(defn ^:private pattern->text [pattern]
  (|case pattern
    ($PopPM)
    "$PopPM"

    ($BindPM _id)
    (str "($BindPM " _id ")")

    ($BoolPM _value)
    (str "($BoolPM " (pr-str _value) ")")

    ($NatPM _value)
    (str "($NatPM " (pr-str _value) ")")

    ($IntPM _value)
    (str "($IntPM " (pr-str _value) ")")

    ($DegPM _value)
    (str "($DegPM " (pr-str _value) ")")

    ($RealPM _value)
    (str "($RealPM " (pr-str _value) ")")

    ($CharPM _value)
    (str "($CharPM " (pr-str _value) ")")

    ($TextPM _value)
    (str "($TextPM " (pr-str _value) ")")

    ($TuplePM (&/$Left _idx))
    (str "($TuplePM L" _idx ")")

    ($TuplePM (&/$Right _idx))
    (str "($TuplePM R" _idx ")")

    ($VariantPM (&/$Left _idx))
    (str "($VariantPM L" _idx ")")

    ($VariantPM (&/$Right _idx))
    (str "($VariantPM R" _idx ")")

    ($SeqPM _left _right)
    (str "($SeqPM " (pattern->text _left) " " (pattern->text _right) ")")

    ($ExecPM _idx)
    (str "($ExecPM " _idx ")")

    ;; $AltPM is not considered because it's not supposed to be
    ;; present anywhere at this point in time.
    ))

;; This function fuses together the paths of the PM traversal, adding
;; branching AltPMs where necessary, and fusing similar paths together
;; as much as possible, when early parts of them coincide.
;; The goal is to minimize rework as much as possible by sharing as
;; much of each path as possible.
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

    [($DegPM _pre-value) ($DegPM _post-value)]
    (if (= _pre-value _post-value)
      ($DegPM _pre-value)
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

(defn ^:private pattern-vars [pattern]
  (|case pattern
    ($BindPM _id)
    (&/|list (&/T [_id false]))

    ($SeqPM _left _right)
    (&/|++ (pattern-vars _left) (pattern-vars _right))
    
    _
    (&/|list)

    ;; $AltPM is not considered because it's not supposed to be
    ;; present anywhere at this point in time.
    ))

(defn ^:private find-unused-vars [var-table body]
  (|let [[meta body-] body]
    (|case body-
      ($var (&/$Local _idx))
      (&/|update _idx (fn [_] true) var-table)

      ($captured _scope _c-idx [_ ($var (&/$Local _idx))])
      (&/|update _idx (fn [_] true) var-table)
      
      ($variant _idx _is-last? _value)
      (find-unused-vars var-table _value)
      
      ($tuple _elems)
      (&/fold find-unused-vars var-table _elems)

      ($ann _value-expr _type-expr)
      (find-unused-vars var-table _value-expr)
      
      ($apply _func _args)
      (&/fold find-unused-vars
              (find-unused-vars var-table _func)
              _args)

      ($proc _proc-ident _args _special-args)
      (&/fold find-unused-vars var-table _args)

      ($loop _register-offset _inits _body)
      (&/|++ (&/fold find-unused-vars var-table _inits)
             (find-unused-vars var-table _body))
      
      ($iter _ _args)
      (&/fold find-unused-vars var-table _args)

      ($let _value _register _body)
      (-> var-table
          (find-unused-vars _value)
          (find-unused-vars _body))

      ($record-get _value _path)
      (find-unused-vars var-table _value)

      ($if _test _then _else)
      (-> var-table
          (find-unused-vars _test)
          (find-unused-vars _then)
          (find-unused-vars _else))
      
      ($case _value [_pm _bodies])
      (&/fold find-unused-vars
              (find-unused-vars var-table _value)
              _bodies)

      ($function _ _ _ _captured _)
      (->> _captured
           (&/|map &/|second)
           (&/fold find-unused-vars var-table))

      _
      var-table
      )))

(defn ^:private clean-unused-pattern-registers [var-table pattern]
  (|case pattern
    ($BindPM _idx)
    (|let [_new-idx (&/|get _idx var-table)]
      (cond (= _idx _new-idx)
            pattern

            (>= _new-idx 0)
            ($BindPM _new-idx)

            :else
            $PopPM))

    ($SeqPM _left _right)
    ($SeqPM (clean-unused-pattern-registers var-table _left)
            (clean-unused-pattern-registers var-table _right))
    
    _
    pattern

    ;; $AltPM is not considered because it's not supposed to be
    ;; present anywhere at this point in time.
    ))

;; This function assumes that the var-table has an ascending index
;; order.
;; For example: (2 3 4 5 6 7 8), instead of (8 7 6 5 4 3 2)
(defn ^:private adjust-register-indexes* [offset var-table]
  (|case var-table
    (&/$Nil)
    (&/|list)

    (&/$Cons [_idx _used?] _tail)
    (if _used?
      (&/$Cons (&/T [_idx (- _idx offset)])
               (adjust-register-indexes* offset _tail))
      (&/$Cons (&/T [_idx -1])
               (adjust-register-indexes* (inc offset) _tail))
      )))

(defn ^:private adjust-register-indexes [var-table]
  (adjust-register-indexes* 0 var-table))

(defn ^:private clean-unused-body-registers [var-table body]
  (|let [[meta body-] body]
    (|case body-
      ($var (&/$Local _idx))
      (|let [new-idx (or (&/|get _idx var-table)
                         _idx)]
        (&/T [meta ($var (&/$Local new-idx))]))
      
      ($captured _scope _c-idx [_sub-meta ($var (&/$Local _idx))])
      (|let [new-idx (or (&/|get _idx var-table)
                         _idx)]
        (&/T [meta ($captured _scope _c-idx (&/T [_sub-meta ($var (&/$Local new-idx))]))]))
      
      ($variant _idx _is-last? _value)
      (&/T [meta ($variant _idx _is-last? (clean-unused-body-registers var-table _value))])
      
      ($tuple _elems)
      (&/T [meta ($tuple (&/|map (partial clean-unused-body-registers var-table)
                                 _elems))])

      ($ann _value-expr _type-expr)
      (&/T [meta ($ann (clean-unused-body-registers var-table _value-expr) _type-expr)])
      
      ($apply _func _args)
      (&/T [meta ($apply (clean-unused-body-registers var-table _func)
                         (&/|map (partial clean-unused-body-registers var-table)
                                 _args))])

      ($proc _proc-ident _args _special-args)
      (&/T [meta ($proc _proc-ident
                        (&/|map (partial clean-unused-body-registers var-table)
                                _args)
                        _special-args)])

      ($loop _register-offset _inits _body)
      (&/T [meta ($loop _register-offset
                        (&/|map (partial clean-unused-body-registers var-table)
                                _inits)
                        (clean-unused-body-registers var-table _body))])
      
      ($iter _iter-register-offset _args)
      (&/T [meta ($iter _iter-register-offset
                        (&/|map (partial clean-unused-body-registers var-table)
                                _args))])

      ($let _value _register _body)
      (&/T [meta ($let (clean-unused-body-registers var-table _value)
                       _register
                       (clean-unused-body-registers var-table _body))])

      ($record-get _value _path)
      (&/T [meta ($record-get (clean-unused-body-registers var-table _value)
                              _path)])

      ($if _test _then _else)
      (&/T [meta ($if (clean-unused-body-registers var-table _test)
                      (clean-unused-body-registers var-table _then)
                      (clean-unused-body-registers var-table _else))])
      
      ($case _value [_pm _bodies])
      (&/T [meta ($case (clean-unused-body-registers var-table _value)
                        (&/T [_pm
                              (&/|map (partial clean-unused-body-registers var-table)
                                      _bodies)]))])

      ($function _register-offset _arity _scope _captured _body)
      (&/T [meta ($function _register-offset
                            _arity
                            _scope
                            (&/|map (fn [capture]
                                      (|let [[_name __var] capture]
                                        (&/T [_name (clean-unused-body-registers var-table __var)])))
                                    _captured)
                            _body)])

      _
      body
      )))

(defn ^:private simplify-pattern [pattern]
  (|case pattern
    ($SeqPM ($TuplePM _idx) ($SeqPM ($PopPM) pattern*))
    (simplify-pattern pattern*)

    ($SeqPM ($TuplePM _idx) _right)
    (|case (simplify-pattern _right)
      ($SeqPM ($PopPM) pattern*)
      pattern*

      _right*
      ($SeqPM ($TuplePM _idx) _right*))

    ($SeqPM _left _right)
    ($SeqPM _left (simplify-pattern _right))

    _
    pattern))

(defn ^:private optimize-register-use [pattern body]
  (|let [p-vars (pattern-vars pattern)
         p-vars* (find-unused-vars p-vars body)
         adjusted-vars (adjust-register-indexes p-vars*)
         clean-pattern (clean-unused-pattern-registers adjusted-vars pattern)
         simple-pattern (simplify-pattern clean-pattern)
         clean-body (clean-unused-body-registers adjusted-vars body)]
    (&/T [simple-pattern clean-body])))

;; This is the top-level function for optimizing PM, which transforms
;; each branch and then fuses them together.
(defn ^:private optimize-pm [branches]
  (|let [;; branches (&/|reverse branches*)
         pms+bodies (&/map2 (fn [branch _body-id]
                              (|let [[_pattern _body] branch]
                                (optimize-register-use (transform-pm _pattern _body-id)
                                                       _body)))
                            branches
                            (&/|range (&/|length branches)))
         pms (&/|map &/|first pms+bodies)
         bodies (&/|map &/|second pms+bodies)]
    (|case (&/|reverse pms)
      (&/$Nil)
      (assert false)

      (&/$Cons _head-pm _tail-pms)
      (&/T [(&/fold fuse-pms _head-pm _tail-pms)
            bodies])
      )))

;; [[Function-Folding Optimization]]

;; The semantics of Lux establish that all functions are of a single
;; argument and the multi-argument functions are actually nested
;; functions being generated and then applied.
;; This, of course, would generate a lot of waste.
;; To avoid it, Lux actually folds function definitions together,
;; thereby creating functions that can be used both
;; one-argument-at-a-time, and also being called with all, or just a
;; partial amount of their arguments.
;; This avoids generating too many artifacts during compilation, since
;; they get "compressed", and it can also lead to faster execution, by
;; enabling optimized function calls later.

;; Functions and captured variables have "scopes", which tell which
;; function they are, or to which function they belong.
;; During the folding, inner functions dissapear, since their bodies
;; are merged into their outer "parent" functions.
;; Their scopes must change accordingy.
(defn ^:private de-scope
  "(-> Scope Scope Scope Scope)"
  [old-scope new-scope scope]
  (if (identical? new-scope scope)
    old-scope
    scope))

;; Also, it must be noted that when folding functions, the indexes of
;; the registers have to be changed accodingly.
;; That is what the following "shifting" functions are for.

;; Shifts the registers for PM operations.
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

;; Shifts the body of a function after a folding is performed.
(defn shift-function-body
  "(-> Scope Scope Bool Optimized Optimized)"
  [old-scope new-scope own-body? body]
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
      
      ($function _register-offset arity scope captured body*)
      (|let [scope* (de-scope old-scope new-scope scope)]
        (&/T [meta ($function _register-offset
                              arity
                              scope*
                              (&/|map (fn [capture]
                                        (|let [[_name [_meta ($captured _scope _idx _source)]] capture]
                                          (&/T [_name (&/T [_meta ($captured scope* _idx (shift-function-body old-scope new-scope own-body? _source))])])))
                                      captured)
                              (shift-function-body old-scope new-scope false body*))]))

      ($ann value-expr type-expr)
      (&/T [meta ($ann (shift-function-body old-scope new-scope own-body? value-expr)
                       type-expr)])
      
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

      ;; This special "apply" rule is for handling recursive calls better.
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

      ($loop _register-offset _inits _body)
      (&/T [meta ($loop (if own-body?
                          (inc _register-offset)
                          _register-offset)
                        (&/|map (partial shift-function-body old-scope new-scope own-body?)
                                _inits)
                        (shift-function-body old-scope new-scope own-body? _body))])
      
      ($iter _iter-register-offset args)
      (&/T [meta ($iter (if own-body?
                          (inc _iter-register-offset)
                          _iter-register-offset)
                        (&/|map (partial shift-function-body old-scope new-scope own-body?) args))])

      ($let _value _register _body)
      (&/T [meta ($let (shift-function-body old-scope new-scope own-body? _value)
                       (if own-body?
                         (inc _register)
                         _register)
                       (shift-function-body old-scope new-scope own-body? _body))])

      ($record-get _value _path)
      (&/T [meta ($record-get (shift-function-body old-scope new-scope own-body? _value)
                              _path)])

      ($if _test _then _else)
      (&/T [meta ($if (shift-function-body old-scope new-scope own-body? _test)
                      (shift-function-body old-scope new-scope own-body? _then)
                      (shift-function-body old-scope new-scope own-body? _else))])
      
      _
      body
      )))

;; [[Record-Manipulation Optimizations]]

;; If a pattern-matching tree with a single branch is found, and that
;; branch corresponds to a tuple PM, and the body corresponds to a
;; local variable, it's likely that the local refers to some member of
;; the tuple that is being extracted.
;; That is the pattern that is to be expected of record read-access,
;; so this function tries to extract the (possibly nested) path
;; necessary, ending in the data-node of the wanted member.
(defn ^:private record-read-path
  "(-> (List PM) Idx (List Idx))"
  [pms member-idx]
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

;; [[Loop Optimizations]]

;; Lux doesn't offer any looping constructs, relying instead on
;; recursion.
;; Some common usages of recursion can be written more efficiently
;; just using regular loops/iteration.
;; This optimization looks for tail-calls in the function body,
;; rewriting them as jumps to the beginning of the function, while
;; they also updated the necessary local variables for the next iteration.
(defn ^:private optimize-iter
  "(-> Int Optimized Optimized)"
  [arity optim]
  (|let [[meta optim-] optim]
    (|case optim-
      ($apply [meta-0 ($var (&/$Local 0))] _args)
      (if (= arity (&/|length _args))
        (&/T [meta ($iter 1 _args)])
        optim)

      ($case _value [_pattern _bodies])
      (&/T [meta ($case _value
                        (&/T [_pattern
                              (&/|map (partial optimize-iter arity)
                                      _bodies)]))])

      ($let _value _register _body)
      (&/T [meta ($let _value _register (optimize-iter arity _body))])

      ($if _test _then _else)
      (&/T [meta ($if _test
                      (optimize-iter arity _then)
                      (optimize-iter arity _else))])
      
      ($ann _value-expr _type-expr)
      (&/T [meta ($ann (optimize-iter arity _value-expr) _type-expr)])

      _
      optim
      )))

(defn ^:private contains-self-reference?
  "(-> Optimized Bool)"
  [body]
  (|let [[meta body-] body
         stepwise-test (fn [base arg] (or base (contains-self-reference? arg)))]
    (|case body-
      ($variant idx is-last? value)
      (contains-self-reference? value)
      
      ($tuple elems)
      (&/fold stepwise-test false elems)
      
      ($case value [_pm _bodies])
      (or (contains-self-reference? value)
          (&/fold stepwise-test false _bodies))

      ($function _ _ _ captured _)
      (->> captured
           (&/|map (fn [capture]
                     (|let [[_name [_meta ($captured _scope _idx _source)]] capture]
                       _source)))
           (&/fold stepwise-test false))

      ($ann value-expr type-expr)
      (contains-self-reference? value-expr)
      
      ($var (&/$Local 0))
      true

      ($apply func args)
      (or (contains-self-reference? func)
          (&/fold stepwise-test false args))
      
      ($proc proc-ident args special-args)
      (&/fold stepwise-test false args)

      ($loop _register-offset _inits _body)
      (or (&/fold stepwise-test false _inits)
          (contains-self-reference? _body))
      
      ($iter _ args)
      (&/fold stepwise-test false args)

      ($let _value _register _body)
      (or (contains-self-reference? _value)
          (contains-self-reference? _body))

      ($record-get _value _path)
      (contains-self-reference? _value)

      ($if _test _then _else)
      (or (contains-self-reference? _test)
          (contains-self-reference? _then)
          (contains-self-reference? _else))
      
      _
      false
      )))

(defn ^:private pm-loop-transform [register-offset direct? pattern]
  (|case pattern
    ($BindPM _var-id)
    ($BindPM (+ register-offset (if direct?
                                  (- _var-id 2)
                                  (- _var-id 1))))

    ($SeqPM _left-pm _right-pm)
    ($SeqPM (pm-loop-transform register-offset direct? _left-pm)
            (pm-loop-transform register-offset direct? _right-pm))

    ($AltPM _left-pm _right-pm)
    ($AltPM (pm-loop-transform register-offset direct? _left-pm)
            (pm-loop-transform register-offset direct? _right-pm))

    _
    pattern
    ))

;; This function must be run STRICTLY before shift-function body, as
;; the transformation assumes that SFB will be invoke after it.
(defn ^:private loop-transform [register-offset direct? body]
  (|let [adjust-direct (fn [register]
                         ;; The register must be decreased once, since
                         ;; it will be re-increased in
                         ;; shift-function-body.
                         ;; The decrease is meant to keep things stable.
                         (if direct?
                           ;; And, if this adjustment is done
                           ;; directly during a loop-transform (and
                           ;; not indirectly if transforming an inner
                           ;; loop), then it must be decreased again
                           ;; because the 0/self var will no longer
                           ;; exist in the loop's context.
                           (- register 2)
                           (- register 1)))
         [meta body-] body]
    (|case body-
      ($variant idx is-last? value)
      (&/T [meta ($variant idx is-last? (loop-transform register-offset direct? value))])
      
      ($tuple elems)
      (&/T [meta ($tuple (&/|map (partial loop-transform register-offset direct?) elems))])
      
      ($case value [_pm _bodies])
      (&/T [meta ($case (loop-transform register-offset direct? value)
                        (&/T [(pm-loop-transform register-offset direct? _pm)
                              (&/|map (partial loop-transform register-offset direct?)
                                      _bodies)]))])

      ;; Functions are ignored because they'll be handled properly at shift-function-body
      
      ($ann value-expr type-expr)
      (&/T [meta ($ann (loop-transform register-offset direct? value-expr)
                       type-expr)])
      
      ($var (&/$Local idx))
      ;; The index must be decreased once, because the var index is
      ;; 1-based (since 0 is reserved for self-reference).
      ;; Then it must be decreased again, since it will be increased
      ;; in the shift-function-body call.
      ;; Then, I add the offset to ensure the var points to the right register.
      (&/T [meta ($var (&/$Local (-> (adjust-direct idx)
                                     (+ register-offset))))])

      ($apply func args)
      (&/T [meta ($apply (loop-transform register-offset direct? func)
                         (&/|map (partial loop-transform register-offset direct?) args))])
      
      ;; Captured-vars are ignored because they'll be handled properly at shift-function-body
      
      ($proc proc-ident args special-args)
      (&/T [meta ($proc proc-ident (&/|map (partial loop-transform register-offset direct?) args) special-args)])

      ($loop _register-offset _inits _body)
      (&/T [meta ($loop (+ register-offset (adjust-direct _register-offset))
                        (&/|map (partial loop-transform register-offset direct?) _inits)
                        (loop-transform register-offset direct? _body))])
      
      ($iter _iter-register-offset args)
      (&/T [meta ($iter (+ register-offset (adjust-direct _iter-register-offset))
                        (&/|map (partial loop-transform register-offset direct?) args))])

      ($let _value _register _body)
      (&/T [meta ($let (loop-transform register-offset direct? _value)
                       (+ register-offset (adjust-direct _register))
                       (loop-transform register-offset direct? _body))])

      ($record-get _value _path)
      (&/T [meta ($record-get (loop-transform register-offset direct? _value)
                              _path)])

      ($if _test _then _else)
      (&/T [meta ($if (loop-transform register-offset direct? _test)
                      (loop-transform register-offset direct? _then)
                      (loop-transform register-offset direct? _else))])
      
      _
      body
      )))

(defn ^:private inline-loop [meta register-offset scope captured args body]
  (->> body
       (loop-transform register-offset true)
       (shift-function-body scope (&/|tail scope) true)
       ($loop register-offset args)
       (list meta)
       (&/T)))

;; [[Initial Optimization]]

;; Before any big optimization can be done, the incoming Analysis nodes
;; must be transformed into Optimized nodes, amenable to further transformations.
;; This function does the job, while also detecting (and optimizing)
;; some simple surface patterns it may encounter.
(let [optimize-closure (fn [optimize closure]
                         (&/|map (fn [capture]
                                   (|let [[_name _analysis] capture]
                                     (&/T [_name (optimize _analysis)])))
                                 closure))]
  (defn ^:private pass-0
    "(-> Bool Analysis Optimized)"
    [top-level-func? analysis]
    (|let [[meta analysis-] analysis]
      (|case analysis-
        (&a/$bool value)
        (&/T [meta ($bool value)])
        
        (&a/$nat value)
        (&/T [meta ($nat value)])

        (&a/$int value)
        (&/T [meta ($int value)])

        (&a/$deg value)
        (&/T [meta ($deg value)])
        
        (&a/$real value)
        (&/T [meta ($real value)])
        
        (&a/$char value)
        (&/T [meta ($char value)])
        
        (&a/$text value)
        (&/T [meta ($text value)])
        
        (&a/$variant idx is-last? value)
        (&/T [meta ($variant idx is-last? (pass-0 top-level-func? value))])
        
        (&a/$tuple elems)
        (&/T [meta ($tuple (&/|map (partial pass-0 top-level-func?) elems))])
        
        (&a/$apply func args)
        (|let [=func (pass-0 top-level-func? func)
               =args (&/|map (partial pass-0 top-level-func?) args)]
          (|case =func
            [_ ($ann [_ ($function _register-offset _arity _scope _captured _body)]
                     _)]
            (if (and (= _arity (&/|length =args))
                     (not (contains-self-reference? _body)))
              (inline-loop meta _register-offset _scope _captured =args _body)
              (&/T [meta ($apply =func =args)]))
            
            _
            (&/T [meta ($apply =func =args)])))
        
        (&a/$case value branches)
        (let [normal-case-optim (fn []
                                  (&/T [meta ($case (pass-0 top-level-func? value)
                                                    (optimize-pm (&/|map (fn [branch]
                                                                           (|let [[_pattern _body] branch]
                                                                             (&/T [_pattern (pass-0 top-level-func? _body)])))
                                                                         branches)))]))]
          (|case branches
            ;; The pattern for a let-expression is a single branch,
            ;; tying the value to a register.
            (&/$Cons [(&a-case/$StoreTestAC _register) _body] (&/$Nil))
            (&/T [meta ($let (pass-0 top-level-func? value) _register (pass-0 top-level-func? _body))])

            (&/$Cons [(&a-case/$BoolTestAC false) _else]
                     (&/$Cons [(&a-case/$BoolTestAC true) _then]
                              (&/$Nil)))
            (&/T [meta ($if (pass-0 top-level-func? value) (pass-0 top-level-func? _then) (pass-0 top-level-func? _else))])
            
            ;; The pattern for a record-get is a single branch, with a
            ;; tuple pattern and a body corresponding to a
            ;; local-variable extracted from the tuple.
            (&/$Cons [(&a-case/$TupleTestAC _sub-tests) [_ (&a/$var (&/$Local _member-idx))]] (&/$Nil))
            (|let [_path (record-read-path _sub-tests _member-idx)]
              (if (&/|empty? _path)
                ;; If the path is empty, that means it was a
                ;; false-positive and normal PM optimization should be
                ;; done instead.
                (normal-case-optim)
                ;; Otherwise, we've got ourselves a record-get expression.
                (&/T [meta ($record-get (pass-0 top-level-func? value) _path)])))

            ;; If no special patterns are found, just do normal PM optimization.
            _
            (normal-case-optim)))
        
        (&a/$lambda _register-offset scope captured body)
        (|let [inner-func? (|case body
                             [_ (&a/$lambda _ _ _ _)]
                             true

                             _
                             false)]
          (|case (pass-0 (not inner-func?) body)
            ;; If the body of a function is another function, that means
            ;; no work was done in-between and both layers can be folded
            ;; into one.
            [_ ($function _ _arity _scope _captured _body)]
            (|let [new-arity (inc _arity)
                   collapsed-body (shift-function-body scope _scope true _body)]
              (&/T [meta ($function _register-offset
                                    new-arity
                                    scope
                                    (optimize-closure (partial pass-0 top-level-func?) captured)
                                    (if top-level-func?
                                      (optimize-iter new-arity collapsed-body)
                                      collapsed-body))]))

            ;; Otherwise, they're nothing to be done and we've got a
            ;; 1-arity function.
            =body
            (&/T [meta ($function _register-offset
                                  1 scope
                                  (optimize-closure (partial pass-0 top-level-func?) captured)
                                  (if top-level-func?
                                    (optimize-iter 1 =body)
                                    =body))])))

        (&a/$ann value-expr type-expr)
        (&/T [meta ($ann (pass-0 top-level-func? value-expr) type-expr)])
        
        (&a/$var var-kind)
        (&/T [meta ($var var-kind)])
        
        (&a/$captured scope idx source)
        (&/T [meta ($captured scope idx (pass-0 top-level-func? source))])

        (&a/$proc proc-ident args special-args)
        (&/T [meta ($proc proc-ident (&/|map (partial pass-0 top-level-func?) args) special-args)])
        
        _
        (assert false (prn-str 'pass-0 top-level-func? (&/adt->text analysis)))
        ))))

;; [Exports]
(defn optimize
  "(-> Analysis Optimized)"
  [analysis]
  (->> analysis
       (pass-0 true)))
