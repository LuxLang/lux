(ns lux.optimizer
  (:require [lux.analyser :as &analyser]))

;; [List of pending optimizations]
;; Global functions: direct currying or direct invocation when function is known at compile-time
;; Improving function calls: add extra arities (apply2, apply3, ..., apply16)
;; Recursion: tail-call optimization
;; Pattern-matching: decision-trees to avoid unnecessary tests
;; Less classes generated per function: Fold nested function classes into one.
;; Mutability for performance: do escape analysis to know when data-structures can be mutated in-place without anybody noticing.
;; Avoid (un)boxing: Analyser movement of primitive values to/from functions to known when (un)boxing can be avoided.
;; Pre-compute constant expressions: Find function calls for which all arguments are known at compile-time and pre-calculate everything prior to compilation.
;; Convert pattern-matching on booleans into regular if-then-else structures

;; [Exports]
(def optimize &analyser/analyse)
