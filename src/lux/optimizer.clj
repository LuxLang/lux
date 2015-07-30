;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

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
;; Local var aliasing.

;; [Exports]
(defn optimize [eval! compile-module]
  (&analyser/analyse eval! compile-module))
