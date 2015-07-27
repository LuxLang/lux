;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns lux.analyser
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return fail return* fail* |list]]
                 [reader :as &reader]
                 [parser :as &parser]
                 [type :as &type]
                 [host :as &host])
            (lux.analyser [base :as &&]
                          [lux :as &&lux]
                          [host :as &&host])))

;; [Utils]
(defn ^:private parse-handler [[catch+ finally+] token]
  (matchv ::M/objects [token]
    [["lux;Meta" [meta ["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_catch"]]]]
                                                 ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ ?ex-class]]]]
                                                              ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ ?ex-arg]]]]
                                                                           ["lux;Cons" [?catch-body
                                                                                        ["lux;Nil" _]]]]]]]]]]]]]
    (&/T (&/|++ catch+ (|list (&/T ?ex-class ?ex-arg ?catch-body))) finally+)

    [["lux;Meta" [meta ["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_finally"]]]]
                                                 ["lux;Cons" [?finally-body
                                                              ["lux;Nil" _]]]]]]]]]
    (&/T catch+ ?finally-body)))

(defn ^:private aba7 [analyse eval! compile-module exo-type token]
  (matchv ::M/objects [token]
    ;; Arrays
    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_new-array"]]]]
                               ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ ?class]]]]
                                            ["lux;Cons" [["lux;Meta" [_ ["lux;IntS" ?length]]]
                                                         ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-new-array analyse ?class ?length)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_aastore"]]]]
                               ["lux;Cons" [?array
                                            ["lux;Cons" [["lux;Meta" [_ ["lux;IntS" ?idx]]]
                                                         ["lux;Cons" [?elem
                                                                      ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-aastore analyse ?array ?idx ?elem)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_aaload"]]]]
                               ["lux;Cons" [?array
                                            ["lux;Cons" [["lux;Meta" [_ ["lux;IntS" ?idx]]]
                                                         ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-aaload analyse ?array ?idx)

    ;; Classes & interfaces
    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_class"]]]]
                               ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?name]]]
                                            ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?super-class]]]
                                                         ["lux;Cons" [["lux;Meta" [_ ["lux;TupleS" ?interfaces]]]
                                                                      ["lux;Cons" [["lux;Meta" [_ ["lux;TupleS" ?fields]]]
                                                                                   ?methods]]]]]]]]]]]]
    (&&host/analyse-jvm-class analyse ?name ?super-class ?interfaces ?fields ?methods)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_interface"]]]]
                               ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?name]]]
                                            ["lux;Cons" [["lux;Meta" [_ ["lux;TupleS" ?supers]]]
                                                         ?methods]]]]]]]]
    (&&host/analyse-jvm-interface analyse ?name ?supers ?methods)

    ;; Programs
    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_program"]]]]
                               ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" ?args]]]
                                            ["lux;Cons" [?body
                                                         ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-program analyse ?args ?body)
    
    [_]
    (fail "")))

(defn ^:private aba6 [analyse eval! compile-module exo-type token]
  (matchv ::M/objects [token]
    ;; Primitive conversions
    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_d2f"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]
    (&&host/analyse-jvm-d2f analyse ?value)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_d2i"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]
    (&&host/analyse-jvm-d2i analyse ?value)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_d2l"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]
    (&&host/analyse-jvm-d2l analyse ?value)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_f2d"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]
    (&&host/analyse-jvm-f2d analyse ?value)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_f2i"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]
    (&&host/analyse-jvm-f2i analyse ?value)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_f2l"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]
    (&&host/analyse-jvm-f2l analyse ?value)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_i2b"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]
    (&&host/analyse-jvm-i2b analyse ?value)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_i2c"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]
    (&&host/analyse-jvm-i2c analyse ?value)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_i2d"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]
    (&&host/analyse-jvm-i2d analyse ?value)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_i2f"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]
    (&&host/analyse-jvm-i2f analyse ?value)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_i2l"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]
    (&&host/analyse-jvm-i2l analyse ?value)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_i2s"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]
    (&&host/analyse-jvm-i2s analyse ?value)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_l2d"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]
    (&&host/analyse-jvm-l2d analyse ?value)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_l2f"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]
    (&&host/analyse-jvm-l2f analyse ?value)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_l2i"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]
    (&&host/analyse-jvm-l2i analyse ?value)

    ;; Bitwise operators
    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_iand"]]]] ["lux;Cons" [?x ["lux;Cons" [?y ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-iand analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_ior"]]]] ["lux;Cons" [?x ["lux;Cons" [?y ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ior analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_land"]]]] ["lux;Cons" [?x ["lux;Cons" [?y ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-land analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_lor"]]]] ["lux;Cons" [?x ["lux;Cons" [?y ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lor analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_lxor"]]]] ["lux;Cons" [?x ["lux;Cons" [?y ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lxor analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_lshl"]]]] ["lux;Cons" [?x ["lux;Cons" [?y ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lshl analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_lshr"]]]] ["lux;Cons" [?x ["lux;Cons" [?y ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lshr analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_lushr"]]]] ["lux;Cons" [?x ["lux;Cons" [?y ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lushr analyse ?x ?y)

    [_]
    (aba7 analyse eval! compile-module exo-type token)))

(defn ^:private aba5 [analyse eval! compile-module exo-type token]
  (matchv ::M/objects [token]
    ;; Objects
    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_null?"]]]]
                               ["lux;Cons" [?object
                                            ["lux;Nil" _]]]]]]]
    (&&host/analyse-jvm-null? analyse ?object)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_instanceof"]]]]
                               ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?class]]]
                                            ["lux;Cons" [?object
                                                         ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-instanceof analyse ?class ?object)
    
    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_new"]]]]
                               ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?class]]]
                                            ["lux;Cons" [["lux;Meta" [_ ["lux;TupleS" ?classes]]]
                                                         ["lux;Cons" [["lux;Meta" [_ ["lux;TupleS" ?args]]]
                                                                      ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-new analyse ?class ?classes ?args)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_getstatic"]]]]
                               ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?class]]]
                                            ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?field]]]
                                                         ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-getstatic analyse ?class ?field)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_getfield"]]]]
                               ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?class]]]
                                            ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?field]]]
                                                         ["lux;Cons" [?object
                                                                      ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-getfield analyse ?class ?field ?object)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_putstatic"]]]]
                               ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?class]]]
                                            ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?field]]]
                                                         ["lux;Cons" [?value
                                                                      ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-putstatic analyse ?class ?field ?value)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_putfield"]]]]
                               ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?class]]]
                                            ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?field]]]
                                                         ["lux;Cons" [?object
                                                                      ["lux;Cons" [?value
                                                                                   ["lux;Nil" _]]]]]]]]]]]]]
    (&&host/analyse-jvm-putfield analyse ?class ?field ?object ?value)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_invokestatic"]]]]
                               ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?class]]]
                                            ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?method]]]
                                                         ["lux;Cons" [["lux;Meta" [_ ["lux;TupleS" ?classes]]]
                                                                      ["lux;Cons" [["lux;Meta" [_ ["lux;TupleS" ?args]]]
                                                                                   ["lux;Nil" _]]]]]]]]]]]]]
    (&&host/analyse-jvm-invokestatic analyse ?class ?method ?classes ?args)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_invokevirtual"]]]]
                               ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?class]]]
                                            ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?method]]]
                                                         ["lux;Cons" [["lux;Meta" [_ ["lux;TupleS" ?classes]]]
                                                                      ["lux;Cons" [?object
                                                                                   ["lux;Cons" [["lux;Meta" [_ ["lux;TupleS" ?args]]]
                                                                                                ["lux;Nil" _]]]]]]]]]]]]]]]
    (&&host/analyse-jvm-invokevirtual analyse ?class ?method ?classes ?object ?args)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_invokeinterface"]]]]
                               ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?class]]]
                                            ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?method]]]
                                                         ["lux;Cons" [["lux;Meta" [_ ["lux;TupleS" ?classes]]]
                                                                      ["lux;Cons" [?object
                                                                                   ["lux;Cons" [["lux;Meta" [_ ["lux;TupleS" ?args]]]
                                                                                                ["lux;Nil" _]]]]]]]]]]]]]]]
    (&&host/analyse-jvm-invokeinterface analyse ?class ?method ?classes ?object ?args)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_invokespecial"]]]]
                               ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?class]]]
                                            ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?method]]]
                                                         ["lux;Cons" [["lux;Meta" [_ ["lux;TupleS" ?classes]]]
                                                                      ["lux;Cons" [?object
                                                                                   ["lux;Cons" [["lux;Meta" [_ ["lux;TupleS" ?args]]]
                                                                                                ["lux;Nil" _]]]]]]]]]]]]]]]
    (&&host/analyse-jvm-invokespecial analyse ?class ?method ?classes ?object ?args)
    
    ;; Exceptions
    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_try"]]]]
                               ["lux;Cons" [?body
                                            ?handlers]]]]]]
    (&&host/analyse-jvm-try analyse ?body (&/fold parse-handler [(list) nil] ?handlers))

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_throw"]]]]
                               ["lux;Cons" [?ex
                                            ["lux;Nil" _]]]]]]]
    (&&host/analyse-jvm-throw analyse ?ex)

    ;; Syncronization/monitos
    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_monitorenter"]]]]
                               ["lux;Cons" [?monitor
                                            ["lux;Nil" _]]]]]]]
    (&&host/analyse-jvm-monitorenter analyse ?monitor)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_monitorexit"]]]]
                               ["lux;Cons" [?monitor
                                            ["lux;Nil" _]]]]]]]
    (&&host/analyse-jvm-monitorexit analyse ?monitor)

    [_]
    (aba6 analyse eval! compile-module exo-type token)))

(defn ^:private aba4 [analyse eval! compile-module exo-type token]
  (matchv ::M/objects [token]
    ;; Float arithmetic
    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_fadd"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-fadd analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_fsub"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-fsub analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_fmul"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-fmul analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_fdiv"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-fdiv analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_frem"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-frem analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_feq"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-feq analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_flt"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-flt analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_fgt"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-fgt analyse ?x ?y)

    ;; Double arithmetic
    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_dadd"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-dadd analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_dsub"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-dsub analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_dmul"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-dmul analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_ddiv"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ddiv analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_drem"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-drem analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_deq"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-deq analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_dlt"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-dlt analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_dgt"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-dgt analyse ?x ?y)

    [_]
    (aba5 analyse eval! compile-module exo-type token)))

(defn ^:private aba3 [analyse eval! compile-module exo-type token]
  (matchv ::M/objects [token]
    ;; Host special forms
    ;; Characters
    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_ceq"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ceq analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_clt"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-clt analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_cgt"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-cgt analyse ?x ?y)
    
    ;; Integer arithmetic
    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_iadd"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-iadd analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_isub"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-isub analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_imul"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-imul analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_idiv"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-idiv analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_irem"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-irem analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_ieq"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ieq analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_ilt"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ilt analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_igt"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-igt analyse ?x ?y)

    ;; Long arithmetic
    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_ladd"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ladd analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_lsub"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lsub analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_lmul"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lmul analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_ldiv"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ldiv analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_lrem"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lrem analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_leq"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-leq analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_llt"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-llt analyse ?x ?y)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_jvm_lgt"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lgt analyse ?x ?y)

    [_]
    (aba4 analyse eval! compile-module exo-type token)))

(defn ^:private aba2 [analyse eval! compile-module exo-type token]
  (matchv ::M/objects [token]
    [["lux;SymbolS" ?ident]]
    (&&lux/analyse-symbol analyse exo-type ?ident)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_lux_case"]]]]
                               ["lux;Cons" [?value ?branches]]]]]]
    (&&lux/analyse-case analyse exo-type ?value ?branches)
    
    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_lux_lambda"]]]]
                               ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" ?self]]]
                                            ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" ?arg]]]
                                                         ["lux;Cons" [?body
                                                                      ["lux;Nil" _]]]]]]]]]]]
    (&&lux/analyse-lambda analyse exo-type ?self ?arg ?body)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_lux_def"]]]]
                               ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" ["" ?name]]]]
                                            ["lux;Cons" [?value
                                                         ["lux;Nil" _]]]]]]]]]
    (&&lux/analyse-def analyse ?name ?value)
    
    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_lux_declare-macro"]]]]
                               ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" ["" ?name]]]]
                                            ["lux;Nil" _]]]]]]]
    (&&lux/analyse-declare-macro analyse ?name)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_lux_import"]]]]
                               ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?path]]]
                                            ["lux;Nil" _]]]]]]]
    (&&lux/analyse-import analyse compile-module ?path)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_lux_:"]]]]
                               ["lux;Cons" [?type
                                            ["lux;Cons" [?value
                                                         ["lux;Nil" _]]]]]]]]]
    (&&lux/analyse-check analyse eval! exo-type ?type ?value)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_lux_:!"]]]]
                               ["lux;Cons" [?type
                                            ["lux;Cons" [?value
                                                         ["lux;Nil" _]]]]]]]]]
    (&&lux/analyse-coerce analyse eval! exo-type ?type ?value)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_lux_export"]]]]
                               ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" ["" ?ident]]]]
                                            ["lux;Nil" _]]]]]]]
    (&&lux/analyse-export analyse ?ident)

    [["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;SymbolS" [_ "_lux_alias"]]]]
                               ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?alias]]]
                                            ["lux;Cons" [["lux;Meta" [_ ["lux;TextS" ?module]]]
                                                         ["lux;Nil" _]]]]]]]]]
    (&&lux/analyse-alias analyse ?alias ?module)
    
    [_]
    (aba3 analyse eval! compile-module exo-type token)))

(let [unit (&/V "lux;Meta" (&/T (&/T "" -1 -1) (&/V "lux;TupleS" (|list))))]
  (defn ^:private aba1 [analyse eval! compile-module exo-type token]
    (matchv ::M/objects [token]
      ;; Standard special forms
      [["lux;BoolS" ?value]]
      (|do [_ (&type/check exo-type &type/Bool)]
        (return (&/|list (&/T (&/V "bool" ?value) exo-type))))

      [["lux;IntS" ?value]]
      (|do [_ (&type/check exo-type &type/Int)]
        (return (&/|list (&/T (&/V "int" ?value) exo-type))))

      [["lux;RealS" ?value]]
      (|do [_ (&type/check exo-type &type/Real)]
        (return (&/|list (&/T (&/V "real" ?value) exo-type))))

      [["lux;CharS" ?value]]
      (|do [_ (&type/check exo-type &type/Char)]
        (return (&/|list (&/T (&/V "char" ?value) exo-type))))

      [["lux;TextS" ?value]]
      (|do [_ (&type/check exo-type &type/Text)]
        (return (&/|list (&/T (&/V "text" ?value) exo-type))))

      [["lux;TupleS" ?elems]]
      (&&lux/analyse-tuple analyse exo-type ?elems)

      [["lux;RecordS" ?elems]]
      (&&lux/analyse-record analyse exo-type ?elems)

      [["lux;TagS" ?ident]]
      (&&lux/analyse-variant analyse exo-type ?ident unit)
      
      [["lux;SymbolS" [_ "_jvm_null"]]]
      (return (&/|list (&/T (&/V "jvm-null" nil) (&/V "lux;DataT" "null"))))

      [_]
      (aba2 analyse eval! compile-module exo-type token)
      )))

(defn ^:private add-loc [meta ^String msg]
  (if (.startsWith msg "@")
    msg
    (|let [[file line col] meta]
      (str "@ " file "," line "," col "\n" msg))))

(defn ^:private analyse-basic-ast [analyse eval! compile-module exo-type token]
  ;; (prn 'analyse-basic-ast (&/show-ast token))
  (matchv ::M/objects [token]
    [["lux;Meta" [meta ?token]]]
    (fn [state]
      (matchv ::M/objects [((aba1 analyse eval! compile-module exo-type ?token) state)]
        [["lux;Right" [state* output]]]
        (return* state* output)

        [["lux;Left" ""]]
        (fail* (add-loc meta (str "[Analyser Error] Unrecognized token: " (&/show-ast token))))

        [["lux;Left" msg]]
        (fail* (add-loc meta msg))
        ))))

(defn ^:private just-analyse [analyse-ast eval! compile-module syntax]
  (&type/with-var
    (fn [?var]
      (|do [[?output-term ?output-type] (&&/analyse-1 (partial analyse-ast eval! compile-module) ?var syntax)]
        (matchv ::M/objects [?var ?output-type]
          [["lux;VarT" ?e-id] ["lux;VarT" ?a-id]]
          (if (= ?e-id ?a-id)
            (|do [?output-type* (&type/deref ?e-id)]
              (return (&/T ?output-term ?output-type*)))
            (return (&/T ?output-term ?output-type)))

          [_ _]
          (return (&/T ?output-term ?output-type)))
        ))))

(defn ^:private analyse-ast [eval! compile-module exo-type token]
  (matchv ::M/objects [token]
    [["lux;Meta" [meta ["lux;FormS" ["lux;Cons" [["lux;Meta" [_ ["lux;TagS" ?ident]]] ?values]]]]]]
    (do (assert (.equals ^Object (&/|length ?values) 1) "[Analyser Error] Can only tag 1 value.")
      (&&lux/analyse-variant (partial analyse-ast eval! compile-module) exo-type ?ident (&/|head ?values)))
    
    [["lux;Meta" [meta ["lux;FormS" ["lux;Cons" [?fn ?args]]]]]]
    (fn [state]
      (matchv ::M/objects [((just-analyse analyse-ast eval! compile-module ?fn) state)
                           ;; ((&type/with-var #(&&/analyse-1 (partial analyse-ast eval! compile-module) % ?fn)) state)
                           ]
        [["lux;Right" [state* =fn]]]
        (do ;; (prn 'GOT_FUN (&/show-ast ?fn) (&/show-ast token) (aget =fn 0 0) (aget =fn 1 0))
            ((&&lux/analyse-apply (partial analyse-ast eval! compile-module) exo-type meta =fn ?args) state*))

        [_]
        ((analyse-basic-ast (partial analyse-ast eval! compile-module) eval! compile-module exo-type token) state)))
    
    [_]
    (analyse-basic-ast (partial analyse-ast eval! compile-module) eval! compile-module exo-type token)))

;; [Resources]
(defn analyse [eval! compile-module]
  (|do [asts &parser/parse]
    (&/flat-map% (partial analyse-ast eval! compile-module &type/$Void) asts)))
