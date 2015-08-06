;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns lux.analyser
  (:require (clojure [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return fail return* fail* |case]]
                 [reader :as &reader]
                 [parser :as &parser]
                 [type :as &type]
                 [host :as &host])
            (lux.analyser [base :as &&]
                          [lux :as &&lux]
                          [host :as &&host])))

;; [Utils]
(defn ^:private parse-handler [[catch+ finally+] token]
  (|case token
    ("lux;Meta" meta ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_catch"))
                                   ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?ex-class))
                                    ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" "" ?ex-arg))
                                     ("lux;Cons" ?catch-body
                                      ("lux;Nil")))))))
    (return (&/T (&/|++ catch+ (&/|list (&/T ?ex-class ?ex-arg ?catch-body))) finally+))

    ("lux;Meta" meta ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_finally"))
                                   ("lux;Cons" ?finally-body
                                    ("lux;Nil")))))
    (return (&/T catch+ (&/V "lux;Some" ?finally-body)))

    _
    (fail (str "[Analyser Error] Wrong syntax for exception handler: " (&/show-ast token)))))

(defn ^:private aba7 [analyse eval! compile-module compile-token exo-type token]
  (|case token
    ;; Arrays
    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_new-array"))
                  ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ ?class))
                   ("lux;Cons" ("lux;Meta" _ ("lux;IntS" ?length))
                    ("lux;Nil")))))
    (&&host/analyse-jvm-new-array analyse ?class ?length)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_aastore"))
                  ("lux;Cons" ?array
                   ("lux;Cons" ("lux;Meta" _ ("lux;IntS" ?idx))
                    ("lux;Cons" ?elem
                     ("lux;Nil"))))))
    (&&host/analyse-jvm-aastore analyse ?array ?idx ?elem)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_aaload"))
                  ("lux;Cons" ?array
                   ("lux;Cons" ("lux;Meta" _ ("lux;IntS" ?idx))
                    ("lux;Nil")))))
    (&&host/analyse-jvm-aaload analyse ?array ?idx)

    ;; Classes & interfaces
    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_class"))
                  ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?name))
                   ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?super-class))
                    ("lux;Cons" ("lux;Meta" _ ("lux;TupleS" ?interfaces))
                     ("lux;Cons" ("lux;Meta" _ ("lux;TupleS" ?fields))
                      ("lux;Cons" ("lux;Meta" _ ("lux;TupleS" ?methods))
                       ("lux;Nil"))))))))
    (&&host/analyse-jvm-class analyse compile-token ?name ?super-class ?interfaces ?fields ?methods)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_interface"))
                  ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?name))
                   ("lux;Cons" ("lux;Meta" _ ("lux;TupleS" ?supers))
                    ?methods))))
    (&&host/analyse-jvm-interface analyse compile-token ?name ?supers ?methods)

    ;; Programs
    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_program"))
                  ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" "" ?args))
                   ("lux;Cons" ?body
                    ("lux;Nil")))))
    (&&host/analyse-jvm-program analyse compile-token ?args ?body)
    
    _
    (fail "")))

(defn ^:private aba6 [analyse eval! compile-module compile-token exo-type token]
  (|case token
    ;; Primitive conversions
    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_d2f")) ("lux;Cons" ?value ("lux;Nil"))))
    (&&host/analyse-jvm-d2f analyse exo-type ?value)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_d2i")) ("lux;Cons" ?value ("lux;Nil"))))
    (&&host/analyse-jvm-d2i analyse exo-type ?value)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_d2l")) ("lux;Cons" ?value ("lux;Nil"))))
    (&&host/analyse-jvm-d2l analyse exo-type ?value)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_f2d")) ("lux;Cons" ?value ("lux;Nil"))))
    (&&host/analyse-jvm-f2d analyse exo-type ?value)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_f2i")) ("lux;Cons" ?value ("lux;Nil"))))
    (&&host/analyse-jvm-f2i analyse exo-type ?value)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_f2l")) ("lux;Cons" ?value ("lux;Nil"))))
    (&&host/analyse-jvm-f2l analyse exo-type ?value)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_i2b")) ("lux;Cons" ?value ("lux;Nil"))))
    (&&host/analyse-jvm-i2b analyse exo-type ?value)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_i2c")) ("lux;Cons" ?value ("lux;Nil"))))
    (&&host/analyse-jvm-i2c analyse exo-type ?value)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_i2d")) ("lux;Cons" ?value ("lux;Nil"))))
    (&&host/analyse-jvm-i2d analyse exo-type ?value)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_i2f")) ("lux;Cons" ?value ("lux;Nil"))))
    (&&host/analyse-jvm-i2f analyse exo-type ?value)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_i2l")) ("lux;Cons" ?value ("lux;Nil"))))
    (&&host/analyse-jvm-i2l analyse exo-type ?value)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_i2s")) ("lux;Cons" ?value ("lux;Nil"))))
    (&&host/analyse-jvm-i2s analyse exo-type ?value)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_l2d")) ("lux;Cons" ?value ("lux;Nil"))))
    (&&host/analyse-jvm-l2d analyse exo-type ?value)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_l2f")) ("lux;Cons" ?value ("lux;Nil"))))
    (&&host/analyse-jvm-l2f analyse exo-type ?value)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_l2i")) ("lux;Cons" ?value ("lux;Nil"))))
    (&&host/analyse-jvm-l2i analyse exo-type ?value)

    ;; Bitwise operators
    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_iand")) ("lux;Cons" ?x ("lux;Cons" ?y ("lux;Nil")))))
    (&&host/analyse-jvm-iand analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_ior")) ("lux;Cons" ?x ("lux;Cons" ?y ("lux;Nil")))))
    (&&host/analyse-jvm-ior analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_ixor")) ("lux;Cons" ?x ("lux;Cons" ?y ("lux;Nil")))))
    (&&host/analyse-jvm-ixor analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_ishl")) ("lux;Cons" ?x ("lux;Cons" ?y ("lux;Nil")))))
    (&&host/analyse-jvm-ishl analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_ishr")) ("lux;Cons" ?x ("lux;Cons" ?y ("lux;Nil")))))
    (&&host/analyse-jvm-ishr analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_iushr")) ("lux;Cons" ?x ("lux;Cons" ?y ("lux;Nil")))))
    (&&host/analyse-jvm-iushr analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_land")) ("lux;Cons" ?x ("lux;Cons" ?y ("lux;Nil")))))
    (&&host/analyse-jvm-land analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_lor")) ("lux;Cons" ?x ("lux;Cons" ?y ("lux;Nil")))))
    (&&host/analyse-jvm-lor analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_lxor")) ("lux;Cons" ?x ("lux;Cons" ?y ("lux;Nil")))))
    (&&host/analyse-jvm-lxor analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_lshl")) ("lux;Cons" ?x ("lux;Cons" ?y ("lux;Nil")))))
    (&&host/analyse-jvm-lshl analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_lshr")) ("lux;Cons" ?x ("lux;Cons" ?y ("lux;Nil")))))
    (&&host/analyse-jvm-lshr analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_lushr")) ("lux;Cons" ?x ("lux;Cons" ?y ("lux;Nil")))))
    (&&host/analyse-jvm-lushr analyse exo-type ?x ?y)

    _
    (aba7 analyse eval! compile-module compile-token exo-type token)))

(defn ^:private aba5 [analyse eval! compile-module compile-token exo-type token]
  (|case token
    ;; Objects
    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_null?"))
                  ("lux;Cons" ?object
                   ("lux;Nil"))))
    (&&host/analyse-jvm-null? analyse exo-type ?object)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_instanceof"))
                  ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?class))
                   ("lux;Cons" ?object
                    ("lux;Nil")))))
    (&&host/analyse-jvm-instanceof analyse exo-type ?class ?object)
    
    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_new"))
                  ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?class))
                   ("lux;Cons" ("lux;Meta" _ ("lux;TupleS" ?classes))
                    ("lux;Cons" ("lux;Meta" _ ("lux;TupleS" ?args))
                     ("lux;Nil"))))))
    (&&host/analyse-jvm-new analyse exo-type ?class ?classes ?args)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_getstatic"))
                  ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?class))
                   ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?field))
                    ("lux;Nil")))))
    (&&host/analyse-jvm-getstatic analyse exo-type ?class ?field)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_getfield"))
                  ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?class))
                   ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?field))
                    ("lux;Cons" ?object
                     ("lux;Nil"))))))
    (&&host/analyse-jvm-getfield analyse exo-type ?class ?field ?object)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_putstatic"))
                  ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?class))
                   ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?field))
                    ("lux;Cons" ?value
                     ("lux;Nil"))))))
    (&&host/analyse-jvm-putstatic analyse exo-type ?class ?field ?value)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_putfield"))
                  ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?class))
                   ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?field))
                    ("lux;Cons" ?object
                     ("lux;Cons" ?value
                      ("lux;Nil")))))))
    (&&host/analyse-jvm-putfield analyse exo-type ?class ?field ?object ?value)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_invokestatic"))
                  ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?class))
                   ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?method))
                    ("lux;Cons" ("lux;Meta" _ ("lux;TupleS" ?classes))
                     ("lux;Cons" ("lux;Meta" _ ("lux;TupleS" ?args))
                      ("lux;Nil")))))))
    (&&host/analyse-jvm-invokestatic analyse exo-type ?class ?method ?classes ?args)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_invokevirtual"))
                  ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?class))
                   ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?method))
                    ("lux;Cons" ("lux;Meta" _ ("lux;TupleS" ?classes))
                     ("lux;Cons" ?object
                      ("lux;Cons" ("lux;Meta" _ ("lux;TupleS" ?args))
                       ("lux;Nil"))))))))
    (&&host/analyse-jvm-invokevirtual analyse exo-type ?class ?method ?classes ?object ?args)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_invokeinterface"))
                  ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?class))
                   ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?method))
                    ("lux;Cons" ("lux;Meta" _ ("lux;TupleS" ?classes))
                     ("lux;Cons" ?object
                      ("lux;Cons" ("lux;Meta" _ ("lux;TupleS" ?args))
                       ("lux;Nil"))))))))
    (&&host/analyse-jvm-invokeinterface analyse exo-type ?class ?method ?classes ?object ?args)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_invokespecial"))
                  ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?class))
                   ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?method))
                    ("lux;Cons" ("lux;Meta" _ ("lux;TupleS" ?classes))
                     ("lux;Cons" ?object
                      ("lux;Cons" ("lux;Meta" _ ("lux;TupleS" ?args))
                       ("lux;Nil"))))))))
    (&&host/analyse-jvm-invokespecial analyse exo-type ?class ?method ?classes ?object ?args)

    ;; Exceptions
    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_try"))
                  ("lux;Cons" ?body
                   ?handlers)))
    (|do [catches+finally (&/fold% parse-handler (&/T (&/|list) (&/V "lux;None" nil)) ?handlers)]
      (&&host/analyse-jvm-try analyse exo-type ?body catches+finally))

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_throw"))
                  ("lux;Cons" ?ex
                   ("lux;Nil"))))
    (&&host/analyse-jvm-throw analyse exo-type ?ex)

    ;; Syncronization/monitos
    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_monitorenter"))
                  ("lux;Cons" ?monitor
                   ("lux;Nil"))))
    (&&host/analyse-jvm-monitorenter analyse exo-type ?monitor)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_monitorexit"))
                  ("lux;Cons" ?monitor
                   ("lux;Nil"))))
    (&&host/analyse-jvm-monitorexit analyse exo-type ?monitor)

    _
    (aba6 analyse eval! compile-module compile-token exo-type token)))

(defn ^:private aba4 [analyse eval! compile-module compile-token exo-type token]
  (|case token
    ;; Float arithmetic
    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_fadd")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-fadd analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_fsub")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-fsub analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_fmul")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-fmul analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_fdiv")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-fdiv analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_frem")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-frem analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_feq")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-feq analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_flt")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-flt analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_fgt")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-fgt analyse exo-type ?x ?y)

    ;; Double arithmetic
    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_dadd")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-dadd analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_dsub")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-dsub analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_dmul")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-dmul analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_ddiv")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-ddiv analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_drem")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-drem analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_deq")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-deq analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_dlt")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-dlt analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_dgt")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-dgt analyse exo-type ?x ?y)
    
    _
    (aba5 analyse eval! compile-module compile-token exo-type token)))

(defn ^:private aba3 [analyse eval! compile-module compile-token exo-type token]
  (|case token
    ;; Host special forms
    ;; Characters
    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_ceq")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-ceq analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_clt")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-clt analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_cgt")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-cgt analyse exo-type ?x ?y)
    
    ;; Integer arithmetic
    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_iadd")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-iadd analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_isub")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-isub analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_imul")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-imul analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_idiv")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-idiv analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_irem")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-irem analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_ieq")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-ieq analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_ilt")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-ilt analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_igt")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-igt analyse exo-type ?x ?y)

    ;; Long arithmetic
    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_ladd")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-ladd analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_lsub")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-lsub analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_lmul")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-lmul analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_ldiv")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-ldiv analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_lrem")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-lrem analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_leq")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-leq analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_llt")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-llt analyse exo-type ?x ?y)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_jvm_lgt")) ("lux;Cons" ?y ("lux;Cons" ?x ("lux;Nil")))))
    (&&host/analyse-jvm-lgt analyse exo-type ?x ?y)

    _
    (aba4 analyse eval! compile-module compile-token exo-type token)))

(defn ^:private aba2 [analyse eval! compile-module compile-token exo-type token]
  (|case token
    ("lux;SymbolS" ?ident)
    (&&lux/analyse-symbol analyse exo-type ?ident)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_lux_case"))
                  ("lux;Cons" ?value ?branches)))
    (&&lux/analyse-case analyse exo-type ?value ?branches)
    
    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_lux_lambda"))
                  ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" "" ?self))
                   ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" "" ?arg))
                    ("lux;Cons" ?body
                     ("lux;Nil"))))))
    (&&lux/analyse-lambda analyse exo-type ?self ?arg ?body)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_lux_def"))
                  ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" "" ?name))
                   ("lux;Cons" ?value
                    ("lux;Nil")))))
    (&&lux/analyse-def analyse compile-token ?name ?value)
    
    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_lux_declare-macro"))
                  ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" "" ?name))
                   ("lux;Nil"))))
    (&&lux/analyse-declare-macro analyse compile-token ?name)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_lux_import"))
                  ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?path))
                   ("lux;Nil"))))
    (&&lux/analyse-import analyse compile-module compile-token ?path)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_lux_:"))
                  ("lux;Cons" ?type
                   ("lux;Cons" ?value
                    ("lux;Nil")))))
    (&&lux/analyse-check analyse eval! exo-type ?type ?value)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_lux_:!"))
                  ("lux;Cons" ?type
                   ("lux;Cons" ?value
                    ("lux;Nil")))))
    (&&lux/analyse-coerce analyse eval! exo-type ?type ?value)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_lux_export"))
                  ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" "" ?ident))
                   ("lux;Nil"))))
    (&&lux/analyse-export analyse compile-token ?ident)

    ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;SymbolS" _ "_lux_alias"))
                  ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?alias))
                   ("lux;Cons" ("lux;Meta" _ ("lux;TextS" ?module))
                    ("lux;Nil")))))
    (&&lux/analyse-alias analyse compile-token ?alias ?module)
    
    _
    (aba3 analyse eval! compile-module compile-token exo-type token)))

(defn ^:private aba1 [analyse eval! compile-module compile-token exo-type token]
  (|case token
    ;; Standard special forms
    ("lux;BoolS" ?value)
    (|do [_ (&type/check exo-type &type/Bool)]
      (return (&/|list (&/T (&/V "bool" ?value) exo-type))))

    ("lux;IntS" ?value)
    (|do [_ (&type/check exo-type &type/Int)]
      (return (&/|list (&/T (&/V "int" ?value) exo-type))))

    ("lux;RealS" ?value)
    (|do [_ (&type/check exo-type &type/Real)]
      (return (&/|list (&/T (&/V "real" ?value) exo-type))))

    ("lux;CharS" ?value)
    (|do [_ (&type/check exo-type &type/Char)]
      (return (&/|list (&/T (&/V "char" ?value) exo-type))))

    ("lux;TextS" ?value)
    (|do [_ (&type/check exo-type &type/Text)]
      (return (&/|list (&/T (&/V "text" ?value) exo-type))))

    ("lux;TupleS" ?elems)
    (&&lux/analyse-tuple analyse exo-type ?elems)

    ("lux;RecordS" ?elems)
    (&&lux/analyse-record analyse exo-type ?elems)

    ("lux;TagS" ?ident)
    (&&lux/analyse-variant analyse exo-type ?ident (&/|list))
    
    ("lux;SymbolS" _ "_jvm_null")
    (&&host/analyse-jvm-null analyse exo-type)

    _
    (aba2 analyse eval! compile-module compile-token exo-type token)
    ))

(defn ^:private add-loc [meta ^String msg]
  (if (.startsWith msg "@")
    msg
    (|let [[file line col] meta]
      (str "@ " file "," line "," col "\n" msg))))

(defn ^:private analyse-basic-ast [analyse eval! compile-module compile-token exo-type token]
  ;; (prn 'analyse-basic-ast (&/show-ast token))
  (|case token
    ("lux;Meta" meta ?token)
    (fn [state]
      (|case ((aba1 analyse eval! compile-module compile-token exo-type ?token) state)
        ("lux;Right" state* output)
        (return* state* output)

        ("lux;Left" "")
        (fail* (add-loc (&/get$ &/$cursor state) (str "[Analyser Error] Unrecognized token: " (&/show-ast token))))

        ("lux;Left" msg)
        (fail* (add-loc (&/get$ &/$cursor state) msg))
        ))
    ))

(defn ^:private just-analyse [analyser syntax]
  (&type/with-var
    (fn [?var]
      (|do [[?output-term ?output-type] (&&/analyse-1 analyser ?var syntax)]
        (|case [?var ?output-type]
          [("lux;VarT" ?e-id) ("lux;VarT" ?a-id)]
          (if (= ?e-id ?a-id)
            (|do [?output-type* (&type/deref ?e-id)]
              (return (&/T ?output-term ?output-type*)))
            (return (&/T ?output-term ?output-type)))

          [_ _]
          (return (&/T ?output-term ?output-type)))
        ))))

(defn ^:private analyse-ast [eval! compile-module compile-token exo-type token]
  (&/with-cursor (aget token 1 0)
    (&/with-expected-type exo-type
      (|case token
        ("lux;Meta" meta ("lux;FormS" ("lux;Cons" ("lux;Meta" _ ("lux;TagS" ?ident)) ?values)))
        (&&lux/analyse-variant (partial analyse-ast eval! compile-module compile-token) exo-type ?ident ?values)
        
        ("lux;Meta" meta ("lux;FormS" ("lux;Cons" ?fn ?args)))
        (fn [state]
          (|case ((just-analyse (partial analyse-ast eval! compile-module compile-token) ?fn) state)
            ("lux;Right" state* =fn)
            (do ;; (prn 'GOT_FUN (&/show-ast ?fn) (&/show-ast token) (aget =fn 0 0) (aget =fn 1 0))
                ((&&lux/analyse-apply (partial analyse-ast eval! compile-module compile-token) exo-type meta =fn ?args) state*))

            _
            ((analyse-basic-ast (partial analyse-ast eval! compile-module compile-token) eval! compile-module compile-token exo-type token) state)))
        
        _
        (analyse-basic-ast (partial analyse-ast eval! compile-module compile-token) eval! compile-module compile-token exo-type token)))))

;; [Resources]
(defn analyse [eval! compile-module compile-token]
  (|do [asts &parser/parse]
    (&/flat-map% (partial analyse-ast eval! compile-module compile-token &type/$Void) asts)))
