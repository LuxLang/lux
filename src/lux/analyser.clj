;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

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
                          [host :as &&host]
                          [module :as &&module]
                          [parser :as &&a-parser])))

;; [Utils]
(defn analyse-variant+ [analyser exo-type ident values]
  (|do [[module tag-name] (&/normalize ident)
        idx (&&module/tag-index module tag-name)]
    (|case exo-type
      (&/$VarT id)
      (|do [? (&type/bound? id)]
        (if (or ? (&&/type-tag? module tag-name))
          (&&lux/analyse-variant analyser (&/V &/$Right exo-type) idx values)
          (|do [wanted-type (&&module/tag-type module tag-name)
                [[variant-type variant-cursor] variant-analysis] (&&/cap-1 (&&lux/analyse-variant analyser (&/V &/$Left wanted-type) idx values))
                _ (&type/check exo-type variant-type)]
            (return (&/|list (&&/|meta exo-type variant-cursor variant-analysis))))))

      _
      (&&lux/analyse-variant analyser (&/V &/$Right exo-type) idx values)
      )))

(defn ^:private add-loc [meta ^String msg]
  (if (.startsWith msg "@")
    msg
    (|let [[file line col] meta]
      (str "@ " file "," line "," col "\n" msg))))

(defn ^:private fail-with-loc [msg]
  (fn [state]
    (fail* (add-loc (&/get$ &/$cursor state) msg))))

(defn ^:private aba10 [analyse eval! compile-module compile-token exo-type token]
  (|case token
    ;; Arrays
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_znewarray")] (&/$Cons ?length (&/$Nil))))
    (&&host/analyse-jvm-znewarray analyse exo-type ?length)
    
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_zastore")] (&/$Cons ?array (&/$Cons ?idx (&/$Cons ?elem (&/$Nil))))))
    (&&host/analyse-jvm-zastore analyse exo-type ?array ?idx ?elem)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_zaload")] (&/$Cons ?array (&/$Cons ?idx (&/$Nil)))))
    (&&host/analyse-jvm-zaload analyse exo-type ?array ?idx)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_bnewarray")] (&/$Cons ?length (&/$Nil))))
    (&&host/analyse-jvm-bnewarray analyse exo-type ?length)
    
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_bastore")] (&/$Cons ?array (&/$Cons ?idx (&/$Cons ?elem (&/$Nil))))))
    (&&host/analyse-jvm-bastore analyse exo-type ?array ?idx ?elem)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_baload")] (&/$Cons ?array (&/$Cons ?idx (&/$Nil)))))
    (&&host/analyse-jvm-baload analyse exo-type ?array ?idx)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_snewarray")] (&/$Cons ?length (&/$Nil))))
    (&&host/analyse-jvm-snewarray analyse exo-type ?length)
    
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_sastore")] (&/$Cons ?array (&/$Cons ?idx (&/$Cons ?elem (&/$Nil))))))
    (&&host/analyse-jvm-sastore analyse exo-type ?array ?idx ?elem)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_saload")] (&/$Cons ?array (&/$Cons ?idx (&/$Nil)))))
    (&&host/analyse-jvm-saload analyse exo-type ?array ?idx)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_inewarray")] (&/$Cons ?length (&/$Nil))))
    (&&host/analyse-jvm-inewarray analyse exo-type ?length)
    
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_iastore")] (&/$Cons ?array (&/$Cons ?idx (&/$Cons ?elem (&/$Nil))))))
    (&&host/analyse-jvm-iastore analyse exo-type ?array ?idx ?elem)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_iaload")] (&/$Cons ?array (&/$Cons ?idx (&/$Nil)))))
    (&&host/analyse-jvm-iaload analyse exo-type ?array ?idx)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_lnewarray")] (&/$Cons ?length (&/$Nil))))
    (&&host/analyse-jvm-lnewarray analyse exo-type ?length)
    
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_lastore")] (&/$Cons ?array (&/$Cons ?idx (&/$Cons ?elem (&/$Nil))))))
    (&&host/analyse-jvm-lastore analyse exo-type ?array ?idx ?elem)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_laload")] (&/$Cons ?array (&/$Cons ?idx (&/$Nil)))))
    (&&host/analyse-jvm-laload analyse exo-type ?array ?idx)

    _
    (fail-with-loc (str "[Analyser Error] Unknown syntax: " (prn-str (&/show-ast (&/T (&/T "" -1 -1) token)))))))

(defn ^:private aba9 [analyse eval! compile-module compile-token exo-type token]
  (|case token
    ;; Arrays
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_fnewarray")] (&/$Cons ?length (&/$Nil))))
    (&&host/analyse-jvm-fnewarray analyse exo-type ?length)
    
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_fastore")] (&/$Cons ?array (&/$Cons ?idx (&/$Cons ?elem (&/$Nil))))))
    (&&host/analyse-jvm-fastore analyse exo-type ?array ?idx ?elem)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_faload")] (&/$Cons ?array (&/$Cons ?idx (&/$Nil)))))
    (&&host/analyse-jvm-faload analyse exo-type ?array ?idx)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_dnewarray")] (&/$Cons ?length (&/$Nil))))
    (&&host/analyse-jvm-dnewarray analyse exo-type ?length)
    
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_dastore")] (&/$Cons ?array (&/$Cons ?idx (&/$Cons ?elem (&/$Nil))))))
    (&&host/analyse-jvm-dastore analyse exo-type ?array ?idx ?elem)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_daload")] (&/$Cons ?array (&/$Cons ?idx (&/$Nil)))))
    (&&host/analyse-jvm-daload analyse exo-type ?array ?idx)
    
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_cnewarray")] (&/$Cons ?length (&/$Nil))))
    (&&host/analyse-jvm-cnewarray analyse exo-type ?length)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_castore")] (&/$Cons ?array (&/$Cons ?idx (&/$Cons ?elem (&/$Nil))))))
    (&&host/analyse-jvm-castore analyse exo-type ?array ?idx ?elem)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_caload")] (&/$Cons ?array (&/$Cons ?idx (&/$Nil)))))
    (&&host/analyse-jvm-caload analyse exo-type ?array ?idx)

    _
    (aba10 analyse eval! compile-module compile-token exo-type token)))

(defn ^:private aba8 [analyse eval! compile-module compile-token exo-type token]
  (|case token
    ;; Arrays
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_anewarray")] (&/$Cons [_ (&/$TextS ?class)] (&/$Cons ?length (&/$Nil)))))
    (&&host/analyse-jvm-anewarray analyse exo-type ?class ?length)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_aastore")] (&/$Cons ?array (&/$Cons ?idx (&/$Cons ?elem (&/$Nil))))))
    (&&host/analyse-jvm-aastore analyse exo-type ?array ?idx ?elem)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_aaload")] (&/$Cons ?array (&/$Cons ?idx (&/$Nil)))))
    (&&host/analyse-jvm-aaload analyse exo-type ?array ?idx)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_arraylength")] (&/$Cons ?array (&/$Nil))))
    (&&host/analyse-jvm-arraylength analyse exo-type ?array)

    _
    (aba9 analyse eval! compile-module compile-token exo-type token)))

(defn ^:private aba7 [analyse eval! compile-module compile-token exo-type token]
  (|case token
    ;; Classes & interfaces
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_class")]
                       (&/$Cons ?class-decl
                                (&/$Cons ?super-class
                                         (&/$Cons [_ (&/$TupleS ?interfaces)]
                                                  (&/$Cons [_ (&/$TupleS ?anns)]
                                                           (&/$Cons [_ (&/$TupleS ?fields)]
                                                                    (&/$Cons [_ (&/$TupleS ?methods)]
                                                                             (&/$Nil)))))))))
    (|do [=gclass-decl (&&a-parser/parse-gclass-decl ?class-decl)
          =super-class (&&a-parser/parse-gclass-super ?super-class)
          =interfaces (&/map% &&a-parser/parse-gclass-super ?interfaces)
          =anns (&/map% &&a-parser/parse-ann ?anns)
          =fields (&/map% &&a-parser/parse-field ?fields)
          =methods (&/map% &&a-parser/parse-method-def ?methods)]
      (&&host/analyse-jvm-class analyse compile-token =gclass-decl =super-class =interfaces =anns =fields =methods))

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_interface")]
                       (&/$Cons ?class-decl
                                (&/$Cons [_ (&/$TupleS ?supers)]
                                         (&/$Cons [_ (&/$TupleS ?anns)]
                                                  ?methods)))))
    (|do [=gclass-decl (&&a-parser/parse-gclass-decl ?class-decl)
          =supers (&/map% &&a-parser/parse-gclass-super ?supers)
          =anns (&/map% &&a-parser/parse-ann ?anns)
          =methods (&/map% &&a-parser/parse-method-decl ?methods)]
      (&&host/analyse-jvm-interface analyse compile-token =gclass-decl =supers =anns =methods))

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_anon-class")]
                       (&/$Cons ?super-class
                                (&/$Cons [_ (&/$TupleS ?interfaces)]
                                         (&/$Cons [_ (&/$TupleS ?ctor-args)]
                                                  (&/$Cons [_ (&/$TupleS ?methods)]
                                                           (&/$Nil)))))))
    (|do [=super-class (&&a-parser/parse-gclass-super ?super-class)
          =interfaces (&/map% &&a-parser/parse-gclass-super ?interfaces)
          =ctor-args (&/map% &&a-parser/parse-ctor-arg ?ctor-args)
          =methods (&/map% &&a-parser/parse-method-def ?methods)]
      (&&host/analyse-jvm-anon-class analyse compile-token exo-type =super-class =interfaces =ctor-args =methods))

    ;; Programs
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_program")]
                       (&/$Cons [_ (&/$SymbolS "" ?args)]
                                (&/$Cons ?body
                                         (&/$Nil)))))
    (&&host/analyse-jvm-program analyse compile-token ?args ?body)
    
    _
    (aba8 analyse eval! compile-module compile-token exo-type token)))

(defn ^:private aba6 [analyse eval! compile-module compile-token exo-type token]
  (|case token
    ;; Bitwise operators
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_iand")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-iand analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_ior")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-ior analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_ixor")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-ixor analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_ishl")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-ishl analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_ishr")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-ishr analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_iushr")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-iushr analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_land")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-land analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_lor")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-lor analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_lxor")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-lxor analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_lshl")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-lshl analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_lshr")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-lshr analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_lushr")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-lushr analyse exo-type ?x ?y)

    _
    (aba7 analyse eval! compile-module compile-token exo-type token)))

(defn ^:private aba5_5 [analyse eval! compile-module compile-token exo-type token]
  (|case token
    ;; Primitive conversions
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_d2f")] (&/$Cons ?value (&/$Nil))))
    (&&host/analyse-jvm-d2f analyse exo-type ?value)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_d2i")] (&/$Cons ?value (&/$Nil))))
    (&&host/analyse-jvm-d2i analyse exo-type ?value)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_d2l")] (&/$Cons ?value (&/$Nil))))
    (&&host/analyse-jvm-d2l analyse exo-type ?value)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_f2d")] (&/$Cons ?value (&/$Nil))))
    (&&host/analyse-jvm-f2d analyse exo-type ?value)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_f2i")] (&/$Cons ?value (&/$Nil))))
    (&&host/analyse-jvm-f2i analyse exo-type ?value)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_f2l")] (&/$Cons ?value (&/$Nil))))
    (&&host/analyse-jvm-f2l analyse exo-type ?value)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_i2b")] (&/$Cons ?value (&/$Nil))))
    (&&host/analyse-jvm-i2b analyse exo-type ?value)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_i2c")] (&/$Cons ?value (&/$Nil))))
    (&&host/analyse-jvm-i2c analyse exo-type ?value)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_i2d")] (&/$Cons ?value (&/$Nil))))
    (&&host/analyse-jvm-i2d analyse exo-type ?value)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_i2f")] (&/$Cons ?value (&/$Nil))))
    (&&host/analyse-jvm-i2f analyse exo-type ?value)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_i2l")] (&/$Cons ?value (&/$Nil))))
    (&&host/analyse-jvm-i2l analyse exo-type ?value)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_i2s")] (&/$Cons ?value (&/$Nil))))
    (&&host/analyse-jvm-i2s analyse exo-type ?value)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_l2d")] (&/$Cons ?value (&/$Nil))))
    (&&host/analyse-jvm-l2d analyse exo-type ?value)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_l2f")] (&/$Cons ?value (&/$Nil))))
    (&&host/analyse-jvm-l2f analyse exo-type ?value)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_l2i")] (&/$Cons ?value (&/$Nil))))
    (&&host/analyse-jvm-l2i analyse exo-type ?value)

    _
    (aba6 analyse eval! compile-module compile-token exo-type token)))

(defn ^:private aba5 [analyse eval! compile-module compile-token exo-type token]
  (|case token
    ;; Objects
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_null")] (&/$Nil)))
    (&&host/analyse-jvm-null analyse exo-type)
    
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_null?")]
                       (&/$Cons ?object
                                (&/$Nil))))
    (&&host/analyse-jvm-null? analyse exo-type ?object)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_instanceof")]
                       (&/$Cons [_ (&/$TextS ?class)]
                                (&/$Cons ?object
                                         (&/$Nil)))))
    (&&host/analyse-jvm-instanceof analyse exo-type ?class ?object)
    
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_new")]
                       (&/$Cons [_ (&/$TextS ?class)]
                                (&/$Cons [_ (&/$TupleS ?arg-classes)]
                                         (&/$Cons [_ (&/$TupleS ?args)]
                                                  (&/$Nil))))))
    (|do [=arg-classes (&/map% &&a-parser/parse-text ?arg-classes)]
      (&&host/analyse-jvm-new analyse exo-type ?class =arg-classes ?args))

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_getstatic")]
                       (&/$Cons [_ (&/$TextS ?class)]
                                (&/$Cons [_ (&/$TextS ?field)]
                                         (&/$Nil)))))
    (&&host/analyse-jvm-getstatic analyse exo-type ?class ?field)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_getfield")]
                       (&/$Cons [_ (&/$TextS ?class)]
                                (&/$Cons [_ (&/$TextS ?field)]
                                         (&/$Cons ?object
                                                  (&/$Nil))))))
    (&&host/analyse-jvm-getfield analyse exo-type ?class ?field ?object)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_putstatic")]
                       (&/$Cons [_ (&/$TextS ?class)]
                                (&/$Cons [_ (&/$TextS ?field)]
                                         (&/$Cons ?value
                                                  (&/$Nil))))))
    (&&host/analyse-jvm-putstatic analyse exo-type ?class ?field ?value)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_putfield")]
                       (&/$Cons [_ (&/$TextS ?class)]
                                (&/$Cons [_ (&/$TextS ?field)]
                                         (&/$Cons ?value
                                                  (&/$Cons ?object
                                                           (&/$Nil)))))))
    (&&host/analyse-jvm-putfield analyse exo-type ?class ?field ?value ?object)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_invokestatic")]
                       (&/$Cons [_ (&/$TextS ?class)]
                                (&/$Cons [_ (&/$TextS ?method)]
                                         (&/$Cons [_ (&/$TupleS ?arg-classes)]
                                                  (&/$Cons [_ (&/$TupleS ?args)]
                                                           (&/$Nil)))))))
    (|do [=arg-classes (&/map% &&a-parser/parse-text ?arg-classes)]
      (&&host/analyse-jvm-invokestatic analyse exo-type ?class ?method =arg-classes ?args))

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_invokevirtual")]
                       (&/$Cons [_ (&/$TextS ?class)]
                                (&/$Cons [_ (&/$TextS ?method)]
                                         (&/$Cons [_ (&/$TupleS ?arg-classes)]
                                                  (&/$Cons ?object
                                                           (&/$Cons [_ (&/$TupleS ?args)]
                                                                    (&/$Nil))))))))
    (|do [=arg-classes (&/map% &&a-parser/parse-text ?arg-classes)]
      (&&host/analyse-jvm-invokevirtual analyse exo-type ?class ?method =arg-classes ?object ?args))

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_invokeinterface")]
                       (&/$Cons [_ (&/$TextS ?class)]
                                (&/$Cons [_ (&/$TextS ?method)]
                                         (&/$Cons [_ (&/$TupleS ?arg-classes)]
                                                  (&/$Cons ?object
                                                           (&/$Cons [_ (&/$TupleS ?args)]
                                                                    (&/$Nil))))))))
    (|do [=arg-classes (&/map% &&a-parser/parse-text ?arg-classes)]
      (&&host/analyse-jvm-invokeinterface analyse exo-type ?class ?method =arg-classes ?object ?args))

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_invokespecial")]
                       (&/$Cons [_ (&/$TextS ?class)]
                                (&/$Cons [_ (&/$TextS ?method)]
                                         (&/$Cons [_ (&/$TupleS ?arg-classes)]
                                                  (&/$Cons ?object
                                                           (&/$Cons [_ (&/$TupleS ?args)]
                                                                    (&/$Nil))))))))
    (|do [=arg-classes (&/map% &&a-parser/parse-text ?arg-classes)]
      (&&host/analyse-jvm-invokespecial analyse exo-type ?class ?method =arg-classes ?object ?args))

    ;; Exceptions
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_try")]
                       (&/$Cons ?body
                                ?handlers)))
    (|do [catches+finally (&/fold% &&a-parser/parse-handler (&/T &/Nil$ &/None$) ?handlers)]
      (&&host/analyse-jvm-try analyse exo-type ?body catches+finally))

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_throw")]
                       (&/$Cons ?ex
                                (&/$Nil))))
    (&&host/analyse-jvm-throw analyse exo-type ?ex)

    ;; Syncronization/monitos
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_monitorenter")]
                       (&/$Cons ?monitor
                                (&/$Nil))))
    (&&host/analyse-jvm-monitorenter analyse exo-type ?monitor)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_monitorexit")]
                       (&/$Cons ?monitor
                                (&/$Nil))))
    (&&host/analyse-jvm-monitorexit analyse exo-type ?monitor)

    _
    (aba5_5 analyse eval! compile-module compile-token exo-type token)))

(defn ^:private aba4 [analyse eval! compile-module compile-token exo-type token]
  (|case token
    ;; Float arithmetic
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_fadd")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-fadd analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_fsub")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-fsub analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_fmul")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-fmul analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_fdiv")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-fdiv analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_frem")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-frem analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_feq")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-feq analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_flt")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-flt analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_fgt")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-fgt analyse exo-type ?x ?y)

    ;; Double arithmetic
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_dadd")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-dadd analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_dsub")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-dsub analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_dmul")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-dmul analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_ddiv")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-ddiv analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_drem")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-drem analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_deq")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-deq analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_dlt")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-dlt analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_dgt")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-dgt analyse exo-type ?x ?y)
    
    _
    (aba5 analyse eval! compile-module compile-token exo-type token)))

(defn ^:private aba3 [analyse eval! compile-module compile-token exo-type token]
  (|case token
    ;; Host special forms
    ;; Characters
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_ceq")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-ceq analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_clt")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-clt analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_cgt")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-cgt analyse exo-type ?x ?y)
    
    ;; Integer arithmetic
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_iadd")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-iadd analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_isub")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-isub analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_imul")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-imul analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_idiv")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-idiv analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_irem")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-irem analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_ieq")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-ieq analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_ilt")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-ilt analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_igt")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-igt analyse exo-type ?x ?y)

    ;; Long arithmetic
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_ladd")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-ladd analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_lsub")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-lsub analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_lmul")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-lmul analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_ldiv")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-ldiv analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_lrem")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-lrem analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_leq")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-leq analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_llt")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-llt analyse exo-type ?x ?y)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_lgt")] (&/$Cons ?x (&/$Cons ?y (&/$Nil)))))
    (&&host/analyse-jvm-lgt analyse exo-type ?x ?y)

    _
    (aba4 analyse eval! compile-module compile-token exo-type token)))

(defn ^:private aba2 [analyse eval! compile-module compile-token exo-type token]
  (|case token
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_case")]
                       (&/$Cons ?value ?branches)))
    (&&lux/analyse-case analyse exo-type ?value ?branches)
    
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_lambda")]
                       (&/$Cons [_ (&/$SymbolS "" ?self)]
                                (&/$Cons [_ (&/$SymbolS "" ?arg)]
                                         (&/$Cons ?body
                                                  (&/$Nil))))))
    (&&lux/analyse-lambda analyse exo-type ?self ?arg ?body)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_def")]
                       (&/$Cons [_ (&/$SymbolS "" ?name)]
                                (&/$Cons ?value
                                         (&/$Nil)))))
    (&&lux/analyse-def analyse compile-token ?name ?value)
    
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_declare-macro")]
                       (&/$Cons [_ (&/$SymbolS "" ?name)]
                                (&/$Nil))))
    (&&lux/analyse-declare-macro analyse compile-token ?name)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_declare-tags")]
                       (&/$Cons [_ (&/$TupleS tags)]
                                (&/$Cons [_ (&/$SymbolS "" type-name)]
                                         (&/$Nil)))))
    (|do [tags* (&/map% &&a-parser/parse-tag tags)]
      (&&lux/analyse-declare-tags tags* type-name))
    
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_import")]
                       (&/$Cons [_ (&/$TextS ?path)]
                                (&/$Nil))))
    (&&lux/analyse-import analyse compile-module compile-token ?path)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_:")]
                       (&/$Cons ?type
                                (&/$Cons ?value
                                         (&/$Nil)))))
    (&&lux/analyse-check analyse eval! exo-type ?type ?value)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_:!")]
                       (&/$Cons ?type
                                (&/$Cons ?value
                                         (&/$Nil)))))
    (&&lux/analyse-coerce analyse eval! exo-type ?type ?value)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_export")]
                       (&/$Cons [_ (&/$SymbolS "" ?ident)]
                                (&/$Nil))))
    (&&lux/analyse-export analyse compile-token ?ident)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_alias")]
                       (&/$Cons [_ (&/$TextS ?alias)]
                                (&/$Cons [_ (&/$TextS ?module)]
                                         (&/$Nil)))))
    (&&lux/analyse-alias analyse compile-token ?alias ?module)
    
    _
    (aba3 analyse eval! compile-module compile-token exo-type token)))

(defn ^:private aba1 [analyse eval! compile-module compile-token exo-type token]
  (|case token
    ;; Standard special forms
    (&/$BoolS ?value)
    (|do [_ (&type/check exo-type &type/Bool)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor (&/V &&/$bool ?value)))))

    (&/$IntS ?value)
    (|do [_ (&type/check exo-type &type/Int)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor (&/V &&/$int ?value)))))

    (&/$RealS ?value)
    (|do [_ (&type/check exo-type &type/Real)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor (&/V &&/$real ?value)))))

    (&/$CharS ?value)
    (|do [_ (&type/check exo-type &type/Char)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor (&/V &&/$char ?value)))))

    (&/$TextS ?value)
    (|do [_ (&type/check exo-type &type/Text)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor (&/V &&/$text ?value)))))

    (&/$TupleS ?elems)
    (&&lux/analyse-tuple analyse (&/V &/$Right exo-type) ?elems)

    (&/$RecordS ?elems)
    (&&lux/analyse-record analyse exo-type ?elems)

    (&/$TagS ?ident)
    (analyse-variant+ analyse exo-type ?ident &/Nil$)

    (&/$SymbolS ?ident)
    (&&lux/analyse-symbol analyse exo-type ?ident)
    
    _
    (aba2 analyse eval! compile-module compile-token exo-type token)
    ))

(defn ^:private analyse-basic-ast [analyse eval! compile-module compile-token exo-type token]
  (|case token
    [meta ?token]
    (fn [state]
      (|case ((aba1 analyse eval! compile-module compile-token exo-type ?token) state)
        (&/$Right state* output)
        (return* state* output)

        (&/$Left "")
        (fail* (add-loc (&/get$ &/$cursor state) (str "[Analyser Error] Unrecognized token: " (&/show-ast token))))

        (&/$Left msg)
        (fail* (add-loc (&/get$ &/$cursor state) msg))
        ))
    ))

(defn ^:private just-analyse [analyser syntax]
  (&type/with-var
    (fn [?var]
      (|do [[[?output-type ?output-cursor] ?output-term] (&&/analyse-1 analyser ?var syntax)]
        (|case [?var ?output-type]
          [(&/$VarT ?e-id) (&/$VarT ?a-id)]
          (if (= ?e-id ?a-id)
            (|do [?output-type* (&type/deref ?e-id)]
              (return (&&/|meta ?output-type* ?output-cursor ?output-term)))
            (return (&&/|meta ?output-type ?output-cursor ?output-term)))

          [_ _]
          (return (&&/|meta ?output-type ?output-cursor ?output-term)))
        ))))

(defn ^:private analyse-ast [eval! compile-module compile-token exo-type token]
  (|let [[cursor _] token]
    (&/with-cursor cursor
      (&/with-expected-type exo-type
        (|case token
          [meta (&/$FormS (&/$Cons [_ (&/$IntS idx)] ?values))]
          (&&lux/analyse-variant (partial analyse-ast eval! compile-module compile-token) (&/V &/$Right exo-type) idx ?values)

          [meta (&/$FormS (&/$Cons [_ (&/$TagS ?ident)] ?values))]
          (analyse-variant+ (partial analyse-ast eval! compile-module compile-token) exo-type ?ident ?values)
          
          [meta (&/$FormS (&/$Cons ?fn ?args))]
          (|case ?fn
            [_ (&/$SymbolS _)]
            (fn [state]
              (|case ((just-analyse (partial analyse-ast eval! compile-module compile-token) ?fn) state)
                (&/$Right state* =fn)
                ((&&lux/analyse-apply (partial analyse-ast eval! compile-module compile-token) exo-type meta =fn ?args) state*)

                _
                ((analyse-basic-ast (partial analyse-ast eval! compile-module compile-token) eval! compile-module compile-token exo-type token) state)))

            _
            (|do [=fn (just-analyse (partial analyse-ast eval! compile-module compile-token) ?fn)]
              (&&lux/analyse-apply (partial analyse-ast eval! compile-module compile-token) exo-type meta =fn ?args)))
          
          _
          (analyse-basic-ast (partial analyse-ast eval! compile-module compile-token) eval! compile-module compile-token exo-type token))))))

;; [Resources]
(defn analyse [eval! compile-module compile-token]
  (|do [asts &parser/parse]
    (&/flat-map% (partial analyse-ast eval! compile-module compile-token &type/$Void) asts)))
