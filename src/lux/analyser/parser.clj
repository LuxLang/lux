;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.analyser.parser
  (:require (clojure [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |let |case]])))

;; [Parsers]
(defn parse-tag [ast]
  (|case ast
    [_ (&/$TagS "" name)]
    (return name)
    
    _
    (fail (str "[Analyser Error] Not a tag: " (&/show-ast ast)))))

(defn parse-text [ast]
  (|case ast
    [_ (&/$TextS text)]
    (return text)

    _
    (fail (str "[Analyser Error] Not text: " (&/show-ast ast)))))

(defn parse-gclass-decl [ast]
  (|case ast
    [_ (&/$FormS (&/$Cons [_ (&/$TextS class-name)] (&/$Cons [_ (&/$TupleS args)] (&/$Nil))))]
    (|do [=args (&/map% parse-text args)]
      (return (&/T class-name =args)))

    _
    (fail (str "[Analyser Error] Not generic class declaration: " (&/show-ast ast)))))

(defn parse-gclass [ast]
  (|case ast
    [_ (&/$TextS var-name)]
    (return (&/V &/$GenericTypeVar var-name))

    [_ (&/$FormS (&/$Cons [_ (&/$TextS class-name)] (&/$Cons [_ (&/$TupleS params)] (&/$Nil))))]
    (|do [=params (&/map% parse-gclass params)]
      (return (&/V &/$GenericClass (&/T class-name =params))))

    [_ (&/$FormS (&/$Cons [_ (&/$TextS "Array")] (&/$Cons param (&/$Nil))))]
    (|do [=param (parse-gclass param)]
      (return (&/V &/$GenericArray =param)))

    _
    (fail (str "[Analyser Error] Not generic class: " (&/show-ast ast)))))

(defn parse-gclass-super [ast]
  (|case ast
    [_ (&/$FormS (&/$Cons [_ (&/$TextS class-name)] (&/$Cons [_ (&/$TupleS params)] (&/$Nil))))]
    (|do [=params (&/map% parse-gclass params)]
      (return (&/T class-name =params)))

    _
    (fail (str "[Analyser Error] Not generic super-class: " (&/show-ast ast)))))

(defn parse-ctor-arg [ast]
  (|case ast
    [_ (&/$TupleS (&/$Cons ?class (&/$Cons ?term (&/$Nil))))]
    (|do [=class (parse-gclass ?class)]
      (return (&/T =class ?term)))

    _
    (fail (str "[Analyser Error] Not constructor argument: " (&/show-ast ast)))))

(defn parse-handler [[catch+ finally+] token]
  (|case token
    [meta (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_catch")]
                             (&/$Cons [_ (&/$TextS ?ex-class)]
                                      (&/$Cons [_ (&/$SymbolS "" ?ex-arg)]
                                               (&/$Cons ?catch-body
                                                        (&/$Nil))))))]
    (return (&/T (&/|++ catch+ (&/|list (&/T ?ex-class ?ex-arg ?catch-body))) finally+))

    [meta (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_finally")]
                             (&/$Cons ?finally-body
                                      (&/$Nil))))]
    (return (&/T catch+ (&/V &/$Some ?finally-body)))

    _
    (fail (str "[Analyser Error] Wrong syntax for exception handler: " (&/show-ast token)))))

(let [failure (fail (str "[Analyser Error] Invalid annotation parameter."))]
  (defn ^:private parse-ann-param [param]
    (|case param
      [[_ (&/$TextS param-name)] param-value]
      (|case param-value
        [_ (&/$BoolS param-value*)] (return (&/T param-name (boolean param-value*)))
        [_ (&/$IntS param-value*)]  (return (&/T param-name (int param-value*)))
        [_ (&/$RealS param-value*)] (return (&/T param-name (float param-value*)))
        [_ (&/$CharS param-value*)] (return (&/T param-name (char param-value*)))
        [_ (&/$TextS param-value*)] (return (&/T param-name param-value*))

        _
        failure)

      _
      failure)))

(defn parse-ann [ast]
  (|case ast
    [_ (&/$FormS (&/$Cons [_ (&/$TextS ann-name)] (&/$Cons [_ (&/$RecordS ann-params)] (&/$Nil))))]
    (|do [=ann-params (&/map% parse-ann-param ann-params)]
      (return {:name ann-name
               :params ann-params}))

    _
    (fail (str "[Analyser Error] Invalid annotation: " (&/show-ast ast)))))

(defn ^:private parse-arg-decl [ast]
  (|case ast
    [_ (&/$FormS (&/$Cons [_ (&/$SymbolS ["" arg-name])]
                          (&/$Cons gclass
                                   (&/$Nil))))]
    (|do [=gclass (parse-gclass gclass)]
      (return (&/T arg-name =gclass)))
    
    _
    (fail (str "[Analyser Error] Invalid argument declaration: " (&/show-ast ast)))))

(defn parse-method-decl [ast]
  (|case ast
    [_ (&/$FormS (&/$Cons [_ (&/$TextS method-name)]
                          (&/$Cons [_ (&/$TupleS anns)]
                                   (&/$Cons [_ (&/$TupleS gvars)]
                                            (&/$Cons [_ (&/$TupleS exceptions)]
                                                     (&/$Cons [_ (&/$TupleS inputs)]
                                                              (&/$Cons output (&/$Nil))))))))]
    (|do [=anns (&/map% parse-ann anns)
          =gvars (&/map% parse-text gvars)
          =exceptions (&/map% parse-gclass exceptions)
          =inputs (&/map% parse-gclass inputs)
          =output (parse-gclass output)]
      (return (&/T method-name =anns =gvars =exceptions =inputs =output)))
    
    _
    (fail (str "[Analyser Error] Invalid method declaration: " (&/show-ast ast)))))

(defn parse-method-def [ast]
  (|case ast
    [_ (&/$FormS (&/$Cons [_ (&/$TextS "init")]
                          (&/$Cons [_ (&/$TupleS anns)]
                                   (&/$Cons [_ (&/$TupleS gvars)]
                                            (&/$Cons [_ (&/$TupleS exceptions)]
                                                     (&/$Cons [_ (&/$TupleS inputs)]
                                                              (&/$Cons ?ctor-args
                                                                       (&/$Cons body (&/$Nil)))))))))]
    (|do [=anns (&/map% parse-ann anns)
          =gvars (&/map% parse-text gvars)
          =exceptions (&/map% parse-gclass exceptions)
          =inputs (&/map% parse-arg-decl inputs)
          =ctor-args (&/map% parse-ctor-arg ?ctor-args)]
      (return (&/V &/$ConstructorMethodSyntax (&/T =anns =gvars =exceptions =inputs =ctor-args body))))

    [_ (&/$FormS (&/$Cons [_ (&/$TextS "virtual")]
                          (&/$Cons [_ (&/$TextS ?name)]
                                   (&/$Cons [_ (&/$TupleS anns)]
                                            (&/$Cons [_ (&/$TupleS gvars)]
                                                     (&/$Cons [_ (&/$TupleS exceptions)]
                                                              (&/$Cons [_ (&/$TupleS inputs)]
                                                                       (&/$Cons output
                                                                                (&/$Cons body (&/$Nil))))))))))]
    (|do [=anns (&/map% parse-ann anns)
          =gvars (&/map% parse-text gvars)
          =exceptions (&/map% parse-gclass exceptions)
          =inputs (&/map% parse-arg-decl inputs)
          =output (parse-gclass output)]
      (return (&/V &/$VirtualMethodSyntax (&/T ?name =anns =gvars =exceptions =inputs =output body))))

    [_ (&/$FormS (&/$Cons [_ (&/$TextS "override")]
                          (&/$Cons ?class-decl
                                   (&/$Cons [_ (&/$TextS ?name)]
                                            (&/$Cons [_ (&/$TupleS anns)]
                                                     (&/$Cons [_ (&/$TupleS gvars)]
                                                              (&/$Cons [_ (&/$TupleS exceptions)]
                                                                       (&/$Cons [_ (&/$TupleS inputs)]
                                                                                (&/$Cons output
                                                                                         (&/$Cons body (&/$Nil)))))))))))]
    (|do [=class-decl (parse-gclass-decl ?class-decl)
          =anns (&/map% parse-ann anns)
          =gvars (&/map% parse-text gvars)
          =exceptions (&/map% parse-gclass exceptions)
          =inputs (&/map% parse-arg-decl inputs)
          =output (parse-gclass output)]
      (return (&/V &/$OverridenMethodSyntax (&/T =class-decl ?name =anns =gvars =exceptions =inputs =output body))))
    
    _
    (fail (str "[Analyser Error] Invalid method definition: " (&/show-ast ast)))))

(defn parse-field [ast]
  (|case ast
    [_ (&/$FormS (&/$Cons [_ (&/$TextS ?name)]
                          (&/$Cons [_ (&/$TupleS ?anns)]
                                   (&/$Cons [_ (&/$TextS ?type)]
                                            (&/$Nil)))))]
    (|do [=anns (&/map% parse-ann ?anns)
          =type (parse-gclass ?type)]
      (return (&/T ?name =anns =type)))
    
    _
    (fail (str "[Analyser Error] Invalid field declaration: " (&/show-ast ast)))))
