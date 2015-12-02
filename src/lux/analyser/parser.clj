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

(defn parse-ctor-arg [ast]
  (|case ast
    [_ (&/$TupleS (&/$Cons ?class (&/$Cons [_ (&/$TextS ?term)] (&/$Nil))))]
    (return (&/T ?class ?term))

    _
    (fail (str "[Analyser Error] Not constructor argument: " (&/show-ast ast)))))

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

(defn parse-modifiers [modifiers]
  (&/fold% (fn [so-far modif]
             (|case modif
               [_ (&/$TextS "public")]
               (return (assoc so-far :visibility "public"))

               [_ (&/$TextS "private")]
               (return (assoc so-far :visibility "private"))

               [_ (&/$TextS "protected")]
               (return (assoc so-far :visibility "protected"))

               [_ (&/$TextS "static")]
               (return (assoc so-far :static? true))

               [_ (&/$TextS "final")]
               (return (assoc so-far :final? true))

               [_ (&/$TextS "abstract")]
               (return (assoc so-far :abstract? true))

               [_ (&/$TextS "synchronized")]
               (return (assoc so-far :concurrency "synchronized"))

               [_ (&/$TextS "volatile")]
               (return (assoc so-far :concurrency "volatile"))

               _
               (fail (str "[Analyser Error] Unknown modifier: " (&/show-ast modif)))))
           {:visibility "default"
            :static? false
            :final? false
            :abstract? false
            :concurrency nil}
           modifiers))

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

(defn ^:private parse-method-decl* [asts]
  (|case asts
    (&/$Cons [_ (&/$TextS method-name)]
             (&/$Cons [_ (&/$TupleS modifiers)]
                      (&/$Cons [_ (&/$TupleS anns)]
                               (&/$Cons [_ (&/$TupleS gvars)]
                                        (&/$Cons [_ (&/$TupleS exceptions)]
                                                 (&/$Cons [_ (&/$TupleS inputs)]
                                                          (&/$Cons output
                                                                   *tail*)))))))
    (|do [=modifiers (parse-modifiers modifiers)
          =anns (&/map% parse-ann anns)
          =gvars (&/map% parse-text gvars)
          =exceptions (&/map% parse-gclass exceptions)
          =inputs (&/map% parse-arg-decl inputs)
          =output (parse-gclass output)]
      (return (&/T (&/T method-name =modifiers =anns =gvars =exceptions =inputs =output)
                   *tail*)))
    
    _
    (fail (str "[Analyser Error] Invalid method declaration: " (->> asts (&/|map &/show-ast) (&/|interpose " ") (&/fold str ""))))))

(defn parse-method-decl [ast]
  (|case ast
    [_ (&/$FormS tokens)]
    (|do [[decl *tail*] (parse-method-decl* tokens)]
      (|case *tail*
        (&/$Nil)
        (return decl)

        _
        (fail (str "[Analyser Error] Invalid method declaration: " (&/show-ast ast)))))
    
    _
    (fail (str "[Analyser Error] Invalid method declaration: " (&/show-ast ast)))))

(defn parse-method-def [ast]
  (|case ast
    [_ (&/$FormS tokens)]
    (|do [[decl *tail*] (parse-method-decl* tokens)]
      (|case *tail*
        (&/$Cons body (&/$Nil))
        (return (&/T decl body))

        _
        (fail (str "[Analyser Error] Invalid method definition: " (&/show-ast ast)))))
    
    _
    (fail (str "[Analyser Error] Invalid method definition: " (&/show-ast ast)))))

(defn parse-field [ast]
  (|case ast
    [_ (&/$FormS (&/$Cons [_ (&/$TextS ?name)]
                          (&/$Cons [_ (&/$TupleS ?modifiers)]
                                   (&/$Cons [_ (&/$TupleS ?anns)]
                                            (&/$Cons [_ (&/$TextS ?type)]
                                                     (&/$Nil))))))]
    (|do [=modifiers (parse-modifiers ?modifiers)
          =anns (&/map% parse-ann ?anns)
          =type (parse-gclass ?type)]
      (return (&/T ?name =modifiers =anns =type)))
    
    _
    (fail (str "[Analyser Error] Invalid field declaration: " (&/show-ast ast)))))
