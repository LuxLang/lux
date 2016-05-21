;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.analyser.parser
  (:require (clojure [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |let |case]]
                 [reader :as &reader]
                 [lexer :as &lexer]
                 [parser :as &parser])))

(declare parse-gclass)

;; [Parsers]
(def ^:private _space_ (&reader/read-text " "))

(defn ^:private repeat% [action]
  (fn [state]
    (|case (action state)
      (&/$Left ^String error)
      (&/$Right (&/T [state &/$Nil]))

      (&/$Right state* head)
      ((|do [tail (repeat% action)]
         (return (&/$Cons head tail)))
       state*))))

(defn ^:private spaced [action]
  (fn [state]
    (|case (action state)
      (&/$Left ^String error)
      (&/$Right (&/T [state &/$Nil]))

      (&/$Right state* head)
      ((&/try-all% (&/|list (|do [_ _space_
                                  tail (spaced action)]
                              (return (&/$Cons head tail)))
                            (return (&/|list head))))
       state*))))

(def ^:private parse-name
  (|do [[_ _ =name] (&reader/read-regex #"^([a-zA-Z0-9_\.]+)")]
    (return =name)))

(def ^:private parse-ident
  (|do [[_ _ =name] (&reader/read-regex &lexer/+ident-re+)]
    (return =name)))

(defn ^:private with-parens [body]
  (|do [_ (&reader/read-text "(")
        output body
        _ (&reader/read-text ")")]
    (return output)))

(defn ^:private with-brackets [body]
  (|do [_ (&reader/read-text "[")
        output body
        _ (&reader/read-text "]")]
    (return output)))

(defn ^:private with-braces [body]
  (|do [_ (&reader/read-text "{")
        output body
        _ (&reader/read-text "}")]
    (return output)))

(def ^:private parse-type-param
  (with-parens
    (|do [=name parse-name
          _ _space_
          =bounds (spaced parse-gclass)]
      (return (&/T [=name =bounds])))))

(def ^:private parse-gclass-decl
  (with-parens
    (|do [=class-name parse-name
          _ _space_
          =params (spaced parse-type-param)]
      (return (&/T [=class-name =params])))))

(def ^:private parse-bound-kind
  (&/try-all% (&/|list (|do [_ (&reader/read-text "<")]
                         (return &/$UpperBound))
                       
                       (|do [_ (&reader/read-text ">")]
                         (return &/$LowerBound))
                       )))

(def parse-gclass
  (&/try-all% (&/|list (|do [=bound-kind parse-bound-kind
                             =bound parse-gclass]
                         (return (&/$GenericWildcard (&/$Some (&/T [=bound-kind =bound])))))
                       
                       (|do [_ (&reader/read-text "?")]
                         (return (&/$GenericWildcard &/$None)))
                       
                       (|do [var-name parse-name]
                         (return (&/$GenericTypeVar var-name)))

                       (with-parens
                         (|do [class-name parse-name
                               _ _space_
                               =params (spaced parse-gclass)]
                           (return (&/$GenericClass class-name =params))))

                       (with-parens
                         (|do [_ (&reader/read-text "Array")
                               _ _space_
                               =param parse-gclass]
                           (return (&/$GenericArray =param))))
                       )))

(def ^:private parse-gclass-super
  (with-parens
    (|do [class-name parse-name
          _ _space_
          =params (spaced parse-gclass)]
      (return (&/T [class-name =params])))))

(def ^:private parse-ctor-arg
  (with-brackets
    (|do [=class parse-gclass
          (&/$Cons =term (&/$Nil)) &parser/parse]
      (return (&/T [=class =term])))))

(def ^:private parse-ann-param
  (|do [param-name parse-name
        _ (&reader/read-text "=")
        param-value (&/try-all% (&/|list (|do [[_ (&lexer/$Bool param-value*)] &lexer/lex-bool]
                                           (return (boolean param-value*)))

                                         (|do [[_ (&lexer/$Int param-value*)] &lexer/lex-int]
                                           (return (int param-value*)))

                                         (|do [_ (&reader/read-text "l")
                                               [_ (&lexer/$Int param-value*)] &lexer/lex-int]
                                           (return (long param-value*)))

                                         (|do [[_ (&lexer/$Real param-value*)] &lexer/lex-real]
                                           (return (float param-value*)))

                                         (|do [_ (&reader/read-text "d")
                                               [_ (&lexer/$Real param-value*)] &lexer/lex-real]
                                           (return (double param-value*)))

                                         (|do [[_ (&lexer/$Char param-value*)] &lexer/lex-char]
                                           (return (char param-value*)))

                                         (|do [[_ (&lexer/$Text param-value*)] &lexer/lex-text]
                                           (return param-value*))
                                         ))]
    (return (&/T [param-name param-value]))))

(def ^:private parse-ann
  (with-parens
    (|do [ann-name parse-name
          _ _space_
          =ann-params (with-braces
                        (spaced parse-ann-param))]
      (return {:name ann-name
               :params =ann-params}))))

(def ^:private parse-arg-decl
  (with-parens
    (|do [=arg-name parse-ident
          _ (&reader/read-text " ")
          =gclass parse-gclass]
      (return (&/T [=arg-name =gclass])))))

(def ^:private parse-gvars
  (|do [=head parse-name
        [_ _ ?] (&reader/read-text? " ")]
    (if ?
      (|do [=tail parse-gvars]
        (return (&/$Cons =head =tail)))
      (return (&/|list =head)))))

(def ^:private parse-method-decl
  (with-parens
    (|do [=method-name parse-name
          _ _space_
          =anns (with-brackets
                  (spaced parse-ann))
          _ _space_
          =gvars (with-brackets
                   parse-gvars)
          _ _space_
          =exceptions (with-brackets
                        (spaced parse-gclass))
          _ _space_
          =inputs (with-brackets
                    (spaced parse-gclass))
          _ _space_
          =output parse-gclass]
      (return (&/T [=method-name =anns =gvars =exceptions =inputs =output])))))

(def ^:private parse-privacy-modifier
  (&/try-all% (&/|list (|do [_ (&reader/read-text "default")]
                         (return &/$DefaultPM))

                       (|do [_ (&reader/read-text "public")]
                         (return &/$PublicPM))

                       (|do [_ (&reader/read-text "protected")]
                         (return &/$ProtectedPM))

                       (|do [_ (&reader/read-text "private")]
                         (return &/$PrivatePM))
                       )))

(def ^:private parse-state-modifier
  (&/try-all% (&/|list (|do [_ (&reader/read-text "default")]
                         (return &/$DefaultSM))

                       (|do [_ (&reader/read-text "volatile")]
                         (return &/$VolatileSM))

                       (|do [_ (&reader/read-text "final")]
                         (return &/$FinalSM))
                       )))

(def ^:private parse-inheritance-modifier
  (&/try-all% (&/|list (|do [_ (&reader/read-text "default")]
                         (return &/$DefaultIM))

                       (|do [_ (&reader/read-text "abstract")]
                         (return &/$AbstractIM))

                       (|do [_ (&reader/read-text "final")]
                         (return &/$FinalIM))
                       )))

(def ^:private parse-method-init-def
  (|do [_ (&reader/read-text "init")
        _ _space_
        =privacy-modifier parse-privacy-modifier
        _ _space_
        [_ (&lexer/$Bool =strict)] &lexer/lex-bool
        _ _space_
        =anns (with-brackets
                (spaced parse-ann))
        _ _space_
        =gvars (with-brackets
                 (spaced parse-type-param))
        _ _space_
        =exceptions (with-brackets
                      (spaced parse-gclass))
        _ _space_
        =inputs (with-brackets
                  (spaced parse-arg-decl))
        _ _space_
        =ctor-args (with-brackets
                     (spaced parse-ctor-arg))
        _ _space_
        (&/$Cons =body (&/$Nil)) &parser/parse]
    (return (&/$ConstructorMethodSyntax (&/T [=privacy-modifier =strict =anns =gvars =exceptions =inputs =ctor-args =body])))))

(def ^:private parse-method-virtual-def
  (|do [_ (&reader/read-text "virtual")
        _ _space_
        =name parse-name
        _ _space_
        =privacy-modifier parse-privacy-modifier
        _ _space_
        [_ (&lexer/$Bool =final?)] &lexer/lex-bool
        _ _space_
        [_ (&lexer/$Bool =strict)] &lexer/lex-bool
        _ _space_
        =anns (with-brackets
                (spaced parse-ann))
        _ _space_
        =gvars (with-brackets
                 (spaced parse-type-param))
        _ _space_
        =exceptions (with-brackets
                      (spaced parse-gclass))
        _ _space_
        =inputs (with-brackets
                  (spaced parse-arg-decl))
        _ _space_
        =output parse-gclass
        _ _space_
        (&/$Cons =body (&/$Nil)) &parser/parse]
    (return (&/$VirtualMethodSyntax (&/T [=name =privacy-modifier =final? =strict =anns =gvars =exceptions =inputs =output =body])))))

(def ^:private parse-method-override-def
  (|do [_ (&reader/read-text "override")
        _ _space_
        =class-decl parse-gclass-decl
        _ _space_
        =name parse-name
        _ _space_
        [_ (&lexer/$Bool =strict)] &lexer/lex-bool
        _ _space_
        =anns (with-brackets
                (spaced parse-ann))
        _ _space_
        =gvars (with-brackets
                 (spaced parse-type-param))
        _ _space_
        =exceptions (with-brackets
                      (spaced parse-gclass))
        _ _space_
        =inputs (with-brackets
                  (spaced parse-arg-decl))
        _ _space_
        =output parse-gclass
        _ _space_
        (&/$Cons =body (&/$Nil)) &parser/parse]
    (return (&/$OverridenMethodSyntax (&/T [=class-decl =name =strict =anns =gvars =exceptions =inputs =output =body])))))

(def ^:private parse-method-static-def
  (|do [_ (&reader/read-text "static")
        _ _space_
        =name parse-name
        _ _space_
        =privacy-modifier parse-privacy-modifier
        _ _space_
        [_ (&lexer/$Bool =strict)] &lexer/lex-bool
        _ _space_
        =anns (with-brackets
                (spaced parse-ann))
        _ _space_
        =gvars (with-brackets
                 (spaced parse-type-param))
        _ _space_
        =exceptions (with-brackets
                      (spaced parse-gclass))
        _ _space_
        =inputs (with-brackets
                  (spaced parse-arg-decl))
        _ _space_
        =output parse-gclass
        _ _space_
        (&/$Cons =body (&/$Nil)) &parser/parse]
    (return (&/$StaticMethodSyntax (&/T [=name =privacy-modifier =strict =anns =gvars =exceptions =inputs =output =body])))))

(def ^:private parse-method-abstract-def
  (|do [_ (&reader/read-text "abstract")
        _ _space_
        =name parse-name
        _ _space_
        =privacy-modifier parse-privacy-modifier
        _ _space_
        =anns (with-brackets
                (spaced parse-ann))
        _ _space_
        =gvars (with-brackets
                 (spaced parse-type-param))
        _ _space_
        =exceptions (with-brackets
                      (spaced parse-gclass))
        _ _space_
        =inputs (with-brackets
                  (spaced parse-arg-decl))
        _ _space_
        =output parse-gclass]
    (return (&/$AbstractMethodSyntax (&/T [=name =privacy-modifier =anns =gvars =exceptions =inputs =output])))))

(def ^:private parse-method-native-def
  (|do [_ (&reader/read-text "native")
        _ _space_
        =name parse-name
        _ _space_
        =privacy-modifier parse-privacy-modifier
        _ _space_
        =anns (with-brackets
                (spaced parse-ann))
        _ _space_
        =gvars (with-brackets
                 (spaced parse-type-param))
        _ _space_
        =exceptions (with-brackets
                      (spaced parse-gclass))
        _ _space_
        =inputs (with-brackets
                  (spaced parse-arg-decl))
        _ _space_
        =output parse-gclass]
    (return (&/$NativeMethodSyntax (&/T [=name =privacy-modifier =anns =gvars =exceptions =inputs =output])))))

(def ^:private parse-method-def
  (with-parens
    (&/try-all% (&/|list parse-method-init-def
                         parse-method-virtual-def
                         parse-method-override-def
                         parse-method-static-def
                         parse-method-abstract-def
                         parse-method-native-def
                         ))))

(def ^:private parse-field
  (with-parens
    (&/try-all% (&/|list (|do [_ (&reader/read-text "constant")
                               _ _space_
                               =name parse-name
                               _ _space_
                               =anns (with-brackets
                                       (spaced parse-ann))
                               _ _space_
                               =type parse-gclass
                               _ _space_
                               (&/$Cons =value (&/$Nil)) &parser/parse]
                           (return (&/$ConstantFieldSyntax =name =anns =type =value)))

                         (|do [_ (&reader/read-text "variable")
                               _ _space_
                               =name parse-name
                               _ _space_
                               =privacy-modifier parse-privacy-modifier
                               _ _space_
                               =state-modifier parse-state-modifier
                               _ _space_
                               =anns (with-brackets
                                       (spaced parse-ann))
                               _ _space_
                               =type parse-gclass]
                           (return (&/$VariableFieldSyntax =name =privacy-modifier =state-modifier =anns =type)))
                         ))))

(def parse-interface-def
  (|do [=gclass-decl parse-gclass-decl
        =supers (with-brackets
                  (spaced parse-gclass-super))
        =anns (with-brackets
                (spaced parse-ann))
        =methods (spaced parse-method-decl)]
    (return (&/T [=gclass-decl =supers =anns =methods]))))

(def parse-class-def
  (|do [=gclass-decl parse-gclass-decl
        _ _space_
        =super-class parse-gclass-super
        _ _space_
        =interfaces (with-brackets
                      (spaced parse-gclass-super))
        _ _space_
        =inheritance-modifier parse-inheritance-modifier
        _ _space_
        =anns (with-brackets
                (spaced parse-ann))
        _ _space_
        =fields (with-brackets
                  (spaced parse-field))
        _ _space_
        =methods (with-brackets
                   (spaced parse-method-def))]
    (return (&/T [=gclass-decl =super-class =interfaces =inheritance-modifier =anns =fields =methods]))))

(def parse-anon-class-def
  (|do [=super-class parse-gclass-super
        _ _space_
        =interfaces (with-brackets
                      (spaced parse-gclass-super))
        _ _space_
        =ctor-args (with-brackets
                     (spaced parse-ctor-arg))
        _ _space_
        =methods (with-brackets
                   (spaced parse-method-def))]
    (return (&/T [=super-class =interfaces =ctor-args =methods]))))
