;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.host.generics
  (:require (clojure [string :as string]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |let |case]]))
  (:import java.util.regex.Pattern))

(do-template [<name> <old-sep> <new-sep>]
  (let [regex (-> <old-sep> Pattern/quote re-pattern)]
    (defn <name> [old]
      (string/replace old regex <new-sep>)))

  ;; ->class
  ^String ->bytecode-class-name "." "/"
  ;; ->class-name
  ^String ->class-name          "/" "."
  )

;; ->type-signature
(defn ->type-signature [class]
  (case class
    "void"    "V"
    "boolean" "Z"
    "byte"    "B"
    "short"   "S"
    "int"     "I"
    "long"    "J"
    "float"   "F"
    "double"  "D"
    "char"    "C"
    ;; else
    (let [class* (->bytecode-class-name class)]
      (if (.startsWith class* "[")
        class*
        (str "L" class* ";")))
    ))

(defn super-class-name [super]
  "(-> GenericSuperClassDecl Text)"
  (|let [[super-name super-params] super]
    super-name))

(defn class-decl-params->signature [params]
  (str "<" (->> params (&/|interpose " ") (&/fold str "")) ">"))

(defn gclass->signature [super]
  "(-> GenericClass Text)"
  (|case super
    (&/$GenericTypeVar name)
    (str "T" name ";")
    
    (&/$GenericClass name params)
    (case name
      "void"    "V"
      "boolean" "Z"
      "byte"    "B"
      "short"   "S"
      "int"     "I"
      "long"    "L"
      "float"   "F"
      "double"  "D"
      "char"    "C"
      (let [params* (str "<" (->> params (&/|map gclass->signature) (&/|interpose " ") (&/fold str "")) ">")]
        (str "L" (->bytecode-class-name name) params* ";")))

    (&/$GenericArray param)
    (str "[" (gclass->signature param))))

(defn gsuper-decl->signature [super]
  "(-> GenericSuperClassDecl Text)"
  (|let [[super-name super-params] super
         params* (str "<" (->> super-params (&/|map gclass->signature) (&/|interpose " ") (&/fold str "")) ">")]
    (str "L" (->bytecode-class-name super-name) params* ";")))

(defn gclass-decl->signature [class-decl supers]
  "(-> GenericClassDecl (List GenericSuperClassDecl) Text)"
  (|let [[class-name class-vars] class-decl
         vars-section (class-decl-params->signature class-vars)
         super-section (->> (&/|map gsuper-decl->signature supers) (&/|interpose " ") (&/fold str ""))]
    (str vars-section super-section)))

(let [object-simple-signature (->type-signature "java.lang.Object")]
  (defn gclass->simple-signature [gclass]
    "(-> GenericClass Text)"
    (|case gclass
      (&/$GenericTypeVar name)
      object-simple-signature
      
      (&/$GenericClass name params)
      (->type-signature name)

      (&/$GenericArray param)
      (str "[" (gclass->simple-signature param))

      _
      (assert false (str 'gclass->simple-signature " " (&/adt->text gclass))))))

(defn gclass->class-name [gclass]
  "(-> GenericClass Text)"
  (|case gclass
    (&/$GenericTypeVar name)
    (gclass->class-name "java.lang.Object")
    
    (&/$GenericClass name params)
    (->bytecode-class-name name)

    (&/$GenericArray param)
    (str "[" (gclass->class-name param))

    _
    (assert false (str 'gclass->class-name " " (&/adt->text gclass)))))

(let [object-bc-name (->bytecode-class-name "java.lang.Object")]
  (defn gclass->bytecode-class-name [gclass]
    "(-> GenericClass Text)"
    (|case gclass
      (&/$GenericTypeVar name)
      object-bc-name
      
      (&/$GenericClass name params)
      (->bytecode-class-name name)

      (&/$GenericArray param)
      (assert false "gclass->bytecode-class-name doesn't work on arrays."))))

(defn method-signatures [method-decl]
  (|let [[=name =anns =gvars =exceptions =inputs =output] method-decl
         simple-signature (str "(" (&/fold str "" (&/|map gclass->simple-signature =inputs)) ")" (gclass->simple-signature =output))
         generic-signature (str "<" (->> =gvars (&/|interpose " ") (&/fold str "")) ">"
                                "(" (&/fold str "" (&/|map gclass->signature =inputs)) ")"
                                (gclass->signature =output)
                                (->> =exceptions (&/|map gclass->signature) (&/|interpose " ") (&/fold str "")))]
    (&/T simple-signature generic-signature)))
