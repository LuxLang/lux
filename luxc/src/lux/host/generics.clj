(ns lux.host.generics
  (:require (clojure [string :as string]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return |let |case]]))
  (:import java.util.regex.Pattern))

(declare gclass->signature)

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

(defn formal-type-parameter->signature [param]
  (|let [[pname pbounds] param]
    (|case pbounds
      (&/$Nil)
      pname

      _
      (->> pbounds
           (&/|map (fn [pbound] (str ": " (gclass->signature pbound))))
           (&/|interpose " ")
           (str pname " "))
      )))

(defn formal-type-parameters->signature [params]
  (if (&/|empty? params)
    ""
    (str "<" (->> params (&/|map formal-type-parameter->signature) (&/|interpose " ") (&/fold str "")) ">")))

(defn gclass->signature [super]
  "(-> GenericClass Text)"
  (|case super
    (&/$GenericTypeVar name)
    (str "T" name ";")

    (&/$GenericWildcard (&/$None))
    "*"

    (&/$GenericWildcard (&/$Some [(&/$UpperBound) ?bound]))
    (str "+" (gclass->signature ?bound))

    (&/$GenericWildcard (&/$Some [(&/$LowerBound) ?bound]))
    (str "-" (gclass->signature ?bound))
    
    (&/$GenericClass ^String name params)
    (case name
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
      (if (.startsWith name "[")
        name
        (let [params* (if (&/|empty? params)
                        ""
                        (str "<" (->> params (&/|map gclass->signature) (&/|interpose "") (&/fold str "")) ">"))]
          (str "L" (->bytecode-class-name name) params* ";"))))

    (&/$GenericArray param)
    (str "[" (gclass->signature param))))

(defn gsuper-decl->signature [super]
  "(-> GenericSuperClassDecl Text)"
  (|let [[super-name super-params] super
         params* (if (&/|empty? super-params)
                   ""
                   (str "<" (->> super-params (&/|map gclass->signature) (&/|interpose " ") (&/fold str "")) ">"))]
    (str "L" (->bytecode-class-name super-name) params* ";")))

(defn gclass-decl->signature [class-decl supers]
  "(-> GenericClassDecl (List GenericSuperClassDecl) Text)"
  (|let [[class-name class-vars] class-decl
         vars-section (formal-type-parameters->signature class-vars)
         super-section (->> (&/|map gsuper-decl->signature supers) (&/|interpose " ") (&/fold str ""))]
    (str vars-section super-section)))

(let [object-simple-signature (->type-signature "java.lang.Object")]
  (defn gclass->simple-signature [gclass]
    "(-> GenericClass Text)"
    (|case gclass
      (&/$GenericTypeVar name)
      object-simple-signature

      (&/$GenericWildcard _)
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
    (->bytecode-class-name "java.lang.Object")

    (&/$GenericWildcard _)
    (->bytecode-class-name "java.lang.Object")
    
    (&/$GenericClass name params)
    (->bytecode-class-name name)

    (&/$GenericArray param)
    (str "[" (gclass->class-name param))

    _
    (assert false (str 'gclass->class-name " " (&/adt->text gclass)))))

(let [object-bc-name (->bytecode-class-name "java.lang.Object")]
  (defn gclass->bytecode-class-name* [gclass type-env]
    "(-> GenericClass Text)"
    (|case gclass
      (&/$GenericTypeVar name)
      object-bc-name

      (&/$GenericWildcard _)
      object-bc-name
      
      (&/$GenericClass name params)
      ;; When referring to type-parameters during class or method
      ;; definition, a type-environment is set for storing the names
      ;; of such parameters.
      ;; When a "class" shows up with the name of one of those
      ;; parameters, it must be detected, and the bytecode class-name
      ;; must correspond to Object's.
      (if (&/|get name type-env)
        object-bc-name
        (->bytecode-class-name name))

      (&/$GenericArray param)
      (assert false "gclass->bytecode-class-name* does not work on arrays."))))

(let [object-bc-name (->bytecode-class-name "java.lang.Object")]
  (defn gclass->bytecode-class-name [gclass]
    "(-> GenericClass Text)"
    (|case gclass
      (&/$GenericTypeVar name)
      object-bc-name

      (&/$GenericWildcard _)
      object-bc-name
      
      (&/$GenericClass name params)
      (->bytecode-class-name name)

      (&/$GenericArray param)
      (assert false "gclass->bytecode-class-name does not work on arrays."))))

(defn method-signatures [method-decl]
  (|let [[=name =anns =gvars =exceptions =inputs =output] method-decl
         simple-signature (str "(" (&/fold str "" (&/|map gclass->simple-signature =inputs)) ")" (gclass->simple-signature =output))
         generic-signature (str (formal-type-parameters->signature =gvars)
                                "(" (&/fold str "" (&/|map gclass->signature =inputs)) ")"
                                (gclass->signature =output)
                                (->> =exceptions (&/|map gclass->signature) (&/|interpose " ") (&/fold str "")))]
    (&/T [simple-signature generic-signature])))
