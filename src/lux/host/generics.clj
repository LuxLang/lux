;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.host.generics
  (:require (clojure [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |let |case]]
                 [host :as &host])))

(defn super-class-name [super]
  "(-> GenericSuperClassDecl Text)"
  (|let [[super-name super-params] super]
    super-name))

(defn gclass->signature [super]
  "(-> GenericClass Text)"
  (|case super
    (&/$GenericTypeVar name)
    (str "T" name ";")
    
    (&/$GenericClass name params)
    (|let [params-sigs (->> params (&/|map gclass->signature) (&/|interpose " ") (&/fold str ""))]
      (str "L" (&host/->class name) "<" params-sigs ">" ";"))

    (&/$GenericArray param)
    (str "[" (gclass->signature param))))

(defn gsuper-decl->signature [super]
  "(-> GenericSuperClassDecl Text)"
  (|let [[super-name super-params] super
         params-sigs (->> super-params (&/|map gclass->signature) (&/|interpose " ") (&/fold str ""))]
    (str "L" (&host/->class super-name) "<" params-sigs ">" ";")))

(defn gclass-decl->signature [class-decl supers]
  "(-> GenericClassDecl (List GenericSuperClassDecl) Text)"
  (|let [[class-name class-vars] class-decl
         vars-section (str "<" (->> class-vars (&/|interpose " ") (&/fold str "")) ">")
         super-section (->> (&/|map gsuper-decl->signature supers) (&/|interpose " ") (&/fold str ""))]
    (str vars-section super-section)))

(let [object-simple-signature (&host/->type-signature "java.lang.Object")]
  (defn gclass->simple-signature [gclass]
    "(-> GenericClass Text)"
    (|case gclass
      (&/$GenericTypeVar name)
      object-simple-signature
      
      (&/$GenericClass name params)
      (&host/->type-signature name)

      (&/$GenericArray param)
      (str "[" (gclass->simple-signature param)))))
