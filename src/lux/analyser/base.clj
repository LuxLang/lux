;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.analyser.base
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [deftags |let |do return* return fail |case]]
                 [type :as &type])))

;; [Tags]
(deftags
  ["bool"
   "int"
   "real"
   "char"
   "text"
   "variant"
   "tuple"
   "apply"
   "case"
   "lambda"
   "ann"
   "coerce"
   "def"
   "declare-macro"
   "var"
   "captured"

   "jvm-getstatic"
   "jvm-getfield"
   "jvm-putstatic"
   "jvm-putfield"
   "jvm-invokestatic"
   "jvm-instanceof"
   "jvm-invokevirtual"
   "jvm-invokeinterface"
   "jvm-invokespecial"
   "jvm-null?"
   "jvm-null"
   "jvm-new"
   "jvm-class"
   "jvm-interface"
   "jvm-try"
   "jvm-throw"
   "jvm-monitorenter"
   "jvm-monitorexit"
   "jvm-program"


   "jvm-znewarray"
   "jvm-zastore"
   "jvm-zaload"
   "jvm-bnewarray"
   "jvm-bastore"
   "jvm-baload"
   "jvm-snewarray"
   "jvm-sastore"
   "jvm-saload"
   "jvm-inewarray"
   "jvm-iastore"
   "jvm-iaload"
   "jvm-lnewarray"
   "jvm-lastore"
   "jvm-laload"
   "jvm-fnewarray"
   "jvm-fastore"
   "jvm-faload"
   "jvm-dnewarray"
   "jvm-dastore"
   "jvm-daload"
   "jvm-cnewarray"
   "jvm-castore"
   "jvm-caload"
   "jvm-anewarray"
   "jvm-aastore"
   "jvm-aaload"
   "jvm-arraylength"
   
   "jvm-iadd"
   "jvm-isub"
   "jvm-imul"
   "jvm-idiv"
   "jvm-irem"
   "jvm-ieq"
   "jvm-ilt"
   "jvm-igt"

   "jvm-ceq"
   "jvm-clt"
   "jvm-cgt"

   "jvm-ladd"
   "jvm-lsub"
   "jvm-lmul"
   "jvm-ldiv"
   "jvm-lrem"
   "jvm-leq"
   "jvm-llt"
   "jvm-lgt"

   "jvm-fadd"
   "jvm-fsub"
   "jvm-fmul"
   "jvm-fdiv"
   "jvm-frem"
   "jvm-feq"
   "jvm-flt"
   "jvm-fgt"

   "jvm-dadd"
   "jvm-dsub"
   "jvm-dmul"
   "jvm-ddiv"
   "jvm-drem"
   "jvm-deq"
   "jvm-dlt"
   "jvm-dgt"

   "jvm-d2f"
   "jvm-d2i"
   "jvm-d2l"

   "jvm-f2d"
   "jvm-f2i"
   "jvm-f2l"

   "jvm-i2b"
   "jvm-i2c"
   "jvm-i2d"
   "jvm-i2f"
   "jvm-i2l"
   "jvm-i2s"

   "jvm-l2d"
   "jvm-l2f"
   "jvm-l2i"

   "jvm-iand"
   "jvm-ior"
   "jvm-ixor"
   "jvm-ishl"
   "jvm-ishr"
   "jvm-iushr"

   "jvm-land"
   "jvm-lor"
   "jvm-lxor"
   "jvm-lshl"
   "jvm-lshr"
   "jvm-lushr"])

;; [Exports]
(defn expr-type* [analysis]
  (|let [[[type _] _] analysis]
    type))

(defn with-type [new-type analysis]
  (|let [[[type cursor] adt] analysis]
    (&/T [(&/T [new-type cursor]) adt])))

(defn clean-analysis [$var an]
  "(-> Type Analysis (Lux Analysis))"
  (|do [=an-type (&type/clean $var (expr-type* an))]
    (return (with-type =an-type an))))

(def jvm-this "_jvm_this")

(defn cap-1 [action]
  (|do [result action]
    (|case result
      (&/$Cons x (&/$Nil))
      (return x)

      _
      (fail "[Analyser Error] Can't expand to other than 1 element."))))

(defn analyse-1 [analyse exo-type elem]
  (cap-1 (analyse exo-type elem)))

(defn analyse-1+ [analyse ?token]
  (&type/with-var
    (fn [$var]
      (|do [=expr (analyse-1 analyse $var ?token)]
        (clean-analysis $var =expr)))))

(defn resolved-ident [ident]
  (|do [:let [[?module ?name] ident]
        module* (if (.equals "" ?module)
                  &/get-module-name
                  (return ?module))]
    (return (&/T [module* ?name]))))

(let [tag-names #{"DataT" "VoidT" "UnitT" "SumT" "ProdT" "LambdaT" "BoundT" "VarT" "ExT" "UnivQ" "ExQ" "AppT" "NamedT"}]
  (defn type-tag? [module name]
    (and (= "lux" module)
         (contains? tag-names name))))

(defn |meta [type cursor analysis]
  (&/T [(&/T [type cursor]) analysis]))
