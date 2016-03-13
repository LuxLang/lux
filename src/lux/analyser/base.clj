;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.analyser.base
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [defvariant |let |do return* return fail |case]]
                 [type :as &type])))

;; [Tags]
(defvariant
  ("bool" 1)
  ("int" 1)
  ("real" 1)
  ("char" 1)
  ("text" 1)
  ("variant" 3)
  ("tuple" 1)
  ("apply" 2)
  ("case" 2)
  ("lambda" 3)
  ("ann" 3)
  ("coerce" 3)
  ("def" 3)
  ("declare-macro" 1)
  ("var" 1)
  ("captured" 1)

  ("jvm-getstatic" 1)
  ("jvm-getfield" 1)
  ("jvm-putstatic" 1)
  ("jvm-putfield" 1)
  ("jvm-invokestatic" 1)
  ("jvm-instanceof" 1)
  ("jvm-invokevirtual" 1)
  ("jvm-invokeinterface" 1)
  ("jvm-invokespecial" 1)
  ("jvm-null?" 1)
  ("jvm-null" 0)
  ("jvm-new" 1)
  ("jvm-class" 1)
  ("jvm-interface" 1)
  ("jvm-try" 1)
  ("jvm-throw" 1)
  ("jvm-monitorenter" 1)
  ("jvm-monitorexit" 1)
  ("jvm-program" 1)

  ("jvm-znewarray" 1)
  ("jvm-zastore" 1)
  ("jvm-zaload" 1)
  ("jvm-bnewarray" 1)
  ("jvm-bastore" 1)
  ("jvm-baload" 1)
  ("jvm-snewarray" 1)
  ("jvm-sastore" 1)
  ("jvm-saload" 1)
  ("jvm-inewarray" 1)
  ("jvm-iastore" 1)
  ("jvm-iaload" 1)
  ("jvm-lnewarray" 1)
  ("jvm-lastore" 1)
  ("jvm-laload" 1)
  ("jvm-fnewarray" 1)
  ("jvm-fastore" 1)
  ("jvm-faload" 1)
  ("jvm-dnewarray" 1)
  ("jvm-dastore" 1)
  ("jvm-daload" 1)
  ("jvm-cnewarray" 1)
  ("jvm-castore" 1)
  ("jvm-caload" 1)
  ("jvm-anewarray" 1)
  ("jvm-aastore" 1)
  ("jvm-aaload" 1)
  ("jvm-arraylength" 1)
  
  ("jvm-iadd" 1)
  ("jvm-isub" 1)
  ("jvm-imul" 1)
  ("jvm-idiv" 1)
  ("jvm-irem" 1)
  ("jvm-ieq" 1)
  ("jvm-ilt" 1)
  ("jvm-igt" 1)

  ("jvm-ceq" 1)
  ("jvm-clt" 1)
  ("jvm-cgt" 1)

  ("jvm-ladd" 1)
  ("jvm-lsub" 1)
  ("jvm-lmul" 1)
  ("jvm-ldiv" 1)
  ("jvm-lrem" 1)
  ("jvm-leq" 1)
  ("jvm-llt" 1)
  ("jvm-lgt" 1)

  ("jvm-fadd" 1)
  ("jvm-fsub" 1)
  ("jvm-fmul" 1)
  ("jvm-fdiv" 1)
  ("jvm-frem" 1)
  ("jvm-feq" 1)
  ("jvm-flt" 1)
  ("jvm-fgt" 1)

  ("jvm-dadd" 1)
  ("jvm-dsub" 1)
  ("jvm-dmul" 1)
  ("jvm-ddiv" 1)
  ("jvm-drem" 1)
  ("jvm-deq" 1)
  ("jvm-dlt" 1)
  ("jvm-dgt" 1)

  ("jvm-d2f" 1)
  ("jvm-d2i" 1)
  ("jvm-d2l" 1)

  ("jvm-f2d" 1)
  ("jvm-f2i" 1)
  ("jvm-f2l" 1)

  ("jvm-i2b" 1)
  ("jvm-i2c" 1)
  ("jvm-i2d" 1)
  ("jvm-i2f" 1)
  ("jvm-i2l" 1)
  ("jvm-i2s" 1)

  ("jvm-l2d" 1)
  ("jvm-l2f" 1)
  ("jvm-l2i" 1)

  ("jvm-c2b" 1)
  ("jvm-c2s" 1)
  ("jvm-c2i" 1)
  ("jvm-c2l" 1)

  ("jvm-iand" 1)
  ("jvm-ior" 1)
  ("jvm-ixor" 1)
  ("jvm-ishl" 1)
  ("jvm-ishr" 1)
  ("jvm-iushr" 1)

  ("jvm-land" 1)
  ("jvm-lor" 1)
  ("jvm-lxor" 1)
  ("jvm-lshl" 1)
  ("jvm-lshr" 1)
  ("jvm-lushr" 1))

;; [Exports]
(defn expr-type* [analysis]
  (|let [[[type _] _] analysis]
    type))

(defn expr-term [analysis]
  (|let [[[type _] term] analysis]
    term))

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
