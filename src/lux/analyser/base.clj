;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.analyser.base
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [deftags |let |do return fail |case]]
                 [type :as &type])))

;; [Tags]
(deftags ""
  "bool"
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
  "jvm-new-array"
  "jvm-aastore"
  "jvm-aaload"
  "jvm-class"
  "jvm-interface"
  "jvm-try"
  "jvm-throw"
  "jvm-monitorenter"
  "jvm-monitorexit"
  "jvm-program"

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
  "jvm-lushr"
  
  )

;; [Exports]
(defn expr-type* [syntax+]
  (|let [[_ type] syntax+]
    type))

(defn analyse-1 [analyse exo-type elem]
  (|do [output (analyse exo-type elem)]
    (|case output
      (&/$Cons x (&/$Nil))
      (return x)

      _
      (fail "[Analyser Error] Can't expand to other than 1 element."))))

(defn analyse-1+ [analyse ?token]
  (&type/with-var
    (fn [$var]
      (|do [=expr (analyse-1 analyse $var ?token)
            :let [[?item ?type] =expr]
            =type (&type/clean $var ?type)]
        (return (&/T ?item =type))))))

(defn resolved-ident [ident]
  (|let [[?module ?name] ident]
    (|do [module* (if (.equals "" ?module)
                    &/get-module-name
                    (return ?module))]
      (return (&/T module* ?name)))))
