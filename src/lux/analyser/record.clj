;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns lux.analyser.record
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [deftags |let |do return fail |case]])
            (lux.analyser [base :as &&]
                          [module :as &&module])))

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
(defn order-record [pairs]
  "(-> (List (, Syntax Syntax)) (Lux (List Syntax)))"
  (|do [tag-group (|case pairs
                    (&/$Nil)
                    (return (&/|list))
                    
                    (&/$Cons [(&/$Meta _ (&/$TagS tag1)) _] _)
                    (|do [[module name] (&&/resolved-ident tag1)]
                      (&&module/tag-group module name))

                    _
                    (fail "[Analyser Error] Wrong syntax for records. Odd elements must be tags."))
        =pairs (&/map% (fn [kv]
                         (|case kv
                           [(&/$Meta _ (&/$TagS k)) v]
                           (|do [=k (&&/resolved-ident k)]
                             (return (&/T (&/ident->text =k) v)))

                           _
                           (fail "[Analyser Error] Wrong syntax for records. Odd elements must be tags.")))
                       pairs)]
    (&/map% (fn [tag]
              (if-let [member (&/|get tag =pairs)]
                (return member)
                (fail (str "[Analyser Error] Unknown tag: " tag))))
            (&/|map &/ident->text tag-group))))
