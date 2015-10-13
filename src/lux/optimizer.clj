;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.optimizer
  (:require (lux [base :as & :refer [|let |do return fail return* fail* |case deftags]]
                 [analyser :as &analyser])
            [lux.analyser.base :as &-base]))

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

(defn ^:private optimize-token [analysis]
  "(-> Analysis Optimized)"
  (|case analysis
    (&-base/$bool value)
    (return (&/V $bool value))
    
    (&-base/$int value)
    (return (&/V $int value))
    
    (&-base/$real value)
    (return (&/V $real value))
    
    (&-base/$char value)
    (return (&/V $char value))
    
    (&-base/$text value)
    (return (&/V $text value))
    
    (&-base/$variant value)
    (return (&/V $variant value))
    
    (&-base/$tuple value)
    (return (&/V $tuple value))
    
    (&-base/$apply value)
    (return (&/V $apply value))
    
    (&-base/$case value)
    (return (&/V $case value))
    
    (&-base/$lambda value)
    (return (&/V $lambda value))
    
    (&-base/$ann value)
    (return (&/V $ann value))
    
    (&-base/$coerce value)
    (return (&/V $coerce value))
    
    (&-base/$def value)
    (return (&/V $def value))
    
    (&-base/$declare-macro value)
    (return (&/V $declare-macro value))
    
    (&-base/$var value)
    (return (&/V $var value))
    
    (&-base/$captured value)
    (return (&/V $captured value))
    
    (&-base/$jvm-getstatic value)
    (return (&/V $jvm-getstatic value))
    
    (&-base/$jvm-getfield value)
    (return (&/V $jvm-getfield value))
    
    (&-base/$jvm-putstatic value)
    (return (&/V $jvm-putstatic value))
    
    (&-base/$jvm-putfield value)
    (return (&/V $jvm-putfield value))
    
    (&-base/$jvm-invokestatic value)
    (return (&/V $jvm-invokestatic value))
    
    (&-base/$jvm-instanceof value)
    (return (&/V $jvm-instanceof value))
    
    (&-base/$jvm-invokevirtual value)
    (return (&/V $jvm-invokevirtual value))
    
    (&-base/$jvm-invokeinterface value)
    (return (&/V $jvm-invokeinterface value))
    
    (&-base/$jvm-invokespecial value)
    (return (&/V $jvm-invokespecial value))
    
    (&-base/$jvm-null? value)
    (return (&/V $jvm-null? value))
    
    (&-base/$jvm-null value)
    (return (&/V $jvm-null value))
    
    (&-base/$jvm-new value)
    (return (&/V $jvm-new value))
    
    (&-base/$jvm-class value)
    (return (&/V $jvm-class value))
    
    (&-base/$jvm-interface value)
    (return (&/V $jvm-interface value))
    
    (&-base/$jvm-try value)
    (return (&/V $jvm-try value))
    
    (&-base/$jvm-throw value)
    (return (&/V $jvm-throw value))
    
    (&-base/$jvm-monitorenter value)
    (return (&/V $jvm-monitorenter value))
    
    (&-base/$jvm-monitorexit value)
    (return (&/V $jvm-monitorexit value))
    
    (&-base/$jvm-program value)
    (return (&/V $jvm-program value))
    
    (&-base/$jvm-znewarray value)
    (return (&/V $jvm-znewarray value))
    
    (&-base/$jvm-zastore value)
    (return (&/V $jvm-zastore value))
    
    (&-base/$jvm-zaload value)
    (return (&/V $jvm-zaload value))
    
    (&-base/$jvm-bnewarray value)
    (return (&/V $jvm-bnewarray value))
    
    (&-base/$jvm-bastore value)
    (return (&/V $jvm-bastore value))
    
    (&-base/$jvm-baload value)
    (return (&/V $jvm-baload value))
    
    (&-base/$jvm-snewarray value)
    (return (&/V $jvm-snewarray value))
    
    (&-base/$jvm-sastore value)
    (return (&/V $jvm-sastore value))
    
    (&-base/$jvm-saload value)
    (return (&/V $jvm-saload value))
    
    (&-base/$jvm-inewarray value)
    (return (&/V $jvm-inewarray value))
    
    (&-base/$jvm-iastore value)
    (return (&/V $jvm-iastore value))
    
    (&-base/$jvm-iaload value)
    (return (&/V $jvm-iaload value))
    
    (&-base/$jvm-lnewarray value)
    (return (&/V $jvm-lnewarray value))
    
    (&-base/$jvm-lastore value)
    (return (&/V $jvm-lastore value))
    
    (&-base/$jvm-laload value)
    (return (&/V $jvm-laload value))
    
    (&-base/$jvm-fnewarray value)
    (return (&/V $jvm-fnewarray value))
    
    (&-base/$jvm-fastore value)
    (return (&/V $jvm-fastore value))
    
    (&-base/$jvm-faload value)
    (return (&/V $jvm-faload value))
    
    (&-base/$jvm-dnewarray value)
    (return (&/V $jvm-dnewarray value))
    
    (&-base/$jvm-dastore value)
    (return (&/V $jvm-dastore value))
    
    (&-base/$jvm-daload value)
    (return (&/V $jvm-daload value))
    
    (&-base/$jvm-cnewarray value)
    (return (&/V $jvm-cnewarray value))
    
    (&-base/$jvm-castore value)
    (return (&/V $jvm-castore value))
    
    (&-base/$jvm-caload value)
    (return (&/V $jvm-caload value))
    
    (&-base/$jvm-anewarray value)
    (return (&/V $jvm-anewarray value))
    
    (&-base/$jvm-aastore value)
    (return (&/V $jvm-aastore value))
    
    (&-base/$jvm-aaload value)
    (return (&/V $jvm-aaload value))
    
    (&-base/$jvm-arraylength value)
    (return (&/V $jvm-arraylength value))
    
    (&-base/$jvm-iadd value)
    (return (&/V $jvm-iadd value))
    
    (&-base/$jvm-isub value)
    (return (&/V $jvm-isub value))
    
    (&-base/$jvm-imul value)
    (return (&/V $jvm-imul value))
    
    (&-base/$jvm-idiv value)
    (return (&/V $jvm-idiv value))
    
    (&-base/$jvm-irem value)
    (return (&/V $jvm-irem value))
    
    (&-base/$jvm-ieq value)
    (return (&/V $jvm-ieq value))
    
    (&-base/$jvm-ilt value)
    (return (&/V $jvm-ilt value))
    
    (&-base/$jvm-igt value)
    (return (&/V $jvm-igt value))
    
    (&-base/$jvm-ceq value)
    (return (&/V $jvm-ceq value))
    
    (&-base/$jvm-clt value)
    (return (&/V $jvm-clt value))
    
    (&-base/$jvm-cgt value)
    (return (&/V $jvm-cgt value))
    
    (&-base/$jvm-ladd value)
    (return (&/V $jvm-ladd value))
    
    (&-base/$jvm-lsub value)
    (return (&/V $jvm-lsub value))
    
    (&-base/$jvm-lmul value)
    (return (&/V $jvm-lmul value))
    
    (&-base/$jvm-ldiv value)
    (return (&/V $jvm-ldiv value))
    
    (&-base/$jvm-lrem value)
    (return (&/V $jvm-lrem value))
    
    (&-base/$jvm-leq value)
    (return (&/V $jvm-leq value))
    
    (&-base/$jvm-llt value)
    (return (&/V $jvm-llt value))
    
    (&-base/$jvm-lgt value)
    (return (&/V $jvm-lgt value))
    
    (&-base/$jvm-fadd value)
    (return (&/V $jvm-fadd value))
    
    (&-base/$jvm-fsub value)
    (return (&/V $jvm-fsub value))
    
    (&-base/$jvm-fmul value)
    (return (&/V $jvm-fmul value))
    
    (&-base/$jvm-fdiv value)
    (return (&/V $jvm-fdiv value))
    
    (&-base/$jvm-frem value)
    (return (&/V $jvm-frem value))
    
    (&-base/$jvm-feq value)
    (return (&/V $jvm-feq value))
    
    (&-base/$jvm-flt value)
    (return (&/V $jvm-flt value))
    
    (&-base/$jvm-fgt value)
    (return (&/V $jvm-fgt value))
    
    (&-base/$jvm-dadd value)
    (return (&/V $jvm-dadd value))
    
    (&-base/$jvm-dsub value)
    (return (&/V $jvm-dsub value))
    
    (&-base/$jvm-dmul value)
    (return (&/V $jvm-dmul value))
    
    (&-base/$jvm-ddiv value)
    (return (&/V $jvm-ddiv value))
    
    (&-base/$jvm-drem value)
    (return (&/V $jvm-drem value))
    
    (&-base/$jvm-deq value)
    (return (&/V $jvm-deq value))
    
    (&-base/$jvm-dlt value)
    (return (&/V $jvm-dlt value))
    
    (&-base/$jvm-dgt value)
    (return (&/V $jvm-dgt value))
    
    (&-base/$jvm-d2f value)
    (return (&/V $jvm-d2f value))
    
    (&-base/$jvm-d2i value)
    (return (&/V $jvm-d2i value))
    
    (&-base/$jvm-d2l value)
    (return (&/V $jvm-d2l value))
    
    (&-base/$jvm-f2d value)
    (return (&/V $jvm-f2d value))
    
    (&-base/$jvm-f2i value)
    (return (&/V $jvm-f2i value))
    
    (&-base/$jvm-f2l value)
    (return (&/V $jvm-f2l value))
    
    (&-base/$jvm-i2b value)
    (return (&/V $jvm-i2b value))
    
    (&-base/$jvm-i2c value)
    (return (&/V $jvm-i2c value))
    
    (&-base/$jvm-i2d value)
    (return (&/V $jvm-i2d value))
    
    (&-base/$jvm-i2f value)
    (return (&/V $jvm-i2f value))
    
    (&-base/$jvm-i2l value)
    (return (&/V $jvm-i2l value))
    
    (&-base/$jvm-i2s value)
    (return (&/V $jvm-i2s value))
    
    (&-base/$jvm-l2d value)
    (return (&/V $jvm-l2d value))
    
    (&-base/$jvm-l2f value)
    (return (&/V $jvm-l2f value))
    
    (&-base/$jvm-l2i value)
    (return (&/V $jvm-l2i value))
    
    (&-base/$jvm-iand value)
    (return (&/V $jvm-iand value))
    
    (&-base/$jvm-ior value)
    (return (&/V $jvm-ior value))
    
    (&-base/$jvm-ixor value)
    (return (&/V $jvm-ixor value))
    
    (&-base/$jvm-ishl value)
    (return (&/V $jvm-ishl value))
    
    (&-base/$jvm-ishr value)
    (return (&/V $jvm-ishr value))
    
    (&-base/$jvm-iushr value)
    (return (&/V $jvm-iushr value))
    
    (&-base/$jvm-land value)
    (return (&/V $jvm-land value))
    
    (&-base/$jvm-lor value)
    (return (&/V $jvm-lor value))
    
    (&-base/$jvm-lxor value)
    (return (&/V $jvm-lxor value))
    
    (&-base/$jvm-lshl value)
    (return (&/V $jvm-lshl value))
    
    (&-base/$jvm-lshr value)
    (return (&/V $jvm-lshr value))
    
    (&-base/$jvm-lushr value)
    (return (&/V $jvm-lushr value))
    ))

;; [Exports]
(defn optimize [eval! compile-module compile-token]
  (|do [analyses (&analyser/analyse eval! compile-module compile-token)]
    (&/map% optimize-token analyses)))
