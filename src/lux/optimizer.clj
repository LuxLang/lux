;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.optimizer
  (:require (lux [base :as & :refer [|let |do return fail return* fail* |case defvariant]]
                 [analyser :as &analyser])
            [lux.analyser.base :as &-base]))

;; [Tags]
(defvariant
  ("bool" 1)
  ("int" 1)
  ("real" 1)
  ("char" 1)
  ("text" 1)
  ("variant" 1)
  ("tuple" 1)
  ("apply" 1)
  ("case" 1)
  ("lambda" 1)
  ("ann" 1)
  ("coerce" 1)
  ("def" 1)
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
  ("jvm-null" 1)
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

(defn ^:private optimize-token [analysis]
  "(-> Analysis Optimized)"
  (|case analysis
    (&-base/$bool value)
    (return ($bool value))
    
    (&-base/$int value)
    (return ($int value))
    
    (&-base/$real value)
    (return ($real value))
    
    (&-base/$char value)
    (return ($char value))
    
    (&-base/$text value)
    (return ($text value))
    
    (&-base/$variant value)
    (return ($variant value))
    
    (&-base/$tuple value)
    (return ($tuple value))
    
    (&-base/$apply value)
    (return ($apply value))
    
    (&-base/$case value)
    (return ($case value))
    
    (&-base/$lambda value)
    (return ($lambda value))
    
    (&-base/$ann value)
    (return ($ann value))
    
    (&-base/$coerce value)
    (return ($coerce value))
    
    (&-base/$def value)
    (return ($def value))
    
    (&-base/$declare-macro value)
    (return ($declare-macro value))
    
    (&-base/$var value)
    (return ($var value))
    
    (&-base/$captured value)
    (return ($captured value))
    
    (&-base/$jvm-getstatic value)
    (return ($jvm-getstatic value))
    
    (&-base/$jvm-getfield value)
    (return ($jvm-getfield value))
    
    (&-base/$jvm-putstatic value)
    (return ($jvm-putstatic value))
    
    (&-base/$jvm-putfield value)
    (return ($jvm-putfield value))
    
    (&-base/$jvm-invokestatic value)
    (return ($jvm-invokestatic value))
    
    (&-base/$jvm-instanceof value)
    (return ($jvm-instanceof value))
    
    (&-base/$jvm-invokevirtual value)
    (return ($jvm-invokevirtual value))
    
    (&-base/$jvm-invokeinterface value)
    (return ($jvm-invokeinterface value))
    
    (&-base/$jvm-invokespecial value)
    (return ($jvm-invokespecial value))
    
    (&-base/$jvm-null? value)
    (return ($jvm-null? value))
    
    (&-base/$jvm-null value)
    (return ($jvm-null value))
    
    (&-base/$jvm-new value)
    (return ($jvm-new value))
    
    (&-base/$jvm-class value)
    (return ($jvm-class value))
    
    (&-base/$jvm-interface value)
    (return ($jvm-interface value))
    
    (&-base/$jvm-try value)
    (return ($jvm-try value))
    
    (&-base/$jvm-throw value)
    (return ($jvm-throw value))
    
    (&-base/$jvm-monitorenter value)
    (return ($jvm-monitorenter value))
    
    (&-base/$jvm-monitorexit value)
    (return ($jvm-monitorexit value))
    
    (&-base/$jvm-program value)
    (return ($jvm-program value))
    
    (&-base/$jvm-znewarray value)
    (return ($jvm-znewarray value))
    
    (&-base/$jvm-zastore value)
    (return ($jvm-zastore value))
    
    (&-base/$jvm-zaload value)
    (return ($jvm-zaload value))
    
    (&-base/$jvm-bnewarray value)
    (return ($jvm-bnewarray value))
    
    (&-base/$jvm-bastore value)
    (return ($jvm-bastore value))
    
    (&-base/$jvm-baload value)
    (return ($jvm-baload value))
    
    (&-base/$jvm-snewarray value)
    (return ($jvm-snewarray value))
    
    (&-base/$jvm-sastore value)
    (return ($jvm-sastore value))
    
    (&-base/$jvm-saload value)
    (return ($jvm-saload value))
    
    (&-base/$jvm-inewarray value)
    (return ($jvm-inewarray value))
    
    (&-base/$jvm-iastore value)
    (return ($jvm-iastore value))
    
    (&-base/$jvm-iaload value)
    (return ($jvm-iaload value))
    
    (&-base/$jvm-lnewarray value)
    (return ($jvm-lnewarray value))
    
    (&-base/$jvm-lastore value)
    (return ($jvm-lastore value))
    
    (&-base/$jvm-laload value)
    (return ($jvm-laload value))
    
    (&-base/$jvm-fnewarray value)
    (return ($jvm-fnewarray value))
    
    (&-base/$jvm-fastore value)
    (return ($jvm-fastore value))
    
    (&-base/$jvm-faload value)
    (return ($jvm-faload value))
    
    (&-base/$jvm-dnewarray value)
    (return ($jvm-dnewarray value))
    
    (&-base/$jvm-dastore value)
    (return ($jvm-dastore value))
    
    (&-base/$jvm-daload value)
    (return ($jvm-daload value))
    
    (&-base/$jvm-cnewarray value)
    (return ($jvm-cnewarray value))
    
    (&-base/$jvm-castore value)
    (return ($jvm-castore value))
    
    (&-base/$jvm-caload value)
    (return ($jvm-caload value))
    
    (&-base/$jvm-anewarray value)
    (return ($jvm-anewarray value))
    
    (&-base/$jvm-aastore value)
    (return ($jvm-aastore value))
    
    (&-base/$jvm-aaload value)
    (return ($jvm-aaload value))
    
    (&-base/$jvm-arraylength value)
    (return ($jvm-arraylength value))
    
    (&-base/$jvm-iadd value)
    (return ($jvm-iadd value))
    
    (&-base/$jvm-isub value)
    (return ($jvm-isub value))
    
    (&-base/$jvm-imul value)
    (return ($jvm-imul value))
    
    (&-base/$jvm-idiv value)
    (return ($jvm-idiv value))
    
    (&-base/$jvm-irem value)
    (return ($jvm-irem value))
    
    (&-base/$jvm-ieq value)
    (return ($jvm-ieq value))
    
    (&-base/$jvm-ilt value)
    (return ($jvm-ilt value))
    
    (&-base/$jvm-igt value)
    (return ($jvm-igt value))
    
    (&-base/$jvm-ceq value)
    (return ($jvm-ceq value))
    
    (&-base/$jvm-clt value)
    (return ($jvm-clt value))
    
    (&-base/$jvm-cgt value)
    (return ($jvm-cgt value))
    
    (&-base/$jvm-ladd value)
    (return ($jvm-ladd value))
    
    (&-base/$jvm-lsub value)
    (return ($jvm-lsub value))
    
    (&-base/$jvm-lmul value)
    (return ($jvm-lmul value))
    
    (&-base/$jvm-ldiv value)
    (return ($jvm-ldiv value))
    
    (&-base/$jvm-lrem value)
    (return ($jvm-lrem value))
    
    (&-base/$jvm-leq value)
    (return ($jvm-leq value))
    
    (&-base/$jvm-llt value)
    (return ($jvm-llt value))
    
    (&-base/$jvm-lgt value)
    (return ($jvm-lgt value))
    
    (&-base/$jvm-fadd value)
    (return ($jvm-fadd value))
    
    (&-base/$jvm-fsub value)
    (return ($jvm-fsub value))
    
    (&-base/$jvm-fmul value)
    (return ($jvm-fmul value))
    
    (&-base/$jvm-fdiv value)
    (return ($jvm-fdiv value))
    
    (&-base/$jvm-frem value)
    (return ($jvm-frem value))
    
    (&-base/$jvm-feq value)
    (return ($jvm-feq value))
    
    (&-base/$jvm-flt value)
    (return ($jvm-flt value))
    
    (&-base/$jvm-fgt value)
    (return ($jvm-fgt value))
    
    (&-base/$jvm-dadd value)
    (return ($jvm-dadd value))
    
    (&-base/$jvm-dsub value)
    (return ($jvm-dsub value))
    
    (&-base/$jvm-dmul value)
    (return ($jvm-dmul value))
    
    (&-base/$jvm-ddiv value)
    (return ($jvm-ddiv value))
    
    (&-base/$jvm-drem value)
    (return ($jvm-drem value))
    
    (&-base/$jvm-deq value)
    (return ($jvm-deq value))
    
    (&-base/$jvm-dlt value)
    (return ($jvm-dlt value))
    
    (&-base/$jvm-dgt value)
    (return ($jvm-dgt value))
    
    (&-base/$jvm-d2f value)
    (return ($jvm-d2f value))
    
    (&-base/$jvm-d2i value)
    (return ($jvm-d2i value))
    
    (&-base/$jvm-d2l value)
    (return ($jvm-d2l value))
    
    (&-base/$jvm-f2d value)
    (return ($jvm-f2d value))
    
    (&-base/$jvm-f2i value)
    (return ($jvm-f2i value))
    
    (&-base/$jvm-f2l value)
    (return ($jvm-f2l value))
    
    (&-base/$jvm-i2b value)
    (return ($jvm-i2b value))
    
    (&-base/$jvm-i2c value)
    (return ($jvm-i2c value))
    
    (&-base/$jvm-i2d value)
    (return ($jvm-i2d value))
    
    (&-base/$jvm-i2f value)
    (return ($jvm-i2f value))
    
    (&-base/$jvm-i2l value)
    (return ($jvm-i2l value))
    
    (&-base/$jvm-i2s value)
    (return ($jvm-i2s value))
    
    (&-base/$jvm-l2d value)
    (return ($jvm-l2d value))
    
    (&-base/$jvm-l2f value)
    (return ($jvm-l2f value))
    
    (&-base/$jvm-l2i value)
    (return ($jvm-l2i value))
    
    (&-base/$jvm-iand value)
    (return ($jvm-iand value))
    
    (&-base/$jvm-ior value)
    (return ($jvm-ior value))
    
    (&-base/$jvm-ixor value)
    (return ($jvm-ixor value))
    
    (&-base/$jvm-ishl value)
    (return ($jvm-ishl value))
    
    (&-base/$jvm-ishr value)
    (return ($jvm-ishr value))
    
    (&-base/$jvm-iushr value)
    (return ($jvm-iushr value))
    
    (&-base/$jvm-land value)
    (return ($jvm-land value))
    
    (&-base/$jvm-lor value)
    (return ($jvm-lor value))
    
    (&-base/$jvm-lxor value)
    (return ($jvm-lxor value))
    
    (&-base/$jvm-lshl value)
    (return ($jvm-lshl value))
    
    (&-base/$jvm-lshr value)
    (return ($jvm-lshr value))
    
    (&-base/$jvm-lushr value)
    (return ($jvm-lushr value))
    ))

;; [Exports]
(defn optimize [eval! compile-module compile-token]
  (|do [analyses (&analyser/analyse eval! compile-module compile-token)]
    (&/map% optimize-token analyses)))
