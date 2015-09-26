;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.analyser.host
  (:require (clojure [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return fail |case]]
                 [parser :as &parser]
                 [type :as &type]
                 [host :as &host])
            (lux.analyser [base :as &&]
                          [lambda :as &&lambda]
                          [env :as &&env])
            [lux.compiler.base :as &c!base]))

;; [Utils]
(defn ^:private extract-text [ast]
  (|case ast
    [_ (&/$TextS text)]
    (return text)

    _
    (fail "[Analyser/Host Error] Can't extract text.")))

(defn ^:private ensure-catching [exceptions]
  "(-> (List Text) (Lux (,)))"
  (|do [class-loader &/loader]
    (fn [state]
      (let [exceptions (&/|map #(Class/forName % true class-loader) exceptions)
            catching (->> state (&/get$ &/$host) (&/get$ &/$catching)
                          (&/|map #(Class/forName % true class-loader)))]
        (if-let [missing-ex (&/fold (fn [prev now]
                                      (or prev
                                          (if (&/fold (fn [found? ex-catch]
                                                        (or found?
                                                            (.isAssignableFrom ex-catch now)))
                                                      false
                                                      catching)
                                            nil
                                            now)))
                                    nil
                                    exceptions)]
          (assert false (str "[Analyser Error] Unhandled exception: " missing-ex))
          ;; (&/fail* (str "[Analyser Error] Unhandled exception: " missing-ex))
          (&/return* state nil)))
      )))

(defn ^:private with-catches [catches body]
  "(All [a] (-> (List Text) (Lux a) (Lux a)))"
  (fn [state]
    (let [;; _ (prn 'with-catches/_0 (&/->seq catches))
          old-catches (->> state (&/get$ &/$host) (&/get$ &/$catching))
          ;; _ (prn 'with-catches/_1 (&/->seq (->> state (&/get$ &/$host) (&/get$ &/$catching))))
          state* (->> state (&/update$ &/$host #(&/update$ &/$catching (partial &/|++ catches) %)))
          ;; _ (prn 'with-catches/_2 (&/->seq (->> state* (&/get$ &/$host) (&/get$ &/$catching))))
          ]
      (|case (&/run-state body state*)
        (&/$Left msg)
        (&/V &/$Left msg)

        (&/$Right state** output)
        (do ;; (prn 'with-catches/_3 (&/->seq (->> state** (&/get$ &/$host) (&/get$ &/$catching))))
            (&/V &/$Right (&/T (->> state** (&/update$ &/$host #(&/set$ &/$catching old-catches %)))
                               output)))))
    ))

(defn ^:private ensure-object [token]
  "(-> Analysis (Lux (,)))"
  (|case token
    [_ (&/$DataT _ _)]
    (return nil)

    _
    (fail "[Analyser Error] Expecting object")))

(defn ^:private as-object [type]
  "(-> Type Type)"
  (|case type
    (&/$DataT class params)
    (&type/Data$ (&type/as-obj class) params)

    _
    type))

(defn ^:private as-otype [tname]
  (case tname
    "boolean" "java.lang.Boolean"
    "byte"    "java.lang.Byte"
    "short"   "java.lang.Short"
    "int"     "java.lang.Integer"
    "long"    "java.lang.Long"
    "float"   "java.lang.Float"
    "double"  "java.lang.Double"
    "char"    "java.lang.Character"
    ;; else
    tname
    ))

(defn ^:private as-otype+ [type]
  "(-> Type Type)"
  (|case type
    (&/$DataT name params)
    (&type/Data$ (as-otype name) params)

    _
    type))

;; [Resources]
(do-template [<name> <output-tag> <input-class> <output-class>]
  (let [input-type (&type/Data$ <input-class> &/Nil$)
        output-type (&type/Data$ <output-class> &/Nil$)]
    (defn <name> [analyse exo-type x y]
      (|do [=x (&&/analyse-1 analyse input-type x)
            =y (&&/analyse-1 analyse input-type y)
            _ (&type/check exo-type output-type)
            _cursor &/cursor]
        (return (&/|list (&&/|meta output-type _cursor
                                   (&/V <output-tag> (&/T =x =y))))))))

  analyse-jvm-iadd &&/$jvm-iadd "java.lang.Integer" "java.lang.Integer"
  analyse-jvm-isub &&/$jvm-isub "java.lang.Integer" "java.lang.Integer"
  analyse-jvm-imul &&/$jvm-imul "java.lang.Integer" "java.lang.Integer"
  analyse-jvm-idiv &&/$jvm-idiv "java.lang.Integer" "java.lang.Integer"
  analyse-jvm-irem &&/$jvm-irem "java.lang.Integer" "java.lang.Integer"
  analyse-jvm-ieq  &&/$jvm-ieq  "java.lang.Integer" "java.lang.Boolean"
  analyse-jvm-ilt  &&/$jvm-ilt  "java.lang.Integer" "java.lang.Boolean"
  analyse-jvm-igt  &&/$jvm-igt  "java.lang.Integer" "java.lang.Boolean"

  analyse-jvm-ceq  &&/$jvm-ceq  "java.lang.Character" "java.lang.Boolean"
  analyse-jvm-clt  &&/$jvm-clt  "java.lang.Character" "java.lang.Boolean"
  analyse-jvm-cgt  &&/$jvm-cgt  "java.lang.Character" "java.lang.Boolean"

  analyse-jvm-ladd &&/$jvm-ladd "java.lang.Long"    "java.lang.Long"
  analyse-jvm-lsub &&/$jvm-lsub "java.lang.Long"    "java.lang.Long"
  analyse-jvm-lmul &&/$jvm-lmul "java.lang.Long"    "java.lang.Long"
  analyse-jvm-ldiv &&/$jvm-ldiv "java.lang.Long"    "java.lang.Long"
  analyse-jvm-lrem &&/$jvm-lrem "java.lang.Long"    "java.lang.Long"
  analyse-jvm-leq  &&/$jvm-leq  "java.lang.Long"    "java.lang.Boolean"
  analyse-jvm-llt  &&/$jvm-llt  "java.lang.Long"    "java.lang.Boolean"
  analyse-jvm-lgt  &&/$jvm-lgt  "java.lang.Long"    "java.lang.Boolean"

  analyse-jvm-fadd &&/$jvm-fadd "java.lang.Float"   "java.lang.Float"
  analyse-jvm-fsub &&/$jvm-fsub "java.lang.Float"   "java.lang.Float"
  analyse-jvm-fmul &&/$jvm-fmul "java.lang.Float"   "java.lang.Float"
  analyse-jvm-fdiv &&/$jvm-fdiv "java.lang.Float"   "java.lang.Float"
  analyse-jvm-frem &&/$jvm-frem "java.lang.Float"   "java.lang.Float"
  analyse-jvm-feq  &&/$jvm-feq  "java.lang.Float"   "java.lang.Boolean"
  analyse-jvm-flt  &&/$jvm-flt  "java.lang.Float"   "java.lang.Boolean"
  analyse-jvm-fgt  &&/$jvm-fgt  "java.lang.Float"   "java.lang.Boolean"

  analyse-jvm-dadd &&/$jvm-dadd "java.lang.Double"  "java.lang.Double"
  analyse-jvm-dsub &&/$jvm-dsub "java.lang.Double"  "java.lang.Double"
  analyse-jvm-dmul &&/$jvm-dmul "java.lang.Double"  "java.lang.Double"
  analyse-jvm-ddiv &&/$jvm-ddiv "java.lang.Double"  "java.lang.Double"
  analyse-jvm-drem &&/$jvm-drem "java.lang.Double"  "java.lang.Double"
  analyse-jvm-deq  &&/$jvm-deq  "java.lang.Double"  "java.lang.Boolean"
  analyse-jvm-dlt  &&/$jvm-dlt  "java.lang.Double"  "java.lang.Boolean"
  analyse-jvm-dgt  &&/$jvm-dgt  "java.lang.Double"  "java.lang.Boolean"
  )

(defn analyse-jvm-getstatic [analyse exo-type class field]
  (|do [class-loader &/loader
        =type (&host/lookup-static-field class-loader class field)
        :let [output-type =type]
        _ (&type/check exo-type output-type)
        _cursor &/cursor]
    (return (&/|list (&&/|meta output-type _cursor
                               (&/V &&/$jvm-getstatic (&/T class field output-type)))))))

(defn analyse-jvm-getfield [analyse exo-type class field object]
  (|do [class-loader &/loader
        =type (&host/lookup-static-field class-loader class field)
        =object (&&/analyse-1 analyse object)
        :let [output-type =type]
        _ (&type/check exo-type output-type)
        _cursor &/cursor]
    (return (&/|list (&&/|meta output-type _cursor
                               (&/V &&/$jvm-getfield (&/T class field =object output-type)))))))

(defn analyse-jvm-putstatic [analyse exo-type class field value]
  (|do [class-loader &/loader
        =type (&host/lookup-static-field class-loader class field)
        =value (&&/analyse-1 analyse =type value)
        :let [output-type &type/Unit]
        _ (&type/check exo-type output-type)
        _cursor &/cursor]
    (return (&/|list (&&/|meta output-type _cursor
                               (&/V &&/$jvm-putstatic (&/T class field =value output-type)))))))

(defn analyse-jvm-putfield [analyse exo-type class field value object]
  (|do [class-loader &/loader
        =type (&host/lookup-static-field class-loader class field)
        =object (&&/analyse-1 analyse object)
        =value (&&/analyse-1 analyse =type value)
        :let [output-type &type/Unit]
        _ (&type/check exo-type output-type)
        _cursor &/cursor]
    (return (&/|list (&&/|meta output-type _cursor
                               (&/V &&/$jvm-putfield (&/T class field =value =object (&&/expr-type* =object))))))))

(defn analyse-jvm-invokestatic [analyse exo-type class method classes args]
  (|do [class-loader &/loader
        =return+exceptions (&host/lookup-static-method class-loader class method classes)
        :let [[=return exceptions] =return+exceptions]
        ;; :let [_ (prn 'analyse-jvm-invokestatic (&/adt->text =return+exceptions))]
        _ (ensure-catching exceptions)
        ;; :let [_ (matchv ::M/objects [=return]
        ;;           [[&/$DataT _return-class &/Nil$]]
        ;;           (prn 'analyse-jvm-invokestatic class method _return-class))]
        =args (&/map2% (fn [_class _arg]
                         (&&/analyse-1 analyse (&type/Data$ _class &/Nil$) _arg))
                       classes
                       args)
        :let [output-type =return]
        _ (&type/check exo-type (as-otype+ output-type))
        _cursor &/cursor]
    (return (&/|list (&&/|meta output-type _cursor
                               (&/V &&/$jvm-invokestatic (&/T class method classes =args output-type)))))))

(defn analyse-jvm-instanceof [analyse exo-type class object]
  (|do [=object (&&/analyse-1+ analyse object)
        _ (ensure-object =object)
        :let [output-type &type/Bool]
        _ (&type/check exo-type output-type)
        _cursor &/cursor]
    (return (&/|list (&&/|meta output-type _cursor
                               (&/V &&/$jvm-instanceof (&/T class =object)))))))

(do-template [<name> <tag>]
  (defn <name> [analyse exo-type class method classes object args]
    (|do [class-loader &/loader
          =return+exceptions (&host/lookup-virtual-method class-loader class method classes)
          ;; :let [_ (prn '<name> [class method] (&/adt->text =return+exceptions))]
          :let [[=return exceptions] =return+exceptions]
          _ (ensure-catching exceptions)
          =object (&&/analyse-1 analyse (&type/Data$ class &/Nil$) object)
          =args (&/map2% (fn [c o] (&&/analyse-1 analyse (&type/Data$ c &/Nil$) o))
                         classes args)
          :let [output-type =return]
          ;; :let [_ (prn '<name> [class method] '=return (&type/show-type =return))]
          ;; :let [_ (prn '<name> '(as-otype+ output-type) (&type/show-type (as-otype+ output-type)))]
          _ (&type/check exo-type (as-otype+ output-type))
          _cursor &/cursor]
      (return (&/|list (&&/|meta output-type _cursor
                                 (&/V <tag> (&/T class method classes =object =args output-type)))))))

  analyse-jvm-invokevirtual   &&/$jvm-invokevirtual
  analyse-jvm-invokeinterface &&/$jvm-invokeinterface
  )

(defn analyse-jvm-invokespecial [analyse exo-type class method classes object args]
  (|do [class-loader &/loader
        =return+exceptions (if (= "<init>" method)
                             (return (&/T &type/Unit &/Nil$))
                             (&host/lookup-virtual-method class-loader class method classes))
        :let [[=return exceptions] =return+exceptions]
        ;; :let [_ (prn 'analyse-jvm-invokespecial (&/adt->text =return+exceptions))]
        _ (ensure-catching exceptions)
        =object (&&/analyse-1 analyse (&type/Data$ class &/Nil$) object)
        =args (&/map2% (fn [c o]
                         (&&/analyse-1 analyse (&type/Data$ c &/Nil$) o))
                       classes args)
        :let [output-type =return]
        _ (&type/check exo-type (as-otype+ output-type))
        _cursor &/cursor]
    (return (&/|list (&&/|meta output-type _cursor
                               (&/V &&/$jvm-invokespecial (&/T class method classes =object =args output-type)))))))

(defn analyse-jvm-null? [analyse exo-type object]
  (|do [=object (&&/analyse-1+ analyse object)
        _ (ensure-object =object)
        :let [output-type &type/Bool]
        _ (&type/check exo-type output-type)
        _cursor &/cursor]
    (return (&/|list (&&/|meta output-type _cursor
                               (&/V &&/$jvm-null? =object))))))

(defn analyse-jvm-null [analyse exo-type]
  (|do [:let [output-type (&type/Data$ &host/null-data-tag &/Nil$)]
        _ (&type/check exo-type output-type)
        _cursor &/cursor]
    (return (&/|list (&&/|meta output-type _cursor
                               (&/V &&/$jvm-null nil))))))

(defn analyse-jvm-new [analyse exo-type class classes args]
  (|do [class-loader &/loader
        [=return exceptions] (&host/lookup-constructor class-loader class classes)
        =args (&/map2% (fn [c o] (&&/analyse-1 analyse (&type/Data$ c &/Nil$) o))
                       classes args)
        _ (ensure-catching exceptions)
        :let [output-type (&type/Data$ class &/Nil$)]
        _ (&type/check exo-type output-type)
        _cursor &/cursor]
    (return (&/|list (&&/|meta output-type _cursor
                               (&/V &&/$jvm-new (&/T class classes =args)))))))

(do-template [<class> <new-name> <new-tag> <load-name> <load-tag> <store-name> <store-tag>]
  (let [elem-type (&type/Data$ <class> &/Nil$)
        array-type (&type/Data$ &host/array-data-tag (&/|list elem-type))
        length-type &type/Int
        idx-type &type/Int]
    (defn <new-name> [analyse length]
      (|do [=length (&&/analyse-1 analyse length-type length)
            _cursor &/cursor]
        (return (&/|list (&&/|meta array-type _cursor
                                   (&/V <new-tag> =length))))))

    (defn <load-name> [analyse array idx]
      (|do [=array (&&/analyse-1 analyse array-type array)
            =idx (&&/analyse-1 analyse idx-type idx)
            _cursor &/cursor]
        (return (&/|list (&&/|meta elem-type _cursor
                                   (&/V <load-tag> (&/T =array =idx)))))))

    (defn <store-name> [analyse array idx elem]
      (|do [=array (&&/analyse-1 analyse array-type array)
            =idx (&&/analyse-1 analyse idx-type idx)
            =elem (&&/analyse-1 analyse elem-type elem)
            _cursor &/cursor]
        (return (&/|list (&&/|meta array-type _cursor
                                   (&/V <store-tag> (&/T =array =idx =elem)))))))
    )

  "java.lang.Boolean"   analyse-jvm-znewarray &&/$jvm-znewarray analyse-jvm-zaload &&/$jvm-zaload analyse-jvm-zastore &&/$jvm-zastore
  "java.lang.Byte"      analyse-jvm-bnewarray &&/$jvm-bnewarray analyse-jvm-baload &&/$jvm-baload analyse-jvm-bastore &&/$jvm-bastore
  "java.lang.Short"     analyse-jvm-snewarray &&/$jvm-snewarray analyse-jvm-saload &&/$jvm-saload analyse-jvm-sastore &&/$jvm-sastore
  "java.lang.Integer"   analyse-jvm-inewarray &&/$jvm-inewarray analyse-jvm-iaload &&/$jvm-iaload analyse-jvm-iastore &&/$jvm-iastore
  "java.lang.Long"      analyse-jvm-lnewarray &&/$jvm-lnewarray analyse-jvm-laload &&/$jvm-laload analyse-jvm-lastore &&/$jvm-lastore
  "java.lang.Float"     analyse-jvm-fnewarray &&/$jvm-fnewarray analyse-jvm-faload &&/$jvm-faload analyse-jvm-fastore &&/$jvm-fastore
  "java.lang.Double"    analyse-jvm-dnewarray &&/$jvm-dnewarray analyse-jvm-daload &&/$jvm-daload analyse-jvm-dastore &&/$jvm-dastore
  "java.lang.Character" analyse-jvm-cnewarray &&/$jvm-cnewarray analyse-jvm-caload &&/$jvm-caload analyse-jvm-castore &&/$jvm-castore
  )

(let [length-type &type/Int
      idx-type &type/Int]
  (defn analyse-jvm-anewarray [analyse class length]
    (let [elem-type (&type/Data$ class &/Nil$)
          array-type (&type/Data$ &host/array-data-tag (&/|list elem-type))]
      (|do [=length (&&/analyse-1 analyse length-type length)
            _cursor &/cursor]
        (return (&/|list (&&/|meta array-type _cursor
                                   (&/V &&/$jvm-anewarray (&/T class =length))))))))

  (defn analyse-jvm-aaload [analyse class array idx]
    (let [elem-type (&type/Data$ class &/Nil$)
          array-type (&type/Data$ &host/array-data-tag (&/|list elem-type))]
      (|do [=array (&&/analyse-1 analyse array-type array)
            =idx (&&/analyse-1 analyse idx-type idx)
            _cursor &/cursor]
        (return (&/|list (&&/|meta elem-type _cursor
                                   (&/V &&/$jvm-aaload (&/T class =array =idx))))))))

  (defn analyse-jvm-aastore [analyse class array idx elem]
    (let [elem-type (&type/Data$ class &/Nil$)
          array-type (&type/Data$ &host/array-data-tag (&/|list elem-type))]
      (|do [=array (&&/analyse-1 analyse array-type array)
            =idx (&&/analyse-1 analyse idx-type idx)
            =elem (&&/analyse-1 analyse elem-type elem)
            _cursor &/cursor]
        (return (&/|list (&&/|meta array-type _cursor
                                   (&/V &&/$jvm-aastore (&/T class =array =idx =elem)))))))))

(let [length-type (&type/Data$ "java.lang.Long" &/Nil$)]
  (defn analyse-jvm-arraylength [analyse array]
    (&type/with-var
      (fn [$var]
        (let [elem-type $var
              array-type (&type/Data$ &host/array-data-tag (&/|list elem-type))]
          (|do [=array (&&/analyse-1 analyse array-type array)
                _cursor &/cursor]
            (return (&/|list (&&/|meta length-type _cursor
                                       (&/V &&/$jvm-arraylength =array)
                                       )))))))))

(defn ^:private analyse-modifiers [modifiers]
  (&/fold% (fn [so-far modif]
             (|case modif
               [_ (&/$TextS "public")]
               (return (assoc so-far :visibility "public"))

               [_ (&/$TextS "private")]
               (return (assoc so-far :visibility "private"))

               [_ (&/$TextS "protected")]
               (return (assoc so-far :visibility "protected"))

               [_ (&/$TextS "static")]
               (return (assoc so-far :static? true))

               [_ (&/$TextS "final")]
               (return (assoc so-far :final? true))

               [_ (&/$TextS "abstract")]
               (return (assoc so-far :abstract? true))

               [_ (&/$TextS "synchronized")]
               (return (assoc so-far :concurrency "synchronized"))

               [_ (&/$TextS "volatile")]
               (return (assoc so-far :concurrency "volatile"))

               _
               (fail (str "[Analyser Error] Unknown modifier: " (&/show-ast modif)))))
           {:visibility "default"
            :static? false
            :final? false
            :abstract? false
            :concurrency nil}
           modifiers))

(defn ^:private analyse-field [field]
  (|case field
    [_ (&/$FormS (&/$Cons [_ (&/$TextS ?field-name)]
                          (&/$Cons [_ (&/$TupleS ?field-modifiers)]
                                   (&/$Cons [_ (&/$TextS ?field-type)]
                                            (&/$Nil)))))]
    (|do [=field-modifiers (analyse-modifiers ?field-modifiers)]
      (return {:name ?field-name
               :modifiers =field-modifiers
               :type ?field-type}))
    
    _
    (fail "[Analyser Error] Wrong syntax for field.")))

(defn ^:private analyse-method [analyse owner-class method]
  (|case method
    [idx [_ (&/$FormS (&/$Cons [_ (&/$TextS method-name)]
                               (&/$Cons [_ (&/$TupleS method-modifiers)]
                                        (&/$Cons [_ (&/$TupleS method-exs)]
                                                 (&/$Cons [_ (&/$TupleS method-inputs)]
                                                          (&/$Cons [_ (&/$TextS method-output)]
                                                                   (&/$Cons method-body
                                                                            (&/$Nil))))))))]]
    (|do [=method-modifiers (analyse-modifiers method-modifiers)
          =method-exs (&/map% extract-text method-exs)
          =method-inputs (&/map% (fn [minput]
                                   (|case minput
                                     [_ (&/$FormS (&/$Cons [_ (&/$SymbolS "" input-name)]
                                                           (&/$Cons [_ (&/$TextS input-type)]
                                                                    (&/$Nil))))]
                                     (return (&/T input-name input-type))

                                     _
                                     (fail "[Analyser Error] Wrong syntax for method input.")))
                                 method-inputs)
          =method-body (&/fold (fn [body* input*]
                                 (|let [[iname itype] input*]
                                   (&&env/with-local iname (&type/Data$ (as-otype itype) &/Nil$)
                                     body*)))
                               (if (= "void" method-output)
                                 (&&/analyse-1+ analyse method-body)
                                 (&&/analyse-1 analyse (&type/Data$ (as-otype method-output) &/Nil$) method-body))
                               (&/|reverse (&/Cons$ (&/T &&/jvm-this owner-class)
                                                    =method-inputs)))]
      (return {:name method-name
               :modifiers =method-modifiers
               :exceptions =method-exs
               :inputs (&/|map &/|second =method-inputs)
               :output method-output
               :body =method-body}))
    
    _
    (fail "[Analyser Error] Wrong syntax for method.")))

(defn ^:private analyse-method-decl [method]
  (|case method
    [_ (&/$FormS (&/$Cons [_ (&/$TextS method-name)]
                          (&/$Cons [_ (&/$TupleS modifiers)]
                                   (&/$Cons [_ (&/$TupleS method-exs)]
                                            (&/$Cons [_ (&/$TupleS inputs)]
                                                     (&/$Cons [_ (&/$TextS output)]
                                                              (&/$Nil)))))))]
    (|do [=inputs (&/map% extract-text inputs)
          =modifiers (analyse-modifiers modifiers)
          =method-exs (&/map% extract-text method-exs)]
      (return {:name method-name
               :modifiers =modifiers
               :exceptions =method-exs
               :inputs =inputs
               :output output}))
    
    _
    (fail (str "[Analyser Error] Invalid method signature: " (&/show-ast method)))))

(defn ^:private mandatory-methods [supers]
  (|do [class-loader &/loader]
    (&/flat-map% (partial &host/abstract-methods class-loader) supers)))

(defn ^:private check-method-completion [supers methods]
  "(-> (List ClassName) (List MethodDesc) (Lux (,)))"
  (|do [abstract-methods (mandatory-methods supers)
        :let [methods-map (&/fold (fn [mmap mentry]
                                    (assoc mmap (:name mentry) mentry))
                                  {}
                                  methods)
              missing-method (&/fold (fn [missing abs-meth]
                                       (|let [[am-name am-inputs] abs-meth]
                                         (or missing
                                             (if-let [meth-struct (get methods-map am-name)]
                                               (let [meth-inputs (:inputs meth-struct)]
                                                 (if (and (= (&/|length meth-inputs) (&/|length am-inputs))
                                                          (&/fold2 (fn [prev mi ai] (and prev (= mi ai)))
                                                                   true
                                                                   meth-inputs am-inputs))
                                                   nil
                                                   am-name))
                                               am-name))))
                                     nil
                                     abstract-methods)]]
    (if (nil? missing-method)
      (return nil)
      (fail (str "[Analyser Error] Missing method: " missing-method)))))

(defn analyse-jvm-class [analyse compile-token name super-class interfaces fields methods]
  (&/with-closure
    (|do [module &/get-module-name
          :let [full-name (str module "." name)]
          ;; :let [_ (prn 'analyse-jvm-class/_0)]
          =fields (&/map% analyse-field fields)
          ;; :let [_ (prn 'analyse-jvm-class/_1)]
          _ (&host/use-dummy-class name super-class interfaces =fields)
          =methods (&/map% (partial analyse-method analyse full-name) (&/enumerate methods))
          ;; :let [_ (prn 'analyse-jvm-class/_2)]
          _ (check-method-completion (&/Cons$ super-class interfaces) =methods)
          ;; :let [_ (prn 'analyse-jvm-class/_3)]
          _ (compile-token (&/V &&/$jvm-class (&/T name super-class interfaces =fields =methods nil)))
          :let [_ (println 'DEF (str module "." name))]]
      (return &/Nil$))))

(defn analyse-jvm-interface [analyse compile-token name supers methods]
  (|do [module &/get-module-name
        =methods (&/map% analyse-method-decl methods)
        _ (compile-token (&/V &&/$jvm-interface (&/T name supers =methods)))
        :let [_ (println 'DEF (str module "." name))]]
    (return &/Nil$)))

(defn ^:private captured-source [env-entry]
  (|case env-entry
    [name [_ (&&/$captured _ _ source)]]
    source))

(let [captured-slot-modifier {:visibility "private"
                              :static? false
                              :final? false
                              :abstract? false
                              :concurrency nil}
      captured-slot-type "java.lang.Object"]
  (defn analyse-jvm-anon-class [analyse compile-token exo-type super-class interfaces methods]
    (&/with-closure
      (|do [;; :let [_ (prn 'analyse-jvm-anon-class/_0 super-class)]
            module &/get-module-name
            scope &/get-scope-name
            ;; :let [_ (prn 'analyse-jvm-anon-class/_1 super-class)]
            :let [name (&host/location (&/|tail scope))
                  anon-class (str module "." name)]
            ;; :let [_ (prn 'analyse-jvm-anon-class/_2 name anon-class)]
            _ (&host/use-dummy-class name super-class interfaces (&/|list))
            =methods (&/map% (partial analyse-method analyse anon-class) (&/enumerate methods))
            ;; :let [_ (prn 'analyse-jvm-anon-class/_3 name anon-class)]
            _ (check-method-completion (&/Cons$ super-class interfaces) =methods)
            ;; :let [_ (prn 'analyse-jvm-anon-class/_4 name anon-class)]
            =captured &&env/captured-vars
            :let [=fields (&/|map (fn [idx+capt]
                                    {:name (str &c!base/closure-prefix (aget idx+capt 0))
                                     :modifiers captured-slot-modifier
                                     :type captured-slot-type})
                                  (&/enumerate =captured))
                  ;; _ (prn '=methods (&/adt->text (&/|map :body =methods)))
                  ;; =methods* (rename-captured-vars)
                  ]
            :let [sources (&/|map captured-source =captured)]
            ;; :let [_ (prn 'analyse-jvm-anon-class/_5 name anon-class)]
            ;; _ (compile-token (&/T (&/V &&/$jvm-anon-class (&/T name super-class interfaces =captured =methods)) exo-type))
            _ (compile-token (&/V &&/$jvm-class (&/T name super-class interfaces =fields =methods =captured)))
            :let [_ (println 'DEF anon-class)]
            _cursor &/cursor]
        (return (&/|list (&&/|meta (&type/Data$ anon-class (&/|list)) _cursor
                                   (&/V &&/$jvm-new (&/T anon-class (&/|repeat (&/|length sources) captured-slot-type) sources))
                                   )))
        ;; (analyse-jvm-new analyse exo-type anon-class (&/|repeat (&/|length sources) captured-slot-type) sources)
        ))))

(defn analyse-jvm-try [analyse exo-type ?body ?catches+?finally]
  (|do [:let [[?catches ?finally] ?catches+?finally]
        =catches (&/map% (fn [[?ex-class ?ex-arg ?catch-body]]
                           (|do [=catch-body (&&env/with-local ?ex-arg (&type/Data$ ?ex-class &/Nil$)
                                               (&&/analyse-1 analyse exo-type ?catch-body))
                                 idx &&env/next-local-idx]
                             (return (&/T ?ex-class idx =catch-body))))
                         ?catches)
        :let [catched-exceptions (&/|map #(aget % 0) =catches)]
        =body (with-catches catched-exceptions
                (&&/analyse-1 analyse exo-type ?body))
        =finally (|case ?finally
                   (&/$None)           (return &/None$)
                   (&/$Some ?finally*) (|do [=finally (&&/analyse-1+ analyse ?finally*)]
                                         (return (&/V &/$Some =finally))))
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&/V &&/$jvm-try (&/T =body =catches =finally)))))))

(defn analyse-jvm-throw [analyse exo-type ?ex]
  (|do [=ex (&&/analyse-1 analyse (&type/Data$ "java.lang.Throwable" &/Nil$) ?ex)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor (&/V &&/$jvm-throw =ex))))))

(do-template [<name> <tag>]
  (defn <name> [analyse exo-type ?monitor]
    (|do [=monitor (&&/analyse-1+ analyse ?monitor)
          _ (ensure-object =monitor)
          :let [output-type &type/Unit]
          _ (&type/check exo-type output-type)
          _cursor &/cursor]
      (return (&/|list (&&/|meta output-type _cursor (&/V <tag> =monitor))))))

  analyse-jvm-monitorenter &&/$jvm-monitorenter
  analyse-jvm-monitorexit  &&/$jvm-monitorexit
  )

(do-template [<name> <tag> <from-class> <to-class>]
  (let [output-type (&type/Data$ <to-class> &/Nil$)]
    (defn <name> [analyse exo-type ?value]
      (|do [=value (&&/analyse-1 analyse (&type/Data$ <from-class> &/Nil$) ?value)
            _ (&type/check exo-type output-type)
            _cursor &/cursor]
        (return (&/|list (&&/|meta output-type _cursor (&/V <tag> =value)))))))

  analyse-jvm-d2f &&/$jvm-d2f "java.lang.Double"  "java.lang.Float"
  analyse-jvm-d2i &&/$jvm-d2i "java.lang.Double"  "java.lang.Integer"
  analyse-jvm-d2l &&/$jvm-d2l "java.lang.Double"  "java.lang.Long"

  analyse-jvm-f2d &&/$jvm-f2d "java.lang.Float"   "java.lang.Double"
  analyse-jvm-f2i &&/$jvm-f2i "java.lang.Float"   "java.lang.Integer"
  analyse-jvm-f2l &&/$jvm-f2l "java.lang.Float"   "java.lang.Long"

  analyse-jvm-i2b &&/$jvm-i2b "java.lang.Integer" "java.lang.Byte"
  analyse-jvm-i2c &&/$jvm-i2c "java.lang.Integer" "java.lang.Character"
  analyse-jvm-i2d &&/$jvm-i2d "java.lang.Integer" "java.lang.Double"
  analyse-jvm-i2f &&/$jvm-i2f "java.lang.Integer" "java.lang.Float"
  analyse-jvm-i2l &&/$jvm-i2l "java.lang.Integer" "java.lang.Long"
  analyse-jvm-i2s &&/$jvm-i2s "java.lang.Integer" "java.lang.Short"

  analyse-jvm-l2d &&/$jvm-l2d "java.lang.Long"    "java.lang.Double"
  analyse-jvm-l2f &&/$jvm-l2f "java.lang.Long"    "java.lang.Float"
  analyse-jvm-l2i &&/$jvm-l2i "java.lang.Long"    "java.lang.Integer"
  )

(do-template [<name> <tag> <from-class> <to-class>]
  (let [output-type (&type/Data$ <to-class> &/Nil$)]
    (defn <name> [analyse exo-type ?value]
      (|do [=value (&&/analyse-1 analyse (&type/Data$ <from-class> &/Nil$) ?value)
            _ (&type/check exo-type output-type)
            _cursor &/cursor]
        (return (&/|list (&&/|meta output-type _cursor (&/V <tag> =value)))))))

  analyse-jvm-iand  &&/$jvm-iand  "java.lang.Integer" "java.lang.Integer"
  analyse-jvm-ior   &&/$jvm-ior   "java.lang.Integer" "java.lang.Integer"
  analyse-jvm-ixor  &&/$jvm-ixor  "java.lang.Integer" "java.lang.Integer"
  analyse-jvm-ishl  &&/$jvm-ishl  "java.lang.Integer" "java.lang.Integer"
  analyse-jvm-ishr  &&/$jvm-ishr  "java.lang.Integer" "java.lang.Integer"
  analyse-jvm-iushr &&/$jvm-iushr "java.lang.Integer" "java.lang.Integer"

  analyse-jvm-land  &&/$jvm-land  "java.lang.Long"    "java.lang.Long"
  analyse-jvm-lor   &&/$jvm-lor   "java.lang.Long"    "java.lang.Long"
  analyse-jvm-lxor  &&/$jvm-lxor  "java.lang.Long"    "java.lang.Long"
  analyse-jvm-lshl  &&/$jvm-lshl  "java.lang.Long"    "java.lang.Integer"
  analyse-jvm-lshr  &&/$jvm-lshr  "java.lang.Long"    "java.lang.Integer"
  analyse-jvm-lushr &&/$jvm-lushr "java.lang.Long"    "java.lang.Integer"
  )

(let [input-type (&type/App$ &type/List &type/Text)
      output-type (&type/App$ &type/IO &type/Unit)]
  (defn analyse-jvm-program [analyse compile-token ?args ?body]
    (|do [=body (&/with-scope ""
                  (&&env/with-local ?args input-type
                    (&&/analyse-1 analyse output-type ?body)))
          _ (compile-token (&/V &&/$jvm-program =body))]
      (return &/Nil$))))
