(ns lux.host
  (:require (clojure [string :as string]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return |let |case]]
                 [type :as &type])
            [lux.type.host :as &host-type]
            [lux.host.generics :as &host-generics])
  (:import (java.lang.reflect Field Method Constructor Modifier Type
                              GenericArrayType ParameterizedType TypeVariable)
           (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Constants]
(def module-separator "/")
(def class-name-separator ".")
(def class-separator "/")
(def bytecode-version Opcodes/V1_6)

(defn ^String external [^String internal]
  (.replace internal class-separator class-name-separator))

(defn ^String internal [^String external]
  (.replace external class-name-separator class-separator))

(defn ^String fundamental-class [^String name]
  (str (external &/prelude) class-name-separator name))

(def ^:const ^String function-class
  (fundamental-class "Function"))

(def ^:const ^String lux-utils-class
  (fundamental-class "Runtime"))

;; [Resources]
(defn ^String ->module-class [old]
  old)

(def ->package ->module-class)

(defn unfold-array
  "(-> Type [Nat Type])"
  [type]
  (|case type
    (&/$Primitive "#Array" (&/$Item (&/$Primitive "#Mutable" (&/$Item (&/$Function _ param)
                                                                      (&/$End)))
                                    (&/$End)))
    (|let [[count inner] (unfold-array param)]
      (&/T [(inc count) inner]))

    (&/$Primitive "#Array" (&/$Item param (&/$End)))
    (|let [[count inner] (unfold-array param)]
      (&/T [(inc count) inner]))
    
    _
    (&/T [0 type])))

(let [ex-type-class (str "L" (&host-generics/->bytecode-class-name "java.lang.Object") ";")
      object-array (str "[" "L" (&host-generics/->bytecode-class-name "java.lang.Object") ";")]
  (defn ->java-sig
    "(-> Type (Lux Text))"
    [^objects type]
    (|case type
      (&/$Primitive ?name params)
      (cond (= &host-type/array-data-tag ?name) (|do [normal_type (&type/normal type)
                                                      :let [[level base] (unfold-array normal_type)]
                                                      base-sig (|case base
                                                                 (&/$Primitive base-class _)
                                                                 (return (&host-generics/->type-signature base-class))

                                                                 _
                                                                 (->java-sig base))]
                                                  (return (str (->> (&/|repeat level "[") (&/fold str ""))
                                                               base-sig)))
            (= &host-type/null-data-tag ?name)  (return (&host-generics/->type-signature "java.lang.Object"))
            :else                               (return (&host-generics/->type-signature ?name)))

      (&/$Function _ _)
      (return (&host-generics/->type-signature function-class))

      (&/$Sum _)
      (return object-array)

      (&/$Product _)
      (return object-array)

      (&/$Named ?name ?type)
      (->java-sig ?type)

      (&/$Apply ?A ?F)
      (|do [type* (&type/apply-type ?F ?A)]
        (->java-sig type*))

      (&/$Ex _)
      (return ex-type-class)

      _
      (if (&type/type= &type/Any type)
        (return "V")
        (assert false (str '->java-sig " " (&type/show-type type))))
      )))

(do-template [<name> <static?>]
  (defn <name> [class-loader target field]
    (|let [target-class (Class/forName target true class-loader)]
      (if-let [^Type gtype (first (for [^Field =field (seq (.getDeclaredFields target-class))
                                        :when (and (.equals ^Object field (.getName =field))
                                                   (.equals ^Object <static?> (Modifier/isStatic (.getModifiers =field))))]
                                    (.getGenericType =field)))]
        (|let [gvars (->> target-class .getTypeParameters seq &/->list)]
          (return (&/T [gvars gtype])))
        (&/fail-with-loc (str "[Host Error] Field does not exist: " target "." field)))))

  lookup-static-field true
  lookup-field        false
  )

(do-template [<name> <static?> <method-type> <options>]
  (defn <name> [class-loader target method-name args]
    (|let [target-class (Class/forName target true class-loader)]
      (if-let [[^Method method ^Class declarer] (first (for [^Method =method (<options> target-class)
                                                             :when (and (.equals ^Object method-name (.getName =method))
                                                                        (.equals ^Object <static?> (Modifier/isStatic (.getModifiers =method)))
                                                                        (let [param-types (&/->list (seq (.getParameterTypes =method)))]
                                                                          (and (= (&/|length args) (&/|length param-types))
                                                                               (&/fold2 #(and %1 (.equals ^Object %2 %3))
                                                                                        true
                                                                                        args
                                                                                        (&/|map #(.getName ^Class %) param-types)))))]
                                                         [=method
                                                          (.getDeclaringClass =method)]))]
        (|let [parent-gvars (->> target-class .getTypeParameters seq &/->list)
               gvars (->> method .getTypeParameters seq &/->list)
               gargs (->> method .getGenericParameterTypes seq &/->list)
               _ (when (.getAnnotation method java.lang.Deprecated)
                   (println (str "[Host Warning] Deprecated method: " target "." method-name " " (->> args &/->seq print-str))))]
          (return (&/T [(.getGenericReturnType method)
                        (->> method .getExceptionTypes &/->list (&/|map #(.getName ^Class %)))
                        parent-gvars
                        gvars
                        gargs])))
        (&/fail-with-loc (str "[Host Error] " <method-type> " method does not exist: " target "." method-name " " "(" (->> args (&/|interpose ", ") (&/fold str "")) ")")))))

  lookup-static-method  true  "Static" .getDeclaredMethods
  lookup-virtual-method false "Virtual" .getMethods
  )

(defn lookup-constructor [class-loader target args]
  (let [target-class (Class/forName target true class-loader)]
    (if-let [^Constructor ctor (first (for [^Constructor =method (.getDeclaredConstructors target-class)
                                            :when (let [param-types (&/->list (seq (.getParameterTypes =method)))]
                                                    (and (= (&/|length args) (&/|length param-types))
                                                         (&/fold2 #(and %1 (.equals ^Object %2 %3))
                                                                  true
                                                                  args
                                                                  (&/|map #(.getName ^Class %) param-types))))]
                                        =method))]
      (|let [gvars (->> target-class .getTypeParameters seq &/->list)
             gargs (->> ctor .getGenericParameterTypes seq &/->list)
             exs (->> ctor .getExceptionTypes &/->list (&/|map #(.getName ^Class %)))
             _ (when (.getAnnotation ctor java.lang.Deprecated)
                 (println (str "[Host Warning] Deprecated constructor: " target " " (->> args &/->seq print-str))))]
        (return (&/T [exs gvars gargs])))
      (&/fail-with-loc (str "[Host Error] Constructor does not exist: " target " " (->> args &/->seq print-str))))))

(defn abstract-methods
  "(-> ClassLoader SuperClassDecl (Lux (List (, Text (List Text)))))"
  [class-loader super-class]
  (|let [[super-name super-params] super-class]
    (return (&/->list (for [^Method =method (.getDeclaredMethods (Class/forName super-name true class-loader))
                            :when (Modifier/isAbstract (.getModifiers =method))]
                        (&/T [(.getName =method) (&/|map #(.getName ^Class %) (&/->list (seq (.getParameterTypes =method))))]))))))

(defn def-name [name]
  (str (&/normalize-name name) "_" (Long/toUnsignedString (hash name))))

(defn location [scope]
  (let [scope (&/$Item (def-name (&/|head scope))
                       (&/|map &/normalize-name (&/|tail scope)))]
    (->> scope
         (&/|interpose "$")
         (&/fold str ""))))

(defn primitive-jvm-type? [type]
  (case type
    ("boolean" "byte" "short" "int" "long" "float" "double" "char")
    true
    ;; else
    false))

(defn dummy-value [^MethodVisitor writer class]
  (|case class
    (&/$GenericClass "boolean" (&/$End))
    (doto writer
      (.visitLdcInsn false))
    
    (&/$GenericClass "byte" (&/$End))
    (doto writer
      (.visitLdcInsn (byte 0)))
    
    (&/$GenericClass "short" (&/$End))
    (doto writer
      (.visitLdcInsn (short 0)))
    
    (&/$GenericClass "int" (&/$End))
    (doto writer
      (.visitLdcInsn (int 0)))
    
    (&/$GenericClass "long" (&/$End))
    (doto writer
      (.visitLdcInsn (long 0)))
    
    (&/$GenericClass "float" (&/$End))
    (doto writer
      (.visitLdcInsn (float 0.0)))
    
    (&/$GenericClass "double" (&/$End))
    (doto writer
      (.visitLdcInsn (double 0.0)))
    
    (&/$GenericClass "char" (&/$End))
    (doto writer
      (.visitLdcInsn (char 0)))

    _
    (doto writer
      (.visitInsn Opcodes/ACONST_NULL))))

(defn ^:private dummy-return [^MethodVisitor writer output]
  (|case output
    (&/$GenericClass "void" (&/$End))
    (.visitInsn writer Opcodes/RETURN)

    (&/$GenericClass "boolean" (&/$End))
    (doto writer
      (dummy-value output)
      (.visitInsn Opcodes/IRETURN))
    
    (&/$GenericClass "byte" (&/$End))
    (doto writer
      (dummy-value output)
      (.visitInsn Opcodes/IRETURN))
    
    (&/$GenericClass "short" (&/$End))
    (doto writer
      (dummy-value output)
      (.visitInsn Opcodes/IRETURN))
    
    (&/$GenericClass "int" (&/$End))
    (doto writer
      (dummy-value output)
      (.visitInsn Opcodes/IRETURN))
    
    (&/$GenericClass "long" (&/$End))
    (doto writer
      (dummy-value output)
      (.visitInsn Opcodes/LRETURN))
    
    (&/$GenericClass "float" (&/$End))
    (doto writer
      (dummy-value output)
      (.visitInsn Opcodes/FRETURN))
    
    (&/$GenericClass "double" (&/$End))
    (doto writer
      (dummy-value output)
      (.visitInsn Opcodes/DRETURN))
    
    (&/$GenericClass "char" (&/$End))
    (doto writer
      (dummy-value output)
      (.visitInsn Opcodes/IRETURN))

    _
    (doto writer
      (dummy-value output)
      (.visitInsn Opcodes/ARETURN))))

(defn ^:private ->dummy-type [real-name store-name gclass]
  (|case gclass
    (&/$GenericClass _name _params)
    (if (= real-name _name)
      (&/$GenericClass store-name (&/|map (partial ->dummy-type real-name store-name) _params))
      gclass)

    _
    gclass))

(def init-method-name "<init>")

(defn ^:private dummy-ctor [^MethodVisitor writer real-name store-name super-class ctor-args]
  (|let [ctor-arg-types (->> ctor-args
                             (&/|map (comp &host-generics/gclass->signature (comp (partial ->dummy-type real-name store-name) &/|first)))
                             (&/fold str ""))]
    (doto writer
      (.visitVarInsn Opcodes/ALOAD 0)
      (-> (doto (dummy-value arg-type)
            (-> (.visitTypeInsn Opcodes/CHECKCAST arg-type)
                (->> (when (not (primitive-jvm-type? arg-type))))))
          (->> (doseq [ctor-arg (&/->seq ctor-args)
                       :let [arg-type (->> ctor-arg
                                           &/|first
                                           (->dummy-type real-name store-name)
                                           &host-generics/gclass->class-name)]])))
      (.visitMethodInsn Opcodes/INVOKESPECIAL (&host-generics/->bytecode-class-name (&host-generics/super-class-name super-class)) init-method-name (str "(" ctor-arg-types ")V"))
      (.visitInsn Opcodes/RETURN))))

(defn ^:private compile-dummy-method [^ClassWriter =class real-name store-name super-class method-def]
  (|case method-def
    (&/$ConstructorMethodSyntax =privacy-modifier ?strict =anns =gvars =exceptions =inputs =ctor-args body)
    (|let [=output (&/$GenericClass "void" (&/|list))
           method-decl [init-method-name
                        =anns
                        =gvars
                        (&/|map (partial ->dummy-type real-name store-name) =exceptions)
                        (&/|map (comp (partial ->dummy-type real-name store-name) &/|second) =inputs)
                        (->dummy-type real-name store-name =output)]
           [simple-signature generic-signature] (&host-generics/method-signatures method-decl)]
      (doto (.visitMethod =class Opcodes/ACC_PUBLIC
                          init-method-name
                          simple-signature
                          generic-signature
                          (->> =exceptions (&/|map &host-generics/gclass->bytecode-class-name) &/->seq (into-array java.lang.String)))
        .visitCode
        (dummy-ctor real-name store-name super-class =ctor-args)
        (.visitMaxs 0 0)
        (.visitEnd)))

    (&/$VirtualMethodSyntax =name =privacy-modifier =final? ?strict =anns =gvars =exceptions =inputs =output body)
    (|let [method-decl [=name
                        =anns
                        =gvars
                        (&/|map (partial ->dummy-type real-name store-name) =exceptions)
                        (&/|map (comp (partial ->dummy-type real-name store-name) &/|second) =inputs)
                        (->dummy-type real-name store-name =output)]
           [simple-signature generic-signature] (&host-generics/method-signatures method-decl)]
      (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC
                                    (if =final? Opcodes/ACC_FINAL 0))
                          =name
                          simple-signature
                          generic-signature
                          (->> =exceptions (&/|map &host-generics/gclass->bytecode-class-name) &/->seq (into-array java.lang.String)))
        .visitCode
        (dummy-return =output)
        (.visitMaxs 0 0)
        (.visitEnd)))
    
    (&/$OverridenMethodSyntax =class-decl =name ?strict =anns =gvars =exceptions =inputs =output body)
    (|let [method-decl [=name
                        =anns
                        =gvars
                        (&/|map (partial ->dummy-type real-name store-name) =exceptions)
                        (&/|map (comp (partial ->dummy-type real-name store-name) &/|second) =inputs)
                        (->dummy-type real-name store-name =output)]
           [simple-signature generic-signature] (&host-generics/method-signatures method-decl)]
      (doto (.visitMethod =class Opcodes/ACC_PUBLIC
                          =name
                          simple-signature
                          generic-signature
                          (->> =exceptions (&/|map &host-generics/gclass->bytecode-class-name) &/->seq (into-array java.lang.String)))
        .visitCode
        (dummy-return =output)
        (.visitMaxs 0 0)
        (.visitEnd)))

    (&/$StaticMethodSyntax =name =privacy-modifier ?strict =anns =gvars =exceptions =inputs =output body)
    (|let [method-decl [=name
                        =anns
                        =gvars
                        (&/|map (partial ->dummy-type real-name store-name) =exceptions)
                        (&/|map (comp (partial ->dummy-type real-name store-name) &/|second) =inputs)
                        (->dummy-type real-name store-name =output)]
           [simple-signature generic-signature] (&host-generics/method-signatures method-decl)]
      (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC)
                          =name
                          simple-signature
                          generic-signature
                          (->> =exceptions (&/|map &host-generics/gclass->bytecode-class-name) &/->seq (into-array java.lang.String)))
        .visitCode
        (dummy-return =output)
        (.visitMaxs 0 0)
        (.visitEnd)))

    (&/$AbstractMethodSyntax =name =privacy-modifier =anns =gvars =exceptions =inputs =output)
    (|let [method-decl [=name
                        =anns
                        =gvars
                        (&/|map (partial ->dummy-type real-name store-name) =exceptions)
                        (&/|map (comp (partial ->dummy-type real-name store-name) &/|second) =inputs)
                        (->dummy-type real-name store-name =output)]
           [simple-signature generic-signature] (&host-generics/method-signatures method-decl)]
      (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT)
                          =name
                          simple-signature
                          generic-signature
                          (->> =exceptions (&/|map &host-generics/gclass->bytecode-class-name) &/->seq (into-array java.lang.String)))
        (.visitEnd)))

    (&/$NativeMethodSyntax =name =privacy-modifier =anns =gvars =exceptions =inputs =output)
    (|let [method-decl [=name
                        =anns
                        =gvars
                        (&/|map (partial ->dummy-type real-name store-name) =exceptions)
                        (&/|map (comp (partial ->dummy-type real-name store-name) &/|second) =inputs)
                        (->dummy-type real-name store-name =output)]
           [simple-signature generic-signature] (&host-generics/method-signatures method-decl)]
      (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_NATIVE)
                          =name
                          simple-signature
                          generic-signature
                          (->> =exceptions (&/|map &host-generics/gclass->bytecode-class-name) &/->seq (into-array java.lang.String)))
        (.visitEnd)))

    _
    (assert false (println-str 'compile-dummy-method (&/adt->text method-def)))
    ))

(defn privacy-modifier->flag
  "(-> PrivacyModifier Int)"
  [privacy-modifier]
  (|case privacy-modifier
    (&/$PublicPM)    Opcodes/ACC_PUBLIC
    (&/$PrivatePM)   Opcodes/ACC_PRIVATE
    (&/$ProtectedPM) Opcodes/ACC_PROTECTED
    (&/$DefaultPM)   0
    ))

(defn state-modifier->flag
  "(-> StateModifier Int)"
  [state-modifier]
  (|case state-modifier
    (&/$DefaultSM)  0
    (&/$VolatileSM) Opcodes/ACC_VOLATILE
    (&/$FinalSM)    Opcodes/ACC_FINAL))

(defn inheritance-modifier->flag
  "(-> InheritanceModifier Int)"
  [inheritance-modifier]
  (|case inheritance-modifier
    (&/$DefaultIM)  0
    (&/$AbstractIM) Opcodes/ACC_ABSTRACT
    (&/$FinalIM)    Opcodes/ACC_FINAL))

(defn use-dummy-class [class-decl super-class interfaces ctor-args fields methods]
  (|do [module &/get-module-name
        :let [[?name ?params] class-decl
              dummy-name ?name;; (str ?name "__DUMMY__")
              dummy-full-name (str module "/" dummy-name)
              real-name (str (&host-generics/->class-name module) "." ?name)
              store-name (str (&host-generics/->class-name module) "." dummy-name)
              class-signature (&host-generics/gclass-decl->signature class-decl (&/$Item super-class interfaces))
              =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                       (.visit bytecode-version (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                               dummy-full-name
                               (if (= "" class-signature) nil class-signature)
                               (&host-generics/->bytecode-class-name (&host-generics/super-class-name super-class))
                               (->> interfaces (&/|map (comp &host-generics/->bytecode-class-name &host-generics/super-class-name)) &/->seq (into-array String))))
              _ (&/|map (fn [field]
                          (|case field
                            (&/$ConstantFieldAnalysis =name =anns =type ?value)
                            (doto (.visitField =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC Opcodes/ACC_FINAL) =name
                                               (&host-generics/gclass->simple-signature =type)
                                               (&host-generics/gclass->signature =type)
                                               nil)
                              (.visitEnd))
                            
                            (&/$VariableFieldAnalysis =name =privacy-modifier =state-modifier =anns =type)
                            (doto (.visitField =class (+ Opcodes/ACC_PUBLIC (state-modifier->flag =state-modifier)) =name
                                               (&host-generics/gclass->simple-signature =type)
                                               (&host-generics/gclass->signature =type)
                                               nil)
                              (.visitEnd))
                            ))
                        fields)
              _ (&/|map (partial compile-dummy-method =class real-name store-name super-class) methods)
              bytecode (.toByteArray (doto =class .visitEnd))]
        ^ClassLoader loader &/loader
        !classes &/classes
        :let [_ (swap! !classes assoc store-name bytecode)
              _ (.loadClass loader store-name)]
        _ (&/push-dummy-name real-name store-name)]
    (return nil)))
