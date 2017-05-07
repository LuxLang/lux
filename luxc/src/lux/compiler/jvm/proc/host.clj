(ns lux.compiler.jvm.proc.host
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return |let |case]]
                 [type :as &type]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser]
                 [optimizer :as &o]
                 [host :as &host])
            [lux.type.host :as &host-type]
            [lux.host.generics :as &host-generics]
            [lux.analyser.base :as &a]
            [lux.compiler.jvm.base :as &&])
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor
                              AnnotationVisitor)))

;; [Utils]
(def init-method "<init>")

(let [class+method+sig {"boolean" &&/unwrap-boolean
                        "byte"    &&/unwrap-byte
                        "short"   &&/unwrap-short
                        "int"     &&/unwrap-int
                        "long"    &&/unwrap-long
                        "float"   &&/unwrap-float
                        "double"  &&/unwrap-double
                        "char"    &&/unwrap-char}]
  (defn ^:private prepare-arg! [^MethodVisitor *writer* class-name]
    (if-let [unwrap (get class+method+sig class-name)]
      (doto *writer*
        unwrap)
      (.visitTypeInsn *writer* Opcodes/CHECKCAST (&host-generics/->bytecode-class-name class-name)))))

(let [boolean-class "java.lang.Boolean"
      byte-class "java.lang.Byte"
      short-class "java.lang.Short"
      int-class "java.lang.Integer"
      long-class "java.lang.Long"
      float-class "java.lang.Float"
      double-class "java.lang.Double"
      char-class "java.lang.Character"]
  (defn prepare-return! [^MethodVisitor *writer* *type*]
    (|case *type*
      (&/$Unit)
      (.visitLdcInsn *writer* &/unit-tag)

      (&/$Host "boolean" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name boolean-class) "valueOf" (str "(Z)" (&host-generics/->type-signature boolean-class)))
      
      (&/$Host "byte" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name byte-class) "valueOf" (str "(B)" (&host-generics/->type-signature byte-class)))

      (&/$Host "short" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name short-class) "valueOf" (str "(S)" (&host-generics/->type-signature short-class)))

      (&/$Host "int" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name int-class) "valueOf" (str "(I)" (&host-generics/->type-signature int-class)))

      (&/$Host "long" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name long-class) "valueOf" (str "(J)" (&host-generics/->type-signature long-class)))

      (&/$Host "float" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name float-class) "valueOf" (str "(F)" (&host-generics/->type-signature float-class)))

      (&/$Host "double" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name double-class) "valueOf" (str "(D)" (&host-generics/->type-signature double-class)))

      (&/$Host "char" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name char-class) "valueOf" (str "(C)" (&host-generics/->type-signature char-class)))
      
      (&/$Host _ _)
      nil

      (&/$Named ?name ?type)
      (prepare-return! *writer* ?type)

      (&/$Ex _)
      nil

      _
      (assert false (str 'prepare-return! " " (&type/show-type *type*))))
    *writer*))

;; [Resources]
(defn ^:private compile-annotation [writer ann]
  (doto ^AnnotationVisitor (.visitAnnotation writer (&host-generics/->type-signature (:name ann)) true)
        (-> (.visit param-name param-value)
            (->> (|let [[param-name param-value] param])
                 (doseq [param (&/->seq (:params ann))])))
        (.visitEnd))
  nil)

(defn ^:private compile-field [^ClassWriter writer field]
  (|case field
    (&/$ConstantFieldSyntax ?name ?anns ?gclass ?value)
    (|let [=field (.visitField writer
                               (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC Opcodes/ACC_FINAL)
                               ?name
                               (&host-generics/gclass->simple-signature ?gclass)
                               (&host-generics/gclass->signature ?gclass) nil)]
      (do (&/|map (partial compile-annotation =field) ?anns)
        (.visitEnd =field)
        nil))
    
    (&/$VariableFieldSyntax =name =privacy-modifier =state-modifier =anns =type)
    (|let [=field (.visitField writer
                               (+ (&host/privacy-modifier->flag =privacy-modifier)
                                  (&host/state-modifier->flag =state-modifier))
                               =name
                               (&host-generics/gclass->simple-signature =type)
                               (&host-generics/gclass->signature =type) nil)]
      (do (&/|map (partial compile-annotation =field) =anns)
        (.visitEnd =field)
        nil))
    ))

(defn ^:private compile-method-return [^MethodVisitor writer output]
  (|case output
    (&/$GenericClass "void" (&/$Nil))
    (.visitInsn writer Opcodes/RETURN)
    
    (&/$GenericClass "boolean" (&/$Nil))
    (doto writer
      &&/unwrap-boolean
      (.visitInsn Opcodes/IRETURN))
    
    (&/$GenericClass "byte" (&/$Nil))
    (doto writer
      &&/unwrap-byte
      (.visitInsn Opcodes/IRETURN))
    
    (&/$GenericClass "short" (&/$Nil))
    (doto writer
      &&/unwrap-short
      (.visitInsn Opcodes/IRETURN))
    
    (&/$GenericClass "int" (&/$Nil))
    (doto writer
      &&/unwrap-int
      (.visitInsn Opcodes/IRETURN))
    
    (&/$GenericClass "long" (&/$Nil))
    (doto writer
      &&/unwrap-long
      (.visitInsn Opcodes/LRETURN))
    
    (&/$GenericClass "float" (&/$Nil))
    (doto writer
      &&/unwrap-float
      (.visitInsn Opcodes/FRETURN))
    
    (&/$GenericClass "double" (&/$Nil))
    (doto writer
      &&/unwrap-double
      (.visitInsn Opcodes/DRETURN))
    
    (&/$GenericClass "char" (&/$Nil))
    (doto writer
      &&/unwrap-char
      (.visitInsn Opcodes/IRETURN))

    (&/$GenericClass _class-name (&/$Nil))
    (doto writer
      (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name _class-name))
      (.visitInsn Opcodes/ARETURN))
    
    _
    (.visitInsn writer Opcodes/ARETURN)))

(defn ^:private prepare-method-input [idx input ^MethodVisitor method-visitor]
  "(-> Int [Text GenericClass] MethodVisitor (Lux FrameTag))"
  (|case input
    [_ (&/$GenericClass name params)]
    (case name
      "boolean" (do (doto method-visitor
                      (.visitVarInsn Opcodes/ILOAD idx)
                      &&/wrap-boolean
                      (.visitVarInsn Opcodes/ASTORE idx))
                  (return (&/T [(inc idx) (&/|list (&host-generics/gclass->class-name (&/$GenericClass "java.lang.Boolean" (&/|list))))])))
      "byte"    (do (doto method-visitor
                      (.visitVarInsn Opcodes/ILOAD idx)
                      &&/wrap-byte
                      (.visitVarInsn Opcodes/ASTORE idx))
                  (return (&/T [(inc idx) (&/|list (&host-generics/gclass->class-name (&/$GenericClass "java.lang.Byte" (&/|list))))])))
      "short"   (do (doto method-visitor
                      (.visitVarInsn Opcodes/ILOAD idx)
                      &&/wrap-short
                      (.visitVarInsn Opcodes/ASTORE idx))
                  (return (&/T [(inc idx) (&/|list (&host-generics/gclass->class-name (&/$GenericClass "java.lang.Short" (&/|list))))])))
      "int"     (do (doto method-visitor
                      (.visitVarInsn Opcodes/ILOAD idx)
                      &&/wrap-int
                      (.visitVarInsn Opcodes/ASTORE idx))
                  (return (&/T [(inc idx) (&/|list (&host-generics/gclass->class-name (&/$GenericClass "java.lang.Integer" (&/|list))))])))
      "long"    (do (doto method-visitor
                      (.visitVarInsn Opcodes/LLOAD idx)
                      &&/wrap-long
                      (.visitVarInsn Opcodes/ASTORE idx))
                  (return (&/T [(+ 2 idx) (&/|list (&host-generics/gclass->class-name (&/$GenericClass "java.lang.Long" (&/|list))) Opcodes/TOP)])))
      "float"   (do (doto method-visitor
                      (.visitVarInsn Opcodes/FLOAD idx)
                      &&/wrap-float
                      (.visitVarInsn Opcodes/ASTORE idx))
                  (return (&/T [(inc idx) (&/|list (&host-generics/gclass->class-name (&/$GenericClass "java.lang.Float" (&/|list))))])))
      "double"  (do (doto method-visitor
                      (.visitVarInsn Opcodes/DLOAD idx)
                      &&/wrap-double
                      (.visitVarInsn Opcodes/ASTORE idx))
                  (return (&/T [(+ 2 idx) (&/|list (&host-generics/gclass->class-name (&/$GenericClass "java.lang.Double" (&/|list))) Opcodes/TOP)])))
      "char"    (do (doto method-visitor
                      (.visitVarInsn Opcodes/ILOAD idx)
                      &&/wrap-char
                      (.visitVarInsn Opcodes/ASTORE idx))
                  (return (&/T [(inc idx) (&/|list (&host-generics/gclass->class-name (&/$GenericClass "java.lang.Character" (&/|list))))])))
      ;; else
      (return (&/T [(inc idx) (&/|list (&host-generics/gclass->class-name (&/$GenericClass name params)))])))

    [_ gclass]
    (return (&/T [(inc idx) (&/|list (&host-generics/gclass->class-name gclass))]))
    ))

(defn ^:private prepare-method-inputs [idx inputs method-visitor]
  "(-> Int (List GenericClass) MethodVisitor (Lux (List FrameTag)))"
  (|case inputs
    (&/$Nil)
    (return &/$Nil)
    
    (&/$Cons input inputs*)
    (|do [[_ outputs*] (&/fold% (fn [idx+outputs input]
                                  (|do [:let [[_idx _outputs] idx+outputs]
                                        [idx* output] (prepare-method-input _idx input method-visitor)]
                                    (return (&/T [idx* (&/$Cons output _outputs)]))))
                                (&/T [idx &/$Nil])
                                inputs)]
      (return (&/list-join (&/|reverse outputs*))))
    ))

(defn ^:private compile-method-def [compile ^ClassWriter class-writer bytecode-class-name ?super-class method-def]
  (|case method-def
    (&/$ConstructorMethodAnalysis ?privacy-modifier ?strict ?anns ?gvars ?exceptions ?inputs ?ctor-args ?body)
    (|let [?output (&/$GenericClass "void" (&/|list))
           =method-decl (&/T [init-method ?anns ?gvars ?exceptions (&/|map &/|second ?inputs) ?output])
           [simple-signature generic-signature] (&host-generics/method-signatures =method-decl)]
      (&/with-writer (.visitMethod class-writer
                                   (+ (&host/privacy-modifier->flag ?privacy-modifier)
                                      (if ?strict Opcodes/ACC_STRICT 0))
                                   init-method
                                   simple-signature
                                   generic-signature
                                   (->> ?exceptions (&/|map &host-generics/gclass->class-name) &/->seq (into-array java.lang.String)))
        (|do [^MethodVisitor =method &/get-writer
              :let [[super-class-name super-class-params] ?super-class
                    init-types (->> ?ctor-args (&/|map (comp &host-generics/gclass->signature &/|first)) (&/fold str ""))
                    init-sig (str "(" init-types ")" "V")
                    _ (&/|map (partial compile-annotation =method) ?anns)
                    _ (.visitCode =method)]
              =input-tags (prepare-method-inputs 1 ?inputs =method)
              :let [_ (.visitVarInsn =method Opcodes/ALOAD 0)]
              _ (->> ?ctor-args (&/|map &/|second) (&/map% compile))
              :let [_ (.visitMethodInsn =method Opcodes/INVOKESPECIAL (&host-generics/->bytecode-class-name super-class-name) init-method init-sig)]
              _ (compile (&o/optimize ?body))
              :let [_ (doto =method
                        (compile-method-return ?output)
                        (.visitMaxs 0 0)
                        (.visitEnd))]]
          (return nil))))
    
    (&/$VirtualMethodAnalysis ?name ?privacy-modifier =final? ?strict ?anns ?gvars ?exceptions ?inputs ?output ?body)
    (|let [=method-decl (&/T [?name ?anns ?gvars ?exceptions (&/|map &/|second ?inputs) ?output])
           [simple-signature generic-signature] (&host-generics/method-signatures =method-decl)]
      (&/with-writer (.visitMethod class-writer
                                   (+ (&host/privacy-modifier->flag ?privacy-modifier)
                                      (if =final? Opcodes/ACC_FINAL 0)
                                      (if ?strict Opcodes/ACC_STRICT 0))
                                   ?name
                                   simple-signature
                                   generic-signature
                                   (->> ?exceptions (&/|map &host-generics/gclass->class-name) &/->seq (into-array java.lang.String)))
        (|do [^MethodVisitor =method &/get-writer
              :let [_ (&/|map (partial compile-annotation =method) ?anns)
                    _ (.visitCode =method)]
              =input-tags (prepare-method-inputs 1 ?inputs =method)
              _ (compile (&o/optimize ?body))
              :let [_ (doto =method
                        (compile-method-return ?output)
                        (.visitMaxs 0 0)
                        (.visitEnd))]]
          (return nil))))
    
    (&/$OverridenMethodAnalysis ?class-decl ?name ?strict ?anns ?gvars ?exceptions ?inputs ?output ?body)
    (|let [=method-decl (&/T [?name ?anns ?gvars ?exceptions (&/|map &/|second ?inputs) ?output])
           [simple-signature generic-signature] (&host-generics/method-signatures =method-decl)]
      (&/with-writer (.visitMethod class-writer
                                   (+ Opcodes/ACC_PUBLIC
                                      (if ?strict Opcodes/ACC_STRICT 0))
                                   ?name
                                   simple-signature
                                   generic-signature
                                   (->> ?exceptions (&/|map &host-generics/gclass->class-name) &/->seq (into-array java.lang.String)))
        (|do [^MethodVisitor =method &/get-writer
              :let [_ (&/|map (partial compile-annotation =method) ?anns)
                    _ (.visitCode =method)]
              =input-tags (prepare-method-inputs 1 ?inputs =method)
              _ (compile (&o/optimize ?body))
              :let [_ (doto =method
                        (compile-method-return ?output)
                        (.visitMaxs 0 0)
                        (.visitEnd))]]
          (return nil))))

    (&/$StaticMethodAnalysis ?name ?privacy-modifier ?strict ?anns ?gvars ?exceptions ?inputs ?output ?body)
    (|let [=method-decl (&/T [?name ?anns ?gvars ?exceptions (&/|map &/|second ?inputs) ?output])
           [simple-signature generic-signature] (&host-generics/method-signatures =method-decl)]
      (&/with-writer (.visitMethod class-writer
                                   (+ (&host/privacy-modifier->flag ?privacy-modifier)
                                      (if ?strict Opcodes/ACC_STRICT 0)
                                      Opcodes/ACC_STATIC)
                                   ?name
                                   simple-signature
                                   generic-signature
                                   (->> ?exceptions (&/|map &host-generics/gclass->class-name) &/->seq (into-array java.lang.String)))
        (|do [^MethodVisitor =method &/get-writer
              :let [_ (&/|map (partial compile-annotation =method) ?anns)
                    _ (.visitCode =method)]
              =input-tags (prepare-method-inputs 0 ?inputs =method)
              _ (compile (&o/optimize ?body))
              :let [_ (doto =method
                        (compile-method-return ?output)
                        (.visitMaxs 0 0)
                        (.visitEnd))]]
          (return nil))))

    (&/$AbstractMethodSyntax ?name ?privacy-modifier ?anns ?gvars ?exceptions ?inputs ?output)
    (|let [=method-decl (&/T [?name ?anns ?gvars ?exceptions (&/|map &/|second ?inputs) ?output])
           [simple-signature generic-signature] (&host-generics/method-signatures =method-decl)]
      (&/with-writer (.visitMethod class-writer
                                   (+ Opcodes/ACC_ABSTRACT
                                      (&host/privacy-modifier->flag ?privacy-modifier))
                                   ?name
                                   simple-signature
                                   generic-signature
                                   (->> ?exceptions (&/|map &host-generics/gclass->class-name) &/->seq (into-array java.lang.String)))
        (|do [^MethodVisitor =method &/get-writer
              :let [_ (&/|map (partial compile-annotation =method) ?anns)
                    _ (.visitEnd =method)]]
          (return nil))))

    (&/$NativeMethodSyntax ?name ?privacy-modifier ?anns ?gvars ?exceptions ?inputs ?output)
    (|let [=method-decl (&/T [?name ?anns ?gvars ?exceptions (&/|map &/|second ?inputs) ?output])
           [simple-signature generic-signature] (&host-generics/method-signatures =method-decl)]
      (&/with-writer (.visitMethod class-writer
                                   (+ Opcodes/ACC_PUBLIC Opcodes/ACC_NATIVE
                                      (&host/privacy-modifier->flag ?privacy-modifier))
                                   ?name
                                   simple-signature
                                   generic-signature
                                   (->> ?exceptions (&/|map &host-generics/gclass->class-name) &/->seq (into-array java.lang.String)))
        (|do [^MethodVisitor =method &/get-writer
              :let [_ (&/|map (partial compile-annotation =method) ?anns)
                    _ (.visitEnd =method)]]
          (return nil))))
    ))

(defn ^:private compile-method-decl [^ClassWriter class-writer =method-decl]
  (|let [[=name =anns =gvars =exceptions =inputs =output] =method-decl
         [simple-signature generic-signature] (&host-generics/method-signatures =method-decl)
         =method (.visitMethod class-writer
                               (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT)
                               =name
                               simple-signature
                               generic-signature
                               (->> =exceptions (&/|map &host-generics/gclass->class-name) &/->seq (into-array java.lang.String)))
         _ (&/|map (partial compile-annotation =method) =anns)
         _ (.visitEnd =method)]
    nil))

(defn ^:private prepare-ctor-arg [^MethodVisitor writer type]
  (case type
    "boolean" (doto writer
                &&/unwrap-boolean)
    "byte" (doto writer
             &&/unwrap-byte)
    "short" (doto writer
              &&/unwrap-short)
    "int" (doto writer
            &&/unwrap-int)
    "long" (doto writer
             &&/unwrap-long)
    "float" (doto writer
              &&/unwrap-float)
    "double" (doto writer
               &&/unwrap-double)
    "char" (doto writer
             &&/unwrap-char)
    ;; else
    (doto writer
      (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name type)))))

(let [clo-field-sig (&host-generics/->type-signature "java.lang.Object")
      <init>-return "V"]
  (defn ^:private anon-class-<init>-signature [env]
    (str "(" (&/fold str "" (&/|repeat (&/|length env) clo-field-sig)) ")"
         <init>-return))

  (defn ^:private add-anon-class-<init> [^ClassWriter class-writer compile class-name super-class env ctor-args]
    (|let [[super-class-name super-class-params] super-class
           init-types (->> ctor-args (&/|map (comp &host-generics/->type-signature &/|first)) (&/fold str ""))]
      (&/with-writer (.visitMethod class-writer Opcodes/ACC_PUBLIC init-method (anon-class-<init>-signature env) nil nil)
        (|do [^MethodVisitor =method &/get-writer
              :let [_ (doto =method
                        (.visitCode)
                        (.visitVarInsn Opcodes/ALOAD 0))]
              _ (&/map% (fn [type+term]
                          (|let [[type term] type+term]
                            (|do [_ (compile term)
                                  :let [_ (prepare-ctor-arg =method type)]]
                              (return nil))))
                        ctor-args)
              :let [_ (doto =method
                        (.visitMethodInsn Opcodes/INVOKESPECIAL (&host-generics/->bytecode-class-name super-class-name) init-method (str "(" init-types ")" <init>-return))
                        (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
                              (.visitVarInsn Opcodes/ALOAD (inc ?captured-id))
                              (.visitFieldInsn Opcodes/PUTFIELD class-name captured-name clo-field-sig))
                            (->> (let [captured-name (str &&/closure-prefix ?captured-id)])
                                 (|case ?name+?captured
                                   [?name [_ (&o/$captured _ ?captured-id ?source)]])
                                 (doseq [?name+?captured (&/->seq env)])))
                        (.visitInsn Opcodes/RETURN)
                        (.visitMaxs 0 0)
                        (.visitEnd))]]
          (return nil)))))
  )

(defn ^:private constant-inits [fields]
  "(-> (List FieldAnalysis) (List [Text GenericClass Analysis]))"
  (&/fold &/|++
          &/$Nil
          (&/|map (fn [field]
                    (|case field
                      (&/$ConstantFieldSyntax ?name ?anns ?gclass ?value)
                      (&/|list (&/T [?name ?gclass ?value]))
                      
                      (&/$VariableFieldSyntax _)
                      (&/|list)
                      ))
                  fields)))

(declare compile-jvm-putstatic)
(defn compile-jvm-class [compile class-decl ?super-class ?interfaces ?inheritance-modifier ?anns ?fields ?methods env ??ctor-args]
  (|do [module &/get-module-name
        [file-name line column] &/cursor
        :let [[?name ?params] class-decl
              class-signature (&host-generics/gclass-decl->signature class-decl (&/$Cons ?super-class ?interfaces))
              full-name (str module "/" ?name)
              super-class* (&host-generics/->bytecode-class-name (&host-generics/super-class-name ?super-class))
              =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                       (.visit &host/bytecode-version (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER
                                                         (&host/inheritance-modifier->flag ?inheritance-modifier))
                               full-name (if (= "" class-signature) nil class-signature) super-class* (->> ?interfaces (&/|map (comp &host-generics/->bytecode-class-name &host-generics/super-class-name)) &/->seq (into-array String)))
                       (.visitSource file-name nil))
              _ (&/|map (partial compile-annotation =class) ?anns)
              _ (&/|map (partial compile-field =class)
                        ?fields)]
        _ (&/map% (partial compile-method-def compile =class full-name ?super-class) ?methods)
        _ (|case ??ctor-args
            (&/$Some ctor-args)
            (add-anon-class-<init> =class compile full-name ?super-class env ctor-args)

            _
            (return nil))
        _ (&/with-writer (.visitMethod =class Opcodes/ACC_STATIC "<clinit>" "()V" nil nil)
            (|do [^MethodVisitor =method &/get-writer
                  :let [_ (doto =method
                            (.visitCode))]
                  _ (&/map% (fn [ftriple]
                              (|let [[fname fgclass fvalue] ftriple]
                                (compile-jvm-putstatic compile (&/|list (&o/optimize fvalue)) (&/|list ?name fname fgclass))))
                            (constant-inits ?fields))
                  :let [_ (doto =method
                            (.visitInsn Opcodes/RETURN)
                            (.visitMaxs 0 0)
                            (.visitEnd))]]
              (return nil)))]
    (&&/save-class! ?name (.toByteArray (doto =class .visitEnd)))))

(defn compile-jvm-interface [interface-decl ?supers ?anns ?methods]
  (|do [:let [[interface-name interface-vars] interface-decl]
        module &/get-module-name
        [file-name _ _] &/cursor
        :let [interface-signature (&host-generics/gclass-decl->signature interface-decl ?supers)
              =interface (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                           (.visit &host/bytecode-version (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT Opcodes/ACC_INTERFACE)
                                   (str module "/" interface-name)
                                   (if (= "" interface-signature) nil interface-signature)
                                   "java/lang/Object"
                                   (->> ?supers (&/|map (comp &host-generics/->bytecode-class-name &host-generics/super-class-name)) &/->seq (into-array String)))
                           (.visitSource file-name nil))
              _ (&/|map (partial compile-annotation =interface) ?anns)
              _ (do (&/|map (partial compile-method-decl =interface) ?methods)
                  (.visitEnd =interface))]]
    (&&/save-class! interface-name (.toByteArray =interface))))

(do-template [<name> <op> <unwrap> <wrap>]
  (defn <name> [compile _?value special-args]
    (|do [:let [(&/$Cons ?value (&/$Nil)) _?value]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?value)
          :let [_ (doto *writer*
                    <unwrap>
                    (.visitInsn <op>)
                    <wrap>)]]
      (return nil)))

  ^:private compile-jvm-d2f Opcodes/D2F &&/unwrap-double &&/wrap-float
  ^:private compile-jvm-d2i Opcodes/D2I &&/unwrap-double &&/wrap-int
  ^:private compile-jvm-d2l Opcodes/D2L &&/unwrap-double &&/wrap-long

  ^:private compile-jvm-f2d Opcodes/F2D &&/unwrap-float &&/wrap-double
  ^:private compile-jvm-f2i Opcodes/F2I &&/unwrap-float &&/wrap-int
  ^:private compile-jvm-f2l Opcodes/F2L &&/unwrap-float &&/wrap-long

  ^:private compile-jvm-i2b Opcodes/I2B &&/unwrap-int &&/wrap-byte
  ^:private compile-jvm-i2c Opcodes/I2C &&/unwrap-int &&/wrap-char
  ^:private compile-jvm-i2d Opcodes/I2D &&/unwrap-int &&/wrap-double
  ^:private compile-jvm-i2f Opcodes/I2F &&/unwrap-int &&/wrap-float
  ^:private compile-jvm-i2l Opcodes/I2L &&/unwrap-int &&/wrap-long
  ^:private compile-jvm-i2s Opcodes/I2S &&/unwrap-int &&/wrap-short

  ^:private compile-jvm-l2d Opcodes/L2D &&/unwrap-long &&/wrap-double
  ^:private compile-jvm-l2f Opcodes/L2F &&/unwrap-long &&/wrap-float
  ^:private compile-jvm-l2i Opcodes/L2I &&/unwrap-long &&/wrap-int

  ^:private compile-jvm-c2b Opcodes/I2B &&/unwrap-char &&/wrap-byte
  ^:private compile-jvm-c2s Opcodes/I2S &&/unwrap-char &&/wrap-short
  ^:private compile-jvm-c2i Opcodes/NOP &&/unwrap-char &&/wrap-int
  ^:private compile-jvm-c2l Opcodes/I2L &&/unwrap-char &&/wrap-long

  ^:private compile-jvm-s2l Opcodes/I2L &&/unwrap-short &&/wrap-long
  
  ^:private compile-jvm-b2l Opcodes/I2L &&/unwrap-byte &&/wrap-long
  )

(do-template [<name> <op> <wrap>]
  (defn <name> [compile _?value special-args]
    (|do [:let [(&/$Cons ?value (&/$Nil)) _?value]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?value)
          :let [_ (doto *writer*
                    &&/unwrap-long
                    (.visitInsn Opcodes/L2I)
                    (.visitInsn <op>)
                    <wrap>)]]
      (return nil)))

  ^:private compile-jvm-l2s Opcodes/I2S &&/wrap-short
  ^:private compile-jvm-l2b Opcodes/I2B &&/wrap-byte
  )

(do-template [<name> <op> <unwrap-left> <unwrap-right> <wrap>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?x)
          :let [_ (doto *writer*
                    <unwrap-left>)]
          _ (compile ?y)
          :let [_ (doto *writer*
                    <unwrap-right>)]
          :let [_ (doto *writer*
                    (.visitInsn <op>)
                    <wrap>)]]
      (return nil)))

  ^:private compile-jvm-iand  Opcodes/IAND  &&/unwrap-int  &&/unwrap-int  &&/wrap-int
  ^:private compile-jvm-ior   Opcodes/IOR   &&/unwrap-int  &&/unwrap-int  &&/wrap-int
  ^:private compile-jvm-ixor  Opcodes/IXOR  &&/unwrap-int  &&/unwrap-int  &&/wrap-int
  ^:private compile-jvm-ishl  Opcodes/ISHL  &&/unwrap-int  &&/unwrap-int  &&/wrap-int
  ^:private compile-jvm-ishr  Opcodes/ISHR  &&/unwrap-int  &&/unwrap-int  &&/wrap-int
  ^:private compile-jvm-iushr Opcodes/IUSHR &&/unwrap-int  &&/unwrap-int  &&/wrap-int
  
  ^:private compile-jvm-land  Opcodes/LAND  &&/unwrap-long &&/unwrap-long &&/wrap-long
  ^:private compile-jvm-lor   Opcodes/LOR   &&/unwrap-long &&/unwrap-long &&/wrap-long
  ^:private compile-jvm-lxor  Opcodes/LXOR  &&/unwrap-long &&/unwrap-long &&/wrap-long
  ^:private compile-jvm-lshl  Opcodes/LSHL  &&/unwrap-long &&/unwrap-int  &&/wrap-long
  ^:private compile-jvm-lshr  Opcodes/LSHR  &&/unwrap-long &&/unwrap-int  &&/wrap-long
  ^:private compile-jvm-lushr Opcodes/LUSHR &&/unwrap-long &&/unwrap-int  &&/wrap-long
  )

(do-template [<name> <opcode> <unwrap> <wrap>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?x)
          :let [_ (doto *writer*
                    <unwrap>)]
          _ (compile ?y)
          :let [_ (doto *writer*
                    <unwrap>)
                _ (doto *writer*
                    (.visitInsn <opcode>)
                    (<wrap>))]]
      (return nil)))

  ^:private compile-jvm-iadd Opcodes/IADD &&/unwrap-int &&/wrap-int
  ^:private compile-jvm-isub Opcodes/ISUB &&/unwrap-int &&/wrap-int
  ^:private compile-jvm-imul Opcodes/IMUL &&/unwrap-int &&/wrap-int
  ^:private compile-jvm-idiv Opcodes/IDIV &&/unwrap-int &&/wrap-int
  ^:private compile-jvm-irem Opcodes/IREM &&/unwrap-int &&/wrap-int
  
  ^:private compile-jvm-ladd Opcodes/LADD &&/unwrap-long &&/wrap-long
  ^:private compile-jvm-lsub Opcodes/LSUB &&/unwrap-long &&/wrap-long
  ^:private compile-jvm-lmul Opcodes/LMUL &&/unwrap-long &&/wrap-long
  ^:private compile-jvm-ldiv Opcodes/LDIV &&/unwrap-long &&/wrap-long
  ^:private compile-jvm-lrem Opcodes/LREM &&/unwrap-long &&/wrap-long

  ^:private compile-jvm-fadd Opcodes/FADD &&/unwrap-float &&/wrap-float
  ^:private compile-jvm-fsub Opcodes/FSUB &&/unwrap-float &&/wrap-float
  ^:private compile-jvm-fmul Opcodes/FMUL &&/unwrap-float &&/wrap-float
  ^:private compile-jvm-fdiv Opcodes/FDIV &&/unwrap-float &&/wrap-float
  ^:private compile-jvm-frem Opcodes/FREM &&/unwrap-float &&/wrap-float
  
  ^:private compile-jvm-dadd Opcodes/DADD &&/unwrap-double &&/wrap-double
  ^:private compile-jvm-dsub Opcodes/DSUB &&/unwrap-double &&/wrap-double
  ^:private compile-jvm-dmul Opcodes/DMUL &&/unwrap-double &&/wrap-double
  ^:private compile-jvm-ddiv Opcodes/DDIV &&/unwrap-double &&/wrap-double
  ^:private compile-jvm-drem Opcodes/DREM &&/unwrap-double &&/wrap-double
  )

(do-template [<name> <opcode> <unwrap>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?x)
          :let [_ (doto *writer*
                    <unwrap>)]
          _ (compile ?y)
          :let [_ (doto *writer*
                    <unwrap>)
                $then (new Label)
                $end (new Label)
                _ (doto *writer*
                    (.visitJumpInsn <opcode> $then)
                    (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "FALSE" (&host-generics/->type-signature "java.lang.Boolean"))
                    (.visitJumpInsn Opcodes/GOTO $end)
                    (.visitLabel $then)
                    (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "TRUE"  (&host-generics/->type-signature "java.lang.Boolean"))
                    (.visitLabel $end))]]
      (return nil)))

  ^:private compile-jvm-ieq Opcodes/IF_ICMPEQ &&/unwrap-int
  ^:private compile-jvm-ilt Opcodes/IF_ICMPLT &&/unwrap-int
  ^:private compile-jvm-igt Opcodes/IF_ICMPGT &&/unwrap-int

  ^:private compile-jvm-ceq Opcodes/IF_ICMPEQ &&/unwrap-char
  ^:private compile-jvm-clt Opcodes/IF_ICMPLT &&/unwrap-char
  ^:private compile-jvm-cgt Opcodes/IF_ICMPGT &&/unwrap-char
  )

(do-template [<name> <cmpcode> <cmp-output> <unwrap>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?x)
          :let [_ (doto *writer*
                    <unwrap>)]
          _ (compile ?y)
          :let [_ (doto *writer*
                    <unwrap>)
                $then (new Label)
                $end (new Label)
                _ (doto *writer*
                    (.visitInsn <cmpcode>)
                    (.visitLdcInsn (int <cmp-output>))
                    (.visitJumpInsn Opcodes/IF_ICMPEQ $then)
                    (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "FALSE"  (&host-generics/->type-signature "java.lang.Boolean"))
                    (.visitJumpInsn Opcodes/GOTO $end)
                    (.visitLabel $then)
                    (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "TRUE" (&host-generics/->type-signature "java.lang.Boolean"))
                    (.visitLabel $end))]]
      (return nil)))

  ^:private compile-jvm-leq Opcodes/LCMP   0 &&/unwrap-long
  ^:private compile-jvm-llt Opcodes/LCMP  -1 &&/unwrap-long
  ^:private compile-jvm-lgt Opcodes/LCMP   1 &&/unwrap-long

  ^:private compile-jvm-feq Opcodes/FCMPG  0 &&/unwrap-float
  ^:private compile-jvm-flt Opcodes/FCMPG -1 &&/unwrap-float
  ^:private compile-jvm-fgt Opcodes/FCMPG  1 &&/unwrap-float
  
  ^:private compile-jvm-deq Opcodes/DCMPG  0 &&/unwrap-double
  ^:private compile-jvm-dlt Opcodes/DCMPG -1 &&/unwrap-double
  ^:private compile-jvm-dgt Opcodes/DCMPG  1 &&/unwrap-double
  )

(do-template [<prim-type> <array-type> <new-name> <load-name> <load-op> <store-name> <store-op> <wrapper> <unwrapper>]
  (do (defn <new-name> [compile ?values special-args]
        (|do [:let [(&/$Cons ?length (&/$Nil)) ?values
                    ;; (&/$Nil) special-args
                    ]
              ^MethodVisitor *writer* &/get-writer
              _ (compile ?length)
              :let [_ (doto *writer*
                        &&/unwrap-long
                        (.visitInsn Opcodes/L2I))]
              :let [_ (.visitIntInsn *writer* Opcodes/NEWARRAY <prim-type>)]]
          (return nil)))

    (defn <load-name> [compile ?values special-args]
      (|do [:let [(&/$Cons ?array (&/$Cons ?idx (&/$Nil))) ?values
                  ;; (&/$Nil) special-args
                  ]
            ^MethodVisitor *writer* &/get-writer
            _ (compile ?array)
            :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST <array-type>)]
            _ (compile ?idx)
            :let [_ (doto *writer*
                      &&/unwrap-long
                      (.visitInsn Opcodes/L2I))]
            :let [_ (doto *writer*
                      (.visitInsn <load-op>)
                      <wrapper>)]]
        (return nil)))

    (defn <store-name> [compile ?values special-args]
      (|do [:let [(&/$Cons ?array (&/$Cons ?idx (&/$Cons ?elem (&/$Nil)))) ?values
                  ;; (&/$Nil) special-args
                  ]
            ^MethodVisitor *writer* &/get-writer
            _ (compile ?array)
            :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST <array-type>)]
            :let [_ (.visitInsn *writer* Opcodes/DUP)]
            _ (compile ?idx)
            :let [_ (doto *writer*
                      &&/unwrap-long
                      (.visitInsn Opcodes/L2I))]
            _ (compile ?elem)
            :let [_ (doto *writer*
                      <unwrapper>
                      (.visitInsn <store-op>))]]
        (return nil)))
    )

  Opcodes/T_BOOLEAN "[Z" ^:private compile-jvm-znewarray compile-jvm-zaload Opcodes/BALOAD compile-jvm-zastore Opcodes/BASTORE &&/wrap-boolean &&/unwrap-boolean
  Opcodes/T_BYTE    "[B" ^:private compile-jvm-bnewarray compile-jvm-baload Opcodes/BALOAD compile-jvm-bastore Opcodes/BASTORE &&/wrap-byte    &&/unwrap-byte
  Opcodes/T_SHORT   "[S" ^:private compile-jvm-snewarray compile-jvm-saload Opcodes/SALOAD compile-jvm-sastore Opcodes/SASTORE &&/wrap-short   &&/unwrap-short
  Opcodes/T_INT     "[I" ^:private compile-jvm-inewarray compile-jvm-iaload Opcodes/IALOAD compile-jvm-iastore Opcodes/IASTORE &&/wrap-int     &&/unwrap-int
  Opcodes/T_LONG    "[J" ^:private compile-jvm-lnewarray compile-jvm-laload Opcodes/LALOAD compile-jvm-lastore Opcodes/LASTORE &&/wrap-long    &&/unwrap-long
  Opcodes/T_FLOAT   "[F" ^:private compile-jvm-fnewarray compile-jvm-faload Opcodes/FALOAD compile-jvm-fastore Opcodes/FASTORE &&/wrap-float   &&/unwrap-float
  Opcodes/T_DOUBLE  "[D" ^:private compile-jvm-dnewarray compile-jvm-daload Opcodes/DALOAD compile-jvm-dastore Opcodes/DASTORE &&/wrap-double  &&/unwrap-double
  Opcodes/T_CHAR    "[C" ^:private compile-jvm-cnewarray compile-jvm-caload Opcodes/CALOAD compile-jvm-castore Opcodes/CASTORE &&/wrap-char    &&/unwrap-char
  )

(defn ^:private compile-jvm-anewarray [compile ?values special-args]
  (|do [:let [(&/$Cons ?length (&/$Nil)) ?values
              (&/$Cons ?gclass (&/$Cons type-env (&/$Nil))) special-args]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?length)
        :let [_ (doto *writer*
                  &&/unwrap-long
                  (.visitInsn Opcodes/L2I))]
        :let [_ (.visitTypeInsn *writer* Opcodes/ANEWARRAY (&host-generics/gclass->bytecode-class-name* ?gclass type-env))]]
    (return nil)))

(defn ^:private compile-jvm-aaload [compile ?values special-args]
  (|do [:let [(&/$Cons ?array (&/$Cons ?idx (&/$Nil))) ?values
              ;; (&/$Nil) special-args
              ]
        ^MethodVisitor *writer* &/get-writer
        array-type (&host/->java-sig (&a/expr-type* ?array))
        _ (compile ?array)
        :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST array-type)]
        _ (compile ?idx)
        :let [_ (doto *writer*
                  &&/unwrap-long
                  (.visitInsn Opcodes/L2I))]
        :let [_ (.visitInsn *writer* Opcodes/AALOAD)]]
    (return nil)))

(defn ^:private compile-jvm-aastore [compile ?values special-args]
  (|do [:let [(&/$Cons ?array (&/$Cons ?idx (&/$Cons ?elem (&/$Nil)))) ?values
              ;; (&/$Nil) special-args
              ]
        ^MethodVisitor *writer* &/get-writer
        array-type (&host/->java-sig (&a/expr-type* ?array))
        _ (compile ?array)
        :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST array-type)]
        :let [_ (.visitInsn *writer* Opcodes/DUP)]
        _ (compile ?idx)
        :let [_ (doto *writer*
                  &&/unwrap-long
                  (.visitInsn Opcodes/L2I))]
        _ (compile ?elem)
        :let [_ (.visitInsn *writer* Opcodes/AASTORE)]]
    (return nil)))

(defn ^:private compile-jvm-arraylength [compile ?values special-args]
  (|do [:let [(&/$Cons ?array (&/$Nil)) ?values
              ;; (&/$Nil) special-args
              ]
        ^MethodVisitor *writer* &/get-writer
        array-type (&host/->java-sig (&a/expr-type* ?array))
        _ (compile ?array)
        :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST array-type)]
        :let [_ (doto *writer*
                  (.visitInsn Opcodes/ARRAYLENGTH)
                  (.visitInsn Opcodes/I2L)
                  &&/wrap-long)]]
    (return nil)))

(defn ^:private compile-jvm-null [compile ?values special-args]
  (|do [:let [;; (&/$Nil) ?values
              (&/$Nil) special-args]
        ^MethodVisitor *writer* &/get-writer
        :let [_ (.visitInsn *writer* Opcodes/ACONST_NULL)]]
    (return nil)))

(defn ^:private compile-jvm-null? [compile ?values special-args]
  (|do [:let [(&/$Cons ?object (&/$Nil)) ?values
              ;; (&/$Nil) special-args
              ]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?object)
        :let [$then (new Label)
              $end (new Label)
              _ (doto *writer*
                  (.visitJumpInsn Opcodes/IFNULL $then)
                  (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "FALSE" (&host-generics/->type-signature "java.lang.Boolean"))
                  (.visitJumpInsn Opcodes/GOTO $end)
                  (.visitLabel $then)
                  (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "TRUE"  (&host-generics/->type-signature "java.lang.Boolean"))
                  (.visitLabel $end))]]
    (return nil)))

(defn compile-jvm-synchronized [compile ?values special-args]
  (|do [:let [(&/$Cons ?monitor (&/$Cons ?expr (&/$Nil))) ?values
              ;; (&/$Nil) special-args
              ]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?monitor)
        :let [_ (doto *writer*
                  (.visitInsn Opcodes/DUP)
                  (.visitInsn Opcodes/MONITORENTER))]
        _ (compile ?expr)
        :let [_ (doto *writer*
                  (.visitInsn Opcodes/SWAP)
                  (.visitInsn Opcodes/MONITOREXIT))]]
    (return nil)))

(defn ^:private compile-jvm-throw [compile ?values special-args]
  (|do [:let [(&/$Cons ?ex (&/$Nil)) ?values
              ;; (&/$Nil) special-args
              ]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?ex)
        :let [_ (.visitInsn *writer* Opcodes/ATHROW)]]
    (return nil)))

(defn ^:private compile-jvm-getstatic [compile ?values special-args]
  (|do [:let [;; (&/$Nil) ?values
              (&/$Cons ?class (&/$Cons ?field (&/$Cons ?output-type (&/$Nil)))) special-args]
        ^MethodVisitor *writer* &/get-writer
        =output-type (&host/->java-sig ?output-type)
        :let [_ (doto *writer*
                  (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name (&host-type/as-obj ?class)) ?field =output-type)
                  (prepare-return! ?output-type))]]
    (return nil)))

(defn ^:private compile-jvm-getfield [compile ?values special-args]
  (|do [:let [(&/$Cons ?object (&/$Nil)) ?values
              (&/$Cons ?class (&/$Cons ?field (&/$Cons ?output-type (&/$Nil)))) special-args]
        :let [class* (&host-generics/->bytecode-class-name (&host-type/as-obj ?class))]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?object)
        =output-type (&host/->java-sig ?output-type)
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/CHECKCAST class*)
                  (.visitFieldInsn Opcodes/GETFIELD class* ?field =output-type)
                  (prepare-return! ?output-type))]]
    (return nil)))

(defn ^:private compile-jvm-putstatic [compile ?values special-args]
  (|do [:let [(&/$Cons ?value (&/$Nil)) ?values
              (&/$Cons ?class (&/$Cons ?field (&/$Cons input-gclass (&/$Nil)))) special-args]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?value)
        :let [=input-sig (&host-type/gclass->sig input-gclass)
              _ (doto *writer*
                  (prepare-arg! (&host-generics/gclass->class-name input-gclass))
                  (.visitFieldInsn Opcodes/PUTSTATIC (&host-generics/->bytecode-class-name (&host-type/as-obj ?class)) ?field =input-sig)
                  (.visitInsn Opcodes/ACONST_NULL))]]
    (return nil)))

(defn ^:private compile-jvm-putfield [compile ?values special-args]
  (|do [:let [(&/$Cons ?object (&/$Cons ?value (&/$Nil))) ?values
              (&/$Cons ?class (&/$Cons ?field (&/$Cons input-gclass (&/$Cons ?input-type (&/$Nil))))) special-args]
        :let [class* (&host-generics/->bytecode-class-name (&host-type/as-obj ?class))]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?object)
        :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST class*)]
        _ (compile ?value)
        =input-sig (&host/->java-sig ?input-type)
        :let [_ (doto *writer*
                  (prepare-arg! (&host-generics/gclass->class-name input-gclass))
                  (.visitFieldInsn Opcodes/PUTFIELD class* ?field =input-sig)
                  (.visitInsn Opcodes/ACONST_NULL))]]
    (return nil)))

(defn ^:private compile-jvm-invokestatic [compile ?values special-args]
  (|do [:let [?args ?values
              (&/$Cons ?class (&/$Cons ?method (&/$Cons ?classes (&/$Cons ?output-type (&/$Cons ?gret (&/$Nil)))))) special-args]
        ^MethodVisitor *writer* &/get-writer
        :let [method-sig (str "(" (&/fold str "" (&/|map &host-generics/->type-signature ?classes)) ")" (&host-type/principal-class ?gret))]
        _ (&/map2% (fn [class-name arg]
                     (|do [ret (compile arg)
                           :let [_ (prepare-arg! *writer* class-name)]]
                       (return ret)))
                   ?classes ?args)
        :let [_ (doto *writer*
                  (.visitMethodInsn Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name (&host-type/as-obj ?class)) ?method method-sig)
                  (prepare-return! ?output-type))]]
    (return nil)))

(do-template [<name> <op>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?object ?args) ?values
                (&/$Cons ?class (&/$Cons ?method (&/$Cons ?classes (&/$Cons ?output-type (&/$Cons ?gret (&/$Nil)))))) special-args]
          :let [?class* (&host-generics/->bytecode-class-name (&host-type/as-obj ?class))]
          ^MethodVisitor *writer* &/get-writer
          :let [method-sig (str "(" (&/fold str "" (&/|map &host-generics/->type-signature ?classes)) ")" (&host-type/principal-class ?gret))]
          _ (compile ?object)
          :let [_ (when (not= "<init>" ?method)
                    (.visitTypeInsn *writer* Opcodes/CHECKCAST ?class*))]
          _ (&/map2% (fn [class-name arg]
                       (|do [ret (compile arg)
                             :let [_ (prepare-arg! *writer* class-name)]]
                         (return ret)))
                     ?classes ?args)
          :let [_ (doto *writer*
                    (.visitMethodInsn <op> ?class* ?method method-sig)
                    (prepare-return! ?output-type))]]
      (return nil)))

  ^:private compile-jvm-invokevirtual   Opcodes/INVOKEVIRTUAL
  ^:private compile-jvm-invokeinterface Opcodes/INVOKEINTERFACE
  ^:private compile-jvm-invokespecial   Opcodes/INVOKESPECIAL
  )

(defn ^:private compile-jvm-new [compile ?values special-args]
  (|do [:let [?args ?values
              (&/$Cons ?class (&/$Cons ?classes (&/$Nil))) special-args]
        ^MethodVisitor *writer* &/get-writer
        :let [init-sig (str "(" (&/fold str "" (&/|map &host-generics/->type-signature ?classes)) ")V")
              class* (&host-generics/->bytecode-class-name ?class)
              _ (doto *writer*
                  (.visitTypeInsn Opcodes/NEW class*)
                  (.visitInsn Opcodes/DUP))]
        _ (&/map% (fn [class-name+arg]
                    (|do [:let [[class-name arg] class-name+arg]
                          ret (compile arg)
                          :let [_ (prepare-arg! *writer* class-name)]]
                      (return ret)))
                  (&/zip2 ?classes ?args))
        :let [_ (doto *writer*
                  (.visitMethodInsn Opcodes/INVOKESPECIAL class* "<init>" init-sig))]]
    (return nil)))

(defn ^:private compile-jvm-load-class [compile ?values special-args]
  (|do [:let [(&/$Cons _class-name (&/$Cons ?output-type (&/$Nil))) special-args]
        ^MethodVisitor *writer* &/get-writer
        :let [_ (doto *writer*
                  (.visitLdcInsn _class-name)
                  (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Class" "forName" "(Ljava/lang/String;)Ljava/lang/Class;")
                  (prepare-return! ?output-type))]]
    (return nil)))

(defn ^:private compile-jvm-instanceof [compile ?values special-args]
  (|do [:let [(&/$Cons object (&/$Nil)) ?values
              (&/$Cons class (&/$Nil)) special-args]
        :let [class* (&host-generics/->bytecode-class-name class)]
        ^MethodVisitor *writer* &/get-writer
        _ (compile object)
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/INSTANCEOF class*)
                  (&&/wrap-boolean))]]
    (return nil)))

(defn compile-proc [compile proc-name ?values special-args]
  (case proc-name
    "synchronized"    (compile-jvm-synchronized compile ?values special-args)
    "load-class"      (compile-jvm-load-class compile ?values special-args)
    "instanceof"      (compile-jvm-instanceof compile ?values special-args)
    "new"             (compile-jvm-new compile ?values special-args)
    "invokestatic"    (compile-jvm-invokestatic compile ?values special-args)
    "invokeinterface" (compile-jvm-invokeinterface compile ?values special-args)
    "invokevirtual"   (compile-jvm-invokevirtual compile ?values special-args)
    "invokespecial"   (compile-jvm-invokespecial compile ?values special-args)
    "getstatic"       (compile-jvm-getstatic compile ?values special-args)
    "getfield"        (compile-jvm-getfield compile ?values special-args)
    "putstatic"       (compile-jvm-putstatic compile ?values special-args)
    "putfield"        (compile-jvm-putfield compile ?values special-args)
    "throw"           (compile-jvm-throw compile ?values special-args)
    "null?"           (compile-jvm-null? compile ?values special-args)
    "null"            (compile-jvm-null compile ?values special-args)
    "anewarray"       (compile-jvm-anewarray compile ?values special-args)
    "aaload"          (compile-jvm-aaload compile ?values special-args)
    "aastore"         (compile-jvm-aastore compile ?values special-args)
    "arraylength"     (compile-jvm-arraylength compile ?values special-args)
    "znewarray"       (compile-jvm-znewarray compile ?values special-args)
    "bnewarray"       (compile-jvm-bnewarray compile ?values special-args)
    "snewarray"       (compile-jvm-snewarray compile ?values special-args)
    "inewarray"       (compile-jvm-inewarray compile ?values special-args)
    "lnewarray"       (compile-jvm-lnewarray compile ?values special-args)
    "fnewarray"       (compile-jvm-fnewarray compile ?values special-args)
    "dnewarray"       (compile-jvm-dnewarray compile ?values special-args)
    "cnewarray"       (compile-jvm-cnewarray compile ?values special-args)
    "iadd"            (compile-jvm-iadd compile ?values special-args)
    "isub"            (compile-jvm-isub compile ?values special-args)
    "imul"            (compile-jvm-imul compile ?values special-args)
    "idiv"            (compile-jvm-idiv compile ?values special-args)
    "irem"            (compile-jvm-irem compile ?values special-args)
    "ieq"             (compile-jvm-ieq compile ?values special-args)
    "ilt"             (compile-jvm-ilt compile ?values special-args)
    "igt"             (compile-jvm-igt compile ?values special-args)
    "ceq"             (compile-jvm-ceq compile ?values special-args)
    "clt"             (compile-jvm-clt compile ?values special-args)
    "cgt"             (compile-jvm-cgt compile ?values special-args)
    "ladd"            (compile-jvm-ladd compile ?values special-args)
    "lsub"            (compile-jvm-lsub compile ?values special-args)
    "lmul"            (compile-jvm-lmul compile ?values special-args)
    "ldiv"            (compile-jvm-ldiv compile ?values special-args)
    "lrem"            (compile-jvm-lrem compile ?values special-args)
    "leq"             (compile-jvm-leq compile ?values special-args)
    "llt"             (compile-jvm-llt compile ?values special-args)
    "lgt"             (compile-jvm-lgt compile ?values special-args)
    "fadd"            (compile-jvm-fadd compile ?values special-args)
    "fsub"            (compile-jvm-fsub compile ?values special-args)
    "fmul"            (compile-jvm-fmul compile ?values special-args)
    "fdiv"            (compile-jvm-fdiv compile ?values special-args)
    "frem"            (compile-jvm-frem compile ?values special-args)
    "feq"             (compile-jvm-feq compile ?values special-args)
    "flt"             (compile-jvm-flt compile ?values special-args)
    "fgt"             (compile-jvm-fgt compile ?values special-args)
    "dadd"            (compile-jvm-dadd compile ?values special-args)
    "dsub"            (compile-jvm-dsub compile ?values special-args)
    "dmul"            (compile-jvm-dmul compile ?values special-args)
    "ddiv"            (compile-jvm-ddiv compile ?values special-args)
    "drem"            (compile-jvm-drem compile ?values special-args)
    "deq"             (compile-jvm-deq compile ?values special-args)
    "dlt"             (compile-jvm-dlt compile ?values special-args)
    "dgt"             (compile-jvm-dgt compile ?values special-args)
    "iand"            (compile-jvm-iand compile ?values special-args)
    "ior"             (compile-jvm-ior compile ?values special-args)
    "ixor"            (compile-jvm-ixor compile ?values special-args)
    "ishl"            (compile-jvm-ishl compile ?values special-args)
    "ishr"            (compile-jvm-ishr compile ?values special-args)
    "iushr"           (compile-jvm-iushr compile ?values special-args)
    "land"            (compile-jvm-land compile ?values special-args)
    "lor"             (compile-jvm-lor compile ?values special-args)
    "lxor"            (compile-jvm-lxor compile ?values special-args)
    "lshl"            (compile-jvm-lshl compile ?values special-args)
    "lshr"            (compile-jvm-lshr compile ?values special-args)
    "lushr"           (compile-jvm-lushr compile ?values special-args)
    "d2f"             (compile-jvm-d2f compile ?values special-args)
    "d2i"             (compile-jvm-d2i compile ?values special-args)
    "d2l"             (compile-jvm-d2l compile ?values special-args)
    "f2d"             (compile-jvm-f2d compile ?values special-args)
    "f2i"             (compile-jvm-f2i compile ?values special-args)
    "f2l"             (compile-jvm-f2l compile ?values special-args)
    "i2b"             (compile-jvm-i2b compile ?values special-args)
    "i2c"             (compile-jvm-i2c compile ?values special-args)
    "i2d"             (compile-jvm-i2d compile ?values special-args)
    "i2f"             (compile-jvm-i2f compile ?values special-args)
    "i2l"             (compile-jvm-i2l compile ?values special-args)
    "i2s"             (compile-jvm-i2s compile ?values special-args)
    "l2d"             (compile-jvm-l2d compile ?values special-args)
    "l2f"             (compile-jvm-l2f compile ?values special-args)
    "l2i"             (compile-jvm-l2i compile ?values special-args)
    "l2s"             (compile-jvm-l2s compile ?values special-args)
    "l2b"             (compile-jvm-l2b compile ?values special-args)
    "c2b"             (compile-jvm-c2b compile ?values special-args)
    "c2s"             (compile-jvm-c2s compile ?values special-args)
    "c2i"             (compile-jvm-c2i compile ?values special-args)
    "c2l"             (compile-jvm-c2l compile ?values special-args)
    "s2l"             (compile-jvm-s2l compile ?values special-args)
    "b2l"             (compile-jvm-b2l compile ?values special-args)
    ;; else
    (&/fail-with-loc (str "[Compiler Error] Unknown host procedure: " ["jvm" proc-name]))))
