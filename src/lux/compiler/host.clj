;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.compiler.host
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |let |case]]
                 [type :as &type]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser]
                 [optimizer :as &o]
                 [host :as &host])
            [lux.type.host :as &host-type]
            [lux.host.generics :as &host-generics]
            [lux.analyser.base :as &a]
            [lux.compiler.base :as &&])
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor
                              AnnotationVisitor)))

;; [Utils]
(def init-method "<init>")

(let [class+method+sig {"boolean" [(&host-generics/->bytecode-class-name "java.lang.Boolean")   "booleanValue" "()Z"]
                        "byte"    [(&host-generics/->bytecode-class-name "java.lang.Byte")      "byteValue"    "()B"]
                        "short"   [(&host-generics/->bytecode-class-name "java.lang.Short")     "shortValue"   "()S"]
                        "int"     [(&host-generics/->bytecode-class-name "java.lang.Integer")   "intValue"     "()I"]
                        "long"    [(&host-generics/->bytecode-class-name "java.lang.Long")      "longValue"    "()J"]
                        "float"   [(&host-generics/->bytecode-class-name "java.lang.Float")     "floatValue"   "()F"]
                        "double"  [(&host-generics/->bytecode-class-name "java.lang.Double")    "doubleValue"  "()D"]
                        "char"    [(&host-generics/->bytecode-class-name "java.lang.Character") "charValue"    "()C"]}]
  (defn ^:private prepare-arg! [^MethodVisitor *writer* class-name]
    (if-let [[class method sig] (get class+method+sig class-name)]
      (doto *writer*
        (.visitTypeInsn Opcodes/CHECKCAST class)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL class method sig))
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
      (&/$UnitT)
      (.visitLdcInsn *writer* &/unit-tag)

      (&/$HostT "boolean" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name boolean-class) "valueOf" (str "(Z)" (&host-generics/->type-signature boolean-class)))
      
      (&/$HostT "byte" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name byte-class) "valueOf" (str "(B)" (&host-generics/->type-signature byte-class)))

      (&/$HostT "short" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name short-class) "valueOf" (str "(S)" (&host-generics/->type-signature short-class)))

      (&/$HostT "int" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name int-class) "valueOf" (str "(I)" (&host-generics/->type-signature int-class)))

      (&/$HostT "long" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name long-class) "valueOf" (str "(J)" (&host-generics/->type-signature long-class)))

      (&/$HostT "float" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name float-class) "valueOf" (str "(F)" (&host-generics/->type-signature float-class)))

      (&/$HostT "double" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name double-class) "valueOf" (str "(D)" (&host-generics/->type-signature double-class)))

      (&/$HostT "char" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name char-class) "valueOf" (str "(C)" (&host-generics/->type-signature char-class)))
      
      (&/$HostT _ _)
      nil

      (&/$NamedT ?name ?type)
      (prepare-return! *writer* ?type)

      (&/$ExT _)
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
                (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name "java.lang.Boolean"))
                &&/unwrap-boolean)
    "byte" (doto writer
             (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name "java.lang.Byte"))
             &&/unwrap-byte)
    "short" (doto writer
              (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name "java.lang.Short"))
              &&/unwrap-short)
    "int" (doto writer
            (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name "java.lang.Integer"))
            &&/unwrap-int)
    "long" (doto writer
             (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name "java.lang.Long"))
             &&/unwrap-long)
    "float" (doto writer
              (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name "java.lang.Float"))
              &&/unwrap-float)
    "double" (doto writer
               (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name "java.lang.Double"))
               &&/unwrap-double)
    "char" (doto writer
             (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name "java.lang.Character"))
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

(def compile-Function-class
  (|do [_ (return nil)
        :let [super-class "java/lang/Object"
              =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                       (.visit &host/bytecode-version (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER
                                                         Opcodes/ACC_ABSTRACT
                                                         ;; Opcodes/ACC_INTERFACE
                                                         )
                               &&/function-class nil super-class (into-array String []))
                       (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL) &&/partials-field "I" nil nil)
                           (doto (.visitEnd))))
              =init-method (doto (.visitMethod =class Opcodes/ACC_PUBLIC init-method "(I)V" nil nil)
                             (.visitCode)
                             (.visitVarInsn Opcodes/ALOAD 0)
                             (.visitMethodInsn Opcodes/INVOKESPECIAL super-class init-method "()V")
                             (.visitVarInsn Opcodes/ALOAD 0)
                             (.visitVarInsn Opcodes/ILOAD 1)
                             (.visitFieldInsn Opcodes/PUTFIELD &&/function-class &&/partials-field "I")
                             (.visitInsn Opcodes/RETURN)
                             (.visitMaxs 0 0)
                             (.visitEnd))
              _ (dotimes [arity* &&/num-apply-variants]
                  (let [arity (inc arity*)]
                    (if (= 1 arity)
                      (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT) &&/apply-method (&&/apply-signature arity) nil nil)
                        (.visitEnd))
                      (doto (.visitMethod =class Opcodes/ACC_PUBLIC &&/apply-method (&&/apply-signature arity) nil nil)
                        (.visitCode)
                        (-> (.visitVarInsn Opcodes/ALOAD idx)
                            (->> (dotimes [idx arity])))
                        (.visitMethodInsn Opcodes/INVOKEVIRTUAL &&/function-class &&/apply-method (&&/apply-signature (dec arity)))
                        (.visitTypeInsn Opcodes/CHECKCAST &&/function-class)
                        (.visitVarInsn Opcodes/ALOAD arity)
                        (.visitMethodInsn Opcodes/INVOKEVIRTUAL &&/function-class &&/apply-method (&&/apply-signature 1))
                        (.visitInsn Opcodes/ARETURN)
                        (.visitMaxs 0 0)
                        (.visitEnd)))))]]
    (&&/save-class! (second (string/split &&/function-class #"/"))
                    (.toByteArray (doto =class .visitEnd)))))

(defn ^:private compile-LuxRT-adt-methods [^ClassWriter =class]
  (|let [_ (let [$begin (new Label)
                 $not-rec (new Label)]
             (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "product_getLeft" "([Ljava/lang/Object;I)Ljava/lang/Object;" nil nil)
               (.visitCode)
               (.visitLabel $begin)
               (.visitVarInsn Opcodes/ALOAD 0) ;; tuple
               (.visitInsn Opcodes/ARRAYLENGTH) ;; tuple-size
               (.visitVarInsn Opcodes/ILOAD 1) ;; tuple-size, index
               (.visitLdcInsn (int 1)) ;; tuple-size, index, offset-last-elem
               (.visitInsn Opcodes/IADD) ;; tuple-size, index-last-elem
               (.visitInsn Opcodes/DUP2) ;; tuple-size, index-last-elem, tuple-size, index-last-elem
               (.visitJumpInsn Opcodes/IF_ICMPGT $not-rec) ;; tuple-size, index-last-elem
               (.visitInsn Opcodes/SWAP) ;; index-last-elem, tuple-size
               (.visitInsn Opcodes/ISUB) ;; sub-index
               (.visitVarInsn Opcodes/ALOAD 0) ;; sub-index, tuple
               (.visitInsn Opcodes/DUP) ;; sub-index, tuple, tuple
               (.visitInsn Opcodes/ARRAYLENGTH) ;; sub-index, tuple, tuple-size
               (.visitLdcInsn (int 1)) ;; sub-index, tuple, tuple-size, offset-last-elem
               (.visitInsn Opcodes/ISUB) ;; sub-index, tuple, index-last-elem
               (.visitInsn Opcodes/AALOAD) ;; sub-index, sub-tuple
               (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
               (.visitVarInsn Opcodes/ASTORE 0) ;; sub-index
               (.visitVarInsn Opcodes/ISTORE 1) ;;
               (.visitJumpInsn Opcodes/GOTO $begin)
               (.visitLabel $not-rec) ;; tuple-size, index-last-elem
               (.visitInsn Opcodes/POP2) ;;
               (.visitVarInsn Opcodes/ALOAD 0) ;; tuple
               (.visitVarInsn Opcodes/ILOAD 1) ;; tuple, index
               (.visitInsn Opcodes/AALOAD) ;; elem
               (.visitInsn Opcodes/ARETURN)
               (.visitMaxs 0 0)
               (.visitEnd)))
         _ (let [$begin (new Label)
                 $is-last (new Label)
                 $must-copy (new Label)]
             (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "product_getRight" "([Ljava/lang/Object;I)Ljava/lang/Object;" nil nil)
               (.visitCode)
               (.visitLabel $begin)
               (.visitVarInsn Opcodes/ALOAD 0) ;; tuple
               (.visitInsn Opcodes/ARRAYLENGTH) ;; tuple-size
               (.visitVarInsn Opcodes/ILOAD 1) ;; tuple-size, index
               (.visitLdcInsn (int 1)) ;; tuple-size, index, offset-last-elem
               (.visitInsn Opcodes/IADD) ;; tuple-size, index-last-elem
               (.visitInsn Opcodes/DUP2) ;; tuple-size, index-last-elem, tuple-size, index-last-elem
               (.visitJumpInsn Opcodes/IF_ICMPEQ $is-last) ;; tuple-size, index-last-elem
               (.visitJumpInsn Opcodes/IF_ICMPGT $must-copy) ;;
               ;; Must recurse
               (.visitVarInsn Opcodes/ALOAD 0) ;; tuple
               (.visitInsn Opcodes/DUP) ;; tuple, tuple
               (.visitInsn Opcodes/ARRAYLENGTH) ;; tuple, tuple-size
               (.visitLdcInsn (int 1)) ;; tuple, tuple-size, offset-last-elem
               (.visitInsn Opcodes/ISUB) ;; tuple, offset-tuple-last-elem
               (.visitInsn Opcodes/AALOAD) ;; tuple-tail
               (.visitVarInsn Opcodes/ILOAD 1) ;; tuple-tail, index
               (.visitVarInsn Opcodes/ALOAD 0) ;; tuple-tail, index, tuple
               (.visitInsn Opcodes/ARRAYLENGTH) ;; tuple-tail, index, tuple-size
               (.visitLdcInsn (int 1)) ;; tuple-tail, index, tuple-size, 1
               (.visitInsn Opcodes/ISUB) ;; tuple-tail, index, tuple-size*
               (.visitInsn Opcodes/ISUB) ;; tuple-tail, index*
               (.visitVarInsn Opcodes/ISTORE 1) ;; tuple-tail
               (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;") ;; tuple-tail
               (.visitVarInsn Opcodes/ASTORE 0) ;;
               (.visitJumpInsn Opcodes/GOTO $begin)
               (.visitLabel $must-copy)
               (.visitVarInsn Opcodes/ALOAD 0)
               (.visitVarInsn Opcodes/ILOAD 1)
               (.visitVarInsn Opcodes/ALOAD 0)
               (.visitInsn Opcodes/ARRAYLENGTH)
               (.visitMethodInsn Opcodes/INVOKESTATIC "java/util/Arrays" "copyOfRange" "([Ljava/lang/Object;II)[Ljava/lang/Object;")
               (.visitInsn Opcodes/ARETURN)
               (.visitLabel $is-last) ;; tuple-size, index-last-elem
               (.visitInsn Opcodes/POP2) ;;
               (.visitVarInsn Opcodes/ALOAD 0) ;; tuple
               (.visitVarInsn Opcodes/ILOAD 1) ;; tuple, index
               (.visitInsn Opcodes/AALOAD) ;; elem
               (.visitInsn Opcodes/ARETURN)
               (.visitMaxs 0 0)
               (.visitEnd)))
         _ (let [$begin (new Label)
                 $just-return (new Label)
                 $then (new Label)
                 $further (new Label)
                 $not-right (new Label)]
             (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "sum_get" "([Ljava/lang/Object;ILjava/lang/Object;)Ljava/lang/Object;" nil nil)
               (.visitCode)
               (.visitLabel $begin)
               (.visitVarInsn Opcodes/ILOAD 1) ;; tag
               (.visitVarInsn Opcodes/ALOAD 0) ;; tag, sum
               (.visitLdcInsn (int 0)) ;; tag, sum, sum-tag-idx
               (.visitInsn Opcodes/AALOAD) ;; tag, sum-tag'
               &&/unwrap-int ;; tag, sum-tag
               (.visitInsn Opcodes/DUP2) ;; tag, sum-tag, tag, sum-tag
               (.visitJumpInsn Opcodes/IF_ICMPEQ $then) ;; tag, sum-tag
               (.visitInsn Opcodes/DUP2) ;; tag, sum-tag, tag, sum-tag
               (.visitJumpInsn Opcodes/IF_ICMPGT $further) ;; tag, sum-tag
               (.visitInsn Opcodes/POP2)
               (.visitInsn Opcodes/ACONST_NULL)
               (.visitInsn Opcodes/ARETURN)
               (.visitLabel $then) ;; tag, sum-tag
               (.visitVarInsn Opcodes/ALOAD 2) ;; tag, sum-tag, wants-last?
               (.visitVarInsn Opcodes/ALOAD 0)
               (.visitLdcInsn (int 1))
               (.visitInsn Opcodes/AALOAD) ;; tag, sum-tag, wants-last?, is-last?
               (.visitJumpInsn Opcodes/IF_ACMPEQ $just-return)
               (.visitJumpInsn Opcodes/GOTO $further)
               (.visitLabel $just-return)
               (.visitInsn Opcodes/POP2)
               (.visitVarInsn Opcodes/ALOAD 0)
               (.visitLdcInsn (int 2))
               (.visitInsn Opcodes/AALOAD)
               (.visitInsn Opcodes/ARETURN)
               (.visitLabel $further) ;; tag, sum-tag
               (.visitVarInsn Opcodes/ALOAD 0) ;; tag, sum-tag, sum
               (.visitLdcInsn (int 1)) ;; tag, sum-tag, sum, last-index?
               (.visitInsn Opcodes/AALOAD) ;; tag, sum-tag, last?
               (.visitJumpInsn Opcodes/IFNULL $not-right) ;; tag, sum-tag
               (.visitInsn Opcodes/ISUB) ;; sub-tag
               (.visitVarInsn Opcodes/ALOAD 0) ;; sub-tag, sum
               (.visitLdcInsn (int 2)) ;; sub-tag, sum, sub-sum-idx
               (.visitInsn Opcodes/AALOAD) ;; sub-tag, sub-sum
               (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
               (.visitVarInsn Opcodes/ASTORE 0) ;; sub-tag
               (.visitVarInsn Opcodes/ISTORE 1) ;;
               (.visitJumpInsn Opcodes/GOTO $begin)
               (.visitLabel $not-right) ;; tag, sum-tag
               (.visitInsn Opcodes/POP2)
               (.visitInsn Opcodes/ACONST_NULL)
               (.visitInsn Opcodes/ARETURN)
               (.visitMaxs 0 0)
               (.visitEnd)))
         _ (let [;; $is-null (new Label)
                 ]
             ;; I commented out some parts because a null-check was
             ;; done to ensure variants were never created with null
             ;; values (this would interfere later with
             ;; pattern-matching).
             ;; Since Lux itself doesn't have null values as part of
             ;; the language, the burden of ensuring non-nulls was
             ;; shifted to library code dealing with host-interop, to
             ;; ensure variant-making was as fast as possible.
             ;; The null-checking code was left as comments in case I
             ;; ever change my mind.
             (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "sum_make" "(ILjava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;" nil nil)
               (.visitCode)
               ;; (.visitVarInsn Opcodes/ALOAD 2)
               ;; (.visitJumpInsn Opcodes/IFNULL $is-null)
               (.visitLdcInsn (int 3))
               (.visitTypeInsn Opcodes/ANEWARRAY "java/lang/Object")
               (.visitInsn Opcodes/DUP)
               (.visitLdcInsn (int 0))
               (.visitVarInsn Opcodes/ILOAD 0)
               (&&/wrap-int)
               (.visitInsn Opcodes/AASTORE)
               (.visitInsn Opcodes/DUP)
               (.visitLdcInsn (int 1))
               (.visitVarInsn Opcodes/ALOAD 1)
               (.visitInsn Opcodes/AASTORE)
               (.visitInsn Opcodes/DUP)
               (.visitLdcInsn (int 2))
               (.visitVarInsn Opcodes/ALOAD 2)
               (.visitInsn Opcodes/AASTORE)
               (.visitInsn Opcodes/ARETURN)
               ;; (.visitLabel $is-null)
               ;; (.visitTypeInsn Opcodes/NEW "java/lang/IllegalStateException")
               ;; (.visitInsn Opcodes/DUP)
               ;; (.visitLdcInsn "Can't create variant for null pointer")
               ;; (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/IllegalStateException" "<init>" "(Ljava/lang/String;)V")
               ;; (.visitInsn Opcodes/ATHROW)
               (.visitMaxs 0 0)
               (.visitEnd)))]
    nil))

(defn ^:private low-4b [^MethodVisitor =method]
  (doto =method
    ;; Assume there is a long at the top of the stack...
    ;; Add mask corresponding to -1 (FFFF...), on the low 32 bits.
    (.visitLdcInsn (int -1))
    (.visitInsn Opcodes/I2L)
    ;; Then do a bitwise and.
    (.visitInsn Opcodes/LAND)
    ))

(defn ^:private high-4b [^MethodVisitor =method]
  (doto =method
    ;; Assume there is a long at the top of the stack...
    (.visitLdcInsn (int 32))
    (.visitInsn Opcodes/LUSHR)
    ))

(defn ^:private swap2 [^MethodVisitor =method]
  (doto =method
    ;; X2, Y2
    (.visitInsn Opcodes/DUP2_X2) ;; Y2, X2, Y2
    (.visitInsn Opcodes/POP2) ;; Y2, X2
    ))

(defn ^:private bit-set-64? [^MethodVisitor =method]
  (doto =method
    ;; L, I
    (.visitLdcInsn (long 1)) ;; L, I, L
    (.visitInsn Opcodes/DUP2_X1) ;; L, L, I, L
    (.visitInsn Opcodes/POP2) ;; L, L, I
    (.visitInsn Opcodes/LSHL) ;; L, L
    (.visitInsn Opcodes/LAND) ;; L
    (.visitLdcInsn (long 0)) ;; L, L
    (.visitInsn Opcodes/LCMP) ;; I
    ))

(defn ^:private compile-LuxRT-frac-methods [^ClassWriter =class]
  (|let [_ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "mul_frac" "(JJ)J" nil nil)
             ;; Based on: http://stackoverflow.com/a/31629280/6823464
             (.visitCode)
             ;; Bottom part
             (.visitVarInsn Opcodes/LLOAD 0) low-4b
             (.visitVarInsn Opcodes/LLOAD 2) low-4b
             (.visitInsn Opcodes/LMUL)
             (.visitLdcInsn (int 32))
             (.visitInsn Opcodes/LUSHR)
             ;; Middle part
             (.visitVarInsn Opcodes/LLOAD 0) high-4b
             (.visitVarInsn Opcodes/LLOAD 2) low-4b
             (.visitInsn Opcodes/LMUL)
             (.visitVarInsn Opcodes/LLOAD 0) low-4b
             (.visitVarInsn Opcodes/LLOAD 2) high-4b
             (.visitInsn Opcodes/LMUL)
             (.visitInsn Opcodes/LADD)
             ;; Join middle and bottom
             (.visitInsn Opcodes/LADD)
             (.visitLdcInsn (int 32))
             (.visitInsn Opcodes/LUSHR)
             ;; Top part
             (.visitVarInsn Opcodes/LLOAD 0) high-4b
             (.visitVarInsn Opcodes/LLOAD 2) high-4b
             (.visitInsn Opcodes/LMUL)
             ;; Join top with rest
             (.visitInsn Opcodes/LADD)
             ;; Return
             (.visitInsn Opcodes/LRETURN)
             (.visitMaxs 0 0)
             (.visitEnd))
         _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "div_frac" "(JJ)J" nil nil)
             (.visitCode)
             ;; Based on: http://stackoverflow.com/a/8510587/6823464
             (.visitVarInsn Opcodes/LLOAD 0)
             (.visitVarInsn Opcodes/LLOAD 2) high-4b
             (.visitInsn Opcodes/LDIV)
             (.visitLdcInsn (int 32))
             (.visitInsn Opcodes/LSHL)
             (.visitInsn Opcodes/LRETURN)
             (.visitMaxs 0 0)
             (.visitEnd))
         _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "frac-to-real" "(J)D" nil nil)
             (.visitCode)
             ;; Translate high bytes
             (.visitVarInsn Opcodes/LLOAD 0) high-4b
             (.visitInsn Opcodes/L2D)
             (.visitLdcInsn (double (Math/pow 2 32)))
             (.visitInsn Opcodes/DDIV)
             ;; Translate low bytes
             (.visitVarInsn Opcodes/LLOAD 0) low-4b
             (.visitInsn Opcodes/L2D)
             (.visitLdcInsn (double (Math/pow 2 32)))
             (.visitInsn Opcodes/DDIV)
             (.visitLdcInsn (double (Math/pow 2 32)))
             (.visitInsn Opcodes/DDIV)
             ;; Combine and return
             (.visitInsn Opcodes/DADD)
             (.visitInsn Opcodes/DRETURN)
             (.visitMaxs 0 0)
             (.visitEnd))
         _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "real-to-frac" "(D)J" nil nil)
             (.visitCode)
             ;; Drop any excess
             (.visitVarInsn Opcodes/DLOAD 0)
             (.visitLdcInsn (double 1.0))
             (.visitInsn Opcodes/DREM)
             ;; Shift upper half, but retain remaining decimals
             (.visitLdcInsn (double (Math/pow 2 32)))
             (.visitInsn Opcodes/DMUL)
             ;; Make a copy, so the lower half can be extracted
             (.visitInsn Opcodes/DUP2)
             ;; Get that lower half
             (.visitLdcInsn (double 1.0))
             (.visitInsn Opcodes/DREM)
             (.visitLdcInsn (double (Math/pow 2 32)))
             (.visitInsn Opcodes/DMUL)
             ;; Turn it into a frac
             (.visitInsn Opcodes/D2L)
             ;; Turn the upper half into frac too
             swap2
             (.visitInsn Opcodes/D2L)
             ;; Combine both pieces
             (.visitInsn Opcodes/LADD)
             ;; FINISH
             (.visitInsn Opcodes/LRETURN)
             (.visitMaxs 0 0)
             (.visitEnd))
         _ (let [$start (new Label)
                 $body (new Label)
                 $end (new Label)
                 $zero (new Label)]
             (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "count_bin_start_0" "(J)I" nil nil)
               (.visitCode)
               ;; Initialize counter
               (.visitLdcInsn (int 0)) ; I
               (.visitVarInsn Opcodes/ISTORE 2) ; 
               ;; Initialize index var
               (.visitLdcInsn (int 63)) ; I
               ;; Begin loop
               (.visitLabel $start) ; I
               ;; Make sure we're still on the valid index range
               (.visitInsn Opcodes/DUP) ; I, I
               (.visitLdcInsn (int -1)) ; I, I, I
               (.visitJumpInsn Opcodes/IF_ICMPGT $body) ; I
               ;; If not, just return what we've got.
               (.visitInsn Opcodes/POP) ;
               (.visitVarInsn Opcodes/ILOAD 2) ; I
               (.visitJumpInsn Opcodes/GOTO $end)
               ;; If so, run the body
               (.visitLabel $body) ;; I
               (.visitInsn Opcodes/DUP) ;; I, I
               (.visitVarInsn Opcodes/LLOAD 0) ;; I, I, L
               (.visitInsn Opcodes/DUP2_X1) ;; I, L, I, L
               (.visitInsn Opcodes/POP2) ;; I, L, I
               bit-set-64? ;; I, I
               (.visitJumpInsn Opcodes/IFEQ $zero) ;; I
               ;; No more zeroes from now on...
               (.visitInsn Opcodes/POP) ;;
               (.visitVarInsn Opcodes/ILOAD 2) ;; I
               (.visitJumpInsn Opcodes/GOTO $end)
               ;; Found another zero...
               (.visitLabel $zero) ;; I
               ;; Increase counter
               (.visitVarInsn Opcodes/ILOAD 2) ;; I, I
               (.visitLdcInsn (int 1)) ;; I, I, I
               (.visitInsn Opcodes/IADD) ;; I, I
               (.visitVarInsn Opcodes/ISTORE 2) ;; I
               ;; Increase index, then iterate again...
               (.visitLdcInsn (int 1)) ;; I, I
               (.visitInsn Opcodes/ISUB) ;; I
               (.visitJumpInsn Opcodes/GOTO $start)
               ;; Finally, return
               (.visitLabel $end) ; I
               (.visitInsn Opcodes/IRETURN)
               (.visitMaxs 0 0)
               (.visitEnd)))
         _ (let [$start (new Label)
                 $can-append (new Label)
                 $end (new Label)]
             (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "make_text_start_0" "(J)Ljava/lang/String;" nil nil)
               (.visitCode)
               ;; Initialize accum
               (.visitLdcInsn "") ;; S
               (.visitVarInsn Opcodes/ASTORE 2) ;;
               ;; Initialize comparator
               (.visitLdcInsn (long 10)) ;; L
               ;; Testing/accum loop
               (.visitLabel $start) ;; L
               (.visitInsn Opcodes/DUP2) ;; L, L
               (.visitVarInsn Opcodes/LLOAD 0) ;; L, L, L
               (.visitInsn Opcodes/LCMP) ;; L, I
               (.visitJumpInsn Opcodes/IFLT $can-append) ;; L
               ;; No more testing.
               ;; Throw away the comparator and return accum.
               (.visitInsn Opcodes/POP2) ;;
               (.visitVarInsn Opcodes/ALOAD 2) ;; S
               (.visitJumpInsn Opcodes/GOTO $end)
               ;; Can keep accumulating
               (.visitLabel $can-append) ;; L
               ;; Add one more 0 to accum
               (.visitVarInsn Opcodes/ALOAD 2) ;; L, S
               (.visitLdcInsn "0") ;; L, S, S
               (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "concat" "(Ljava/lang/String;)Ljava/lang/String;") ;; L, S
               (.visitVarInsn Opcodes/ASTORE 2) ;; L
               ;; Update comparator and re-iterate
               (.visitLdcInsn (long 10)) ;; L, L
               (.visitInsn Opcodes/LMUL) ;; L
               (.visitJumpInsn Opcodes/GOTO $start)
               (.visitLabel $end) ;; S
               (.visitInsn Opcodes/ARETURN)
               (.visitMaxs 0 0)
               (.visitEnd)))
         _ (let [$is-zero (new Label)
                 $end (new Label)]
             (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "encode_frac" "(J)Ljava/lang/String;" nil nil)
               (.visitCode)
               (.visitVarInsn Opcodes/LLOAD 0)
               (.visitLdcInsn (long 0))
               (.visitInsn Opcodes/LCMP)
               (.visitJumpInsn Opcodes/IFEQ $is-zero)
               ;; IF =/= 0
               ;; Generate leading 0s
               (.visitLdcInsn (long 1))
               (.visitVarInsn Opcodes/LLOAD 0)
               (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "count_bin_start_0" "(J)I")
               (.visitInsn Opcodes/LSHL)
               (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_text_start_0" "(J)Ljava/lang/String;")
               ;; Convert to number text
               (.visitVarInsn Opcodes/LLOAD 0)
               (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Long" "toUnsignedString" "(J)Ljava/lang/String;")
               ;; Remove unnecessary trailing zeroes
               (.visitLdcInsn "0*$")
               (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "split" "(Ljava/lang/String;)[Ljava/lang/String;")
               (.visitLdcInsn (int 0))
               (.visitInsn Opcodes/AALOAD)
               ;; Join leading 0s with number text
               (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "concat" "(Ljava/lang/String;)Ljava/lang/String;")
               ;; FINISH
               (.visitJumpInsn Opcodes/GOTO $end)
               ;; IF == 0
               (.visitLabel $is-zero)
               (.visitLdcInsn ".0")
               (.visitLabel $end)
               (.visitInsn Opcodes/ARETURN)
               (.visitMaxs 0 0)
               (.visitEnd)))
         _ (let [$end (new Label)
                 ;; $then (new Label)
                 $else (new Label)
                 $from (new Label)
                 $to (new Label)
                 $handler (new Label)]
             (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "decode_frac" "(Ljava/lang/String;)Ljava/lang/Object;" nil nil)
               (.visitCode)
               ;; Check prefix
               (.visitVarInsn Opcodes/ALOAD 0)
               (.visitLdcInsn ".")
               (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "startsWith" "(Ljava/lang/String;)Z")
               (.visitJumpInsn Opcodes/IFEQ $else)
               ;; Remove prefix
               (.visitVarInsn Opcodes/ALOAD 0)
               (.visitLdcInsn (int 1))
               (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "substring" "(I)Ljava/lang/String;")
               (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "clean_separators" "(Ljava/lang/String;)Ljava/lang/String;")
               (.visitInsn Opcodes/DUP)
               (.visitTryCatchBlock $from $to $handler "java/lang/Exception")
               (.visitLabel $from)
               (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "read_frac_text" "(Ljava/lang/String;)J")
               (.visitLabel $to)
               (.visitInsn Opcodes/DUP2)
               (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "count_bin_start_0" "(J)I")
               (.visitInsn Opcodes/LSHL)
               (.visitInsn Opcodes/DUP2_X1)
               (.visitInsn Opcodes/POP2)
               (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "count_leading_zeroes" "(Ljava/lang/String;)J")
               (.visitInsn Opcodes/L2D)
               (.visitLdcInsn (double 10.0))
               swap2
               (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Math" "pow" "(DD)D")
               (.visitInsn Opcodes/D2L)
               (.visitInsn Opcodes/LDIV)
               ;; (.visitJumpInsn Opcodes/GOTO $then)
               ;; (.visitLabel $then)
               (&&/wrap-long)
               (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_some" "(Ljava/lang/Object;)Ljava/lang/Object;")
               (.visitFrame Opcodes/F_NEW 1 (to-array ["java/lang/String"]) 1 (to-array ["java/lang/Object"]))
               (.visitJumpInsn Opcodes/GOTO $end)
               (.visitLabel $handler)
               (.visitFrame Opcodes/F_NEW 1 (to-array ["java/lang/String"]) 1 (to-array ["java/lang/Exception"]))
               (.visitInsn Opcodes/POP)
               (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_none" "()Ljava/lang/Object;")
               (.visitFrame Opcodes/F_NEW 1 (to-array ["java/lang/String"]) 1 (to-array ["java/lang/Object"]))
               (.visitJumpInsn Opcodes/GOTO $end)
               ;; Doesn't start with necessary prefix.
               (.visitLabel $else)
               (.visitFrame Opcodes/F_NEW 1 (to-array ["java/lang/String"]) 0 (to-array []))
               (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_none" "()Ljava/lang/Object;")
               (.visitLabel $end)
               (.visitFrame Opcodes/F_NEW 1 (to-array ["java/lang/String"]) 1 (to-array ["java/lang/Object"]))
               (.visitInsn Opcodes/ARETURN)
               (.visitMaxs 0 0)
               (.visitEnd)))
         _ (let [string-bcn (&host-generics/->bytecode-class-name "java.lang.String")
                 $valid (new Label)
                 $end (new Label)]
             (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "count_leading_zeroes" "(Ljava/lang/String;)J" nil nil)
               (.visitCode)
               (.visitVarInsn Opcodes/ALOAD 0) ;; S
               (.visitLdcInsn "^0*") ;; S, S
               (.visitMethodInsn Opcodes/INVOKEVIRTUAL string-bcn "split" "(Ljava/lang/String;)[Ljava/lang/String;") ;; [S
               (.visitInsn Opcodes/DUP) ;; [S, [S
               (.visitInsn Opcodes/ARRAYLENGTH) ;; [S, I
               (.visitLdcInsn (int 2)) ;; [S, I, I
               (.visitJumpInsn Opcodes/IF_ICMPEQ $valid) ;; [S
               ;; Invalid...
               (.visitInsn Opcodes/POP) ;;
               (.visitLdcInsn (long 0)) ;; J
               (.visitJumpInsn Opcodes/GOTO $end)
               (.visitLabel $valid) ;; [S
               ;; Valid...
               (.visitLdcInsn (int 1)) ;; [S, I
               (.visitInsn Opcodes/AALOAD) ;; S
               (.visitMethodInsn Opcodes/INVOKEVIRTUAL string-bcn "length" "()I") ;; I
               (.visitVarInsn Opcodes/ALOAD 0) ;; I, S
               (.visitMethodInsn Opcodes/INVOKEVIRTUAL string-bcn "length" "()I") ;; I, I
               (.visitInsn Opcodes/SWAP) ;; I, I
               (.visitInsn Opcodes/ISUB) ;; I
               (.visitInsn Opcodes/I2L) ;; J
               (.visitLabel $end) ;; J
               (.visitInsn Opcodes/LRETURN)
               (.visitMaxs 0 0)
               (.visitEnd)))
         _ (let [$only-zeroes (new Label)
                 $end (new Label)]
             (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "read_frac_text" "(Ljava/lang/String;)J" nil nil)
               (.visitCode)
               (.visitVarInsn Opcodes/ALOAD 0)
               (.visitLdcInsn "0*$")
               (.visitMethodInsn Opcodes/INVOKEVIRTUAL
                                 (&host-generics/->bytecode-class-name "java.lang.String")
                                 "split" "(Ljava/lang/String;)[Ljava/lang/String;")
               (.visitInsn Opcodes/DUP)
               (.visitInsn Opcodes/ARRAYLENGTH)
               (.visitJumpInsn Opcodes/IFEQ $only-zeroes)
               (.visitLdcInsn (int 0))
               (.visitInsn Opcodes/AALOAD)
               (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Long" "parseUnsignedLong" "(Ljava/lang/String;)J")
               (.visitJumpInsn Opcodes/GOTO $end)
               (.visitLabel $only-zeroes)
               (.visitInsn Opcodes/POP)
               (.visitVarInsn Opcodes/ALOAD 0)
               (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Long" "parseUnsignedLong" "(Ljava/lang/String;)J")
               (.visitLabel $end)
               (.visitInsn Opcodes/LRETURN)
               (.visitMaxs 0 0)
               (.visitEnd)))
         ]
    nil))

(let [+wrapper-class+ (&host-generics/->bytecode-class-name "java.lang.Long")]
  (defn ^:private compile-LuxRT-nat-methods [^ClassWriter =class]
    (|let [;; http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/8u40-b25/java/lang/Long.java#677
           _ (let [$from (new Label)
                   $to (new Label)
                   $handler (new Label)
                   
                   $good-start (new Label)
                   $short-enough (new Label)
                   $bad-digit (new Label)
                   $out-of-bounds (new Label)]
               (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "decode_nat" "(Ljava/lang/String;)Ljava/lang/Object;" nil nil)
                 (.visitCode)
                 (.visitTryCatchBlock $from $to $handler "java/lang/Exception")
                 (.visitLabel $from)
                 ;; Remove the + at the beginning...
                 (.visitVarInsn Opcodes/ALOAD 0)
                 (.visitLdcInsn (int 0))
                 (.visitLdcInsn (int 0))
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "substring" "(II)Ljava/lang/String;")
                 (.visitLdcInsn "+")
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/Object" "equals" "(Ljava/lang/Object;)Z")
                 (.visitJumpInsn Opcodes/IFNE $good-start)
                 ;; Doesn't start with +
                 (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_none" "()Ljava/lang/Object;")
                 (.visitInsn Opcodes/ARETURN)
                 ;; Starts with +
                 (.visitLabel $good-start)
                 (.visitVarInsn Opcodes/ALOAD 0)
                 (.visitLdcInsn (int 1))
                 (.visitVarInsn Opcodes/ALOAD 0)
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "length" "()I")
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "substring" "(II)Ljava/lang/String;")
                 (.visitVarInsn Opcodes/ASTORE 0) ;; Removed the + prefix...
                 ;; Begin parsing processs
                 (.visitVarInsn Opcodes/ALOAD 0)
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "length" "()I")
                 (.visitLdcInsn (int 18))
                 (.visitJumpInsn Opcodes/IF_ICMPLE $short-enough)
                 ;; Too long
                 ;; Get prefix...
                 (.visitVarInsn Opcodes/ALOAD 0)
                 (.visitLdcInsn (int 0))
                 (.visitVarInsn Opcodes/ALOAD 0)
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "length" "()I")
                 (.visitLdcInsn (int 1))
                 (.visitInsn Opcodes/ISUB)
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "substring" "(II)Ljava/lang/String;")
                 (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Long" "parseLong" "(Ljava/lang/String;)J")
                 (.visitInsn Opcodes/DUP2) ;; Clone prefix, for later...
                 ;; Get last digit...
                 (.visitVarInsn Opcodes/ALOAD 0)
                 (.visitVarInsn Opcodes/ALOAD 0)
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "length" "()I")
                 (.visitLdcInsn (int 1))
                 (.visitInsn Opcodes/ISUB)
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "charAt" "(I)C")
                 (.visitLdcInsn (int 10))
                 (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Character" "digit" "(CI)I")
                 ;; Test last digit...
                 (.visitInsn Opcodes/DUP)
                 (.visitJumpInsn Opcodes/IFLT $bad-digit)
                 ;; Good digit...
                 ;; Stack: prefix::L, prefix::L, last-digit::I
                 (.visitInsn Opcodes/I2L)
                 ;; Build the result...
                 swap2
                 (.visitLdcInsn (long 10))
                 (.visitInsn Opcodes/LMUL)
                 (.visitInsn Opcodes/LADD) ;; Stack: prefix::L, result::L
                 (.visitInsn Opcodes/DUP2_X2) ;; Stack: result::L, prefix::L, result::L
                 swap2 ;; Stack: result::L, result::L, prefix::L
                 (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_compareUnsigned" "(JJ)I")
                 (.visitJumpInsn Opcodes/IFLT $out-of-bounds)
                 ;; Within bounds
                 ;; Stack: result::L
                 &&/wrap-long
                 (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_some" "(Ljava/lang/Object;)Ljava/lang/Object;")
                 (.visitInsn Opcodes/ARETURN)
                 ;; Out of bounds
                 (.visitLabel $out-of-bounds)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_none" "()Ljava/lang/Object;")
                 (.visitInsn Opcodes/ARETURN)
                 ;; Bad digit...
                 (.visitLabel $bad-digit)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_none" "()Ljava/lang/Object;")
                 (.visitInsn Opcodes/ARETURN)
                 ;; 18 chars or less
                 (.visitLabel $short-enough)
                 (.visitVarInsn Opcodes/ALOAD 0)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Long" "parseLong" "(Ljava/lang/String;)J")
                 &&/wrap-long
                 (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_some" "(Ljava/lang/Object;)Ljava/lang/Object;")
                 (.visitInsn Opcodes/ARETURN)
                 (.visitLabel $to)
                 (.visitLabel $handler)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_none" "()Ljava/lang/Object;")
                 (.visitInsn Opcodes/ARETURN)
                 (.visitMaxs 0 0)
                 (.visitEnd)))
           ;; http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/8u40-b25/java/lang/Long.java#172
           _ (let [$too-big (new Label)]
               (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "encode_nat" "(J)Ljava/lang/String;" nil nil)
                 (.visitCode)
                 (.visitLdcInsn "+")
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitLdcInsn (long 0))
                 (.visitInsn Opcodes/LCMP)
                 (.visitJumpInsn Opcodes/IFLT $too-big)
                 ;; then
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Long" "toString" "(J)Ljava/lang/String;")
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "concat" "(Ljava/lang/String;)Ljava/lang/String;")
                 (.visitInsn Opcodes/ARETURN)
                 ;; else
                 (.visitLabel $too-big)
                 ;; Set up parts of the number string...
                 ;; First digits
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitLdcInsn (int 1))
                 (.visitInsn Opcodes/LUSHR)
                 (.visitLdcInsn (long 5))
                 (.visitInsn Opcodes/LDIV) ;; quot
                 ;; Last digit
                 (.visitInsn Opcodes/DUP2)
                 (.visitLdcInsn (long 10))
                 (.visitInsn Opcodes/LMUL)
                 (.visitVarInsn Opcodes/LLOAD 0)
                 swap2
                 (.visitInsn Opcodes/LSUB) ;; quot, rem
                 ;; Conversion to string...
                 (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Long" "toString" "(J)Ljava/lang/String;") ;; quot, rem*
                 (.visitInsn Opcodes/DUP_X2);; rem*, quot, rem*
                 (.visitInsn Opcodes/POP) ;; rem*, quot
                 (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Long" "toString" "(J)Ljava/lang/String;") ;; rem*, quot*
                 (.visitInsn Opcodes/SWAP) ;; quot*, rem*
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "concat" "(Ljava/lang/String;)Ljava/lang/String;")
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "concat" "(Ljava/lang/String;)Ljava/lang/String;")
                 (.visitInsn Opcodes/ARETURN)
                 (.visitMaxs 0 0)
                 (.visitEnd)))
           ;; http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/8u40-b25/java/lang/Long.java#215
           _ (let [$simple-case (new Label)]
               (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "_toUnsignedBigInteger" "(J)Ljava/math/BigInteger;" nil nil)
                 (.visitCode)
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitLdcInsn (long 0))
                 (.visitInsn Opcodes/LCMP)
                 (.visitJumpInsn Opcodes/IFGE $simple-case)
                 ;; else
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitLdcInsn (int 32))
                 (.visitInsn Opcodes/LUSHR)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;")
                 (.visitLdcInsn (int 32))
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/math/BigInteger" "shiftLeft" "(I)Ljava/math/BigInteger;")
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitLdcInsn (int 32))
                 (.visitInsn Opcodes/LSHL)
                 (.visitLdcInsn (int 32))
                 (.visitInsn Opcodes/LUSHR)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;")
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/math/BigInteger" "add" "(Ljava/math/BigInteger;)Ljava/math/BigInteger;")
                 (.visitInsn Opcodes/ARETURN)
                 ;; then
                 (.visitLabel $simple-case)
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;")
                 (.visitInsn Opcodes/ARETURN)
                 (.visitMaxs 0 0)
                 (.visitEnd)))
           ;; http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/8u40-b25/java/lang/Long.java?av=f#1267
           _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "_compareUnsigned" "(JJ)I" nil nil)
               (.visitCode)
               (.visitVarInsn Opcodes/LLOAD 0)
               (.visitFieldInsn Opcodes/GETSTATIC "java/lang/Long" "MIN_VALUE" "J")
               (.visitInsn Opcodes/LADD)
               (.visitVarInsn Opcodes/LLOAD 2)
               (.visitFieldInsn Opcodes/GETSTATIC "java/lang/Long" "MIN_VALUE" "J")
               (.visitInsn Opcodes/LADD)
               (.visitInsn Opcodes/LCMP)
               (.visitInsn Opcodes/IRETURN)
               (.visitMaxs 0 0)
               (.visitEnd))
           ;; http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/8u40-b25/java/lang/Long.java#1290
           _ (let [$case-1 (new Label)
                   $0 (new Label)
                   $case-2 (new Label)]
               (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "div_nat" "(JJ)J" nil nil)
                 (.visitCode)
                 ;; Test #1
                 (.visitVarInsn Opcodes/LLOAD 2)
                 (.visitLdcInsn (long 0))
                 (.visitInsn Opcodes/LCMP)
                 (.visitJumpInsn Opcodes/IFLT $case-1)
                 ;; Test #2
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitLdcInsn (long 0))
                 (.visitInsn Opcodes/LCMP)
                 (.visitJumpInsn Opcodes/IFGT $case-2)
                 ;; Case #3
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_toUnsignedBigInteger" "(J)Ljava/math/BigInteger;")
                 (.visitVarInsn Opcodes/LLOAD 2)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_toUnsignedBigInteger" "(J)Ljava/math/BigInteger;")
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/math/BigInteger" "divide" "(Ljava/math/BigInteger;)Ljava/math/BigInteger;")
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/math/BigInteger" "longValue" "()J")
                 (.visitInsn Opcodes/LRETURN)
                 ;; Case #2
                 (.visitLabel $case-2)
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitVarInsn Opcodes/LLOAD 2)
                 (.visitInsn Opcodes/LDIV)
                 (.visitInsn Opcodes/LRETURN)
                 ;; Case #1
                 (.visitLabel $case-1)
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitVarInsn Opcodes/LLOAD 2)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_compareUnsigned" "(JJ)I")
                 (.visitJumpInsn Opcodes/IFLT $0)
                 ;; 1
                 (.visitLdcInsn (long 1))
                 (.visitInsn Opcodes/LRETURN)
                 ;; 0
                 (.visitLabel $0)
                 (.visitLdcInsn (long 0))
                 (.visitInsn Opcodes/LRETURN)
                 (.visitMaxs 0 0)
                 (.visitEnd)))
           ;; http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/8u40-b25/java/lang/Long.java#1323
           _ (let [$test-2 (new Label)
                   $case-2 (new Label)]
               (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "rem_nat" "(JJ)J" nil nil)
                 (.visitCode)
                 ;; Test #1
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitLdcInsn (long 0))
                 (.visitInsn Opcodes/LCMP)
                 (.visitJumpInsn Opcodes/IFLE $test-2)
                 (.visitVarInsn Opcodes/LLOAD 2)
                 (.visitLdcInsn (long 0))
                 (.visitInsn Opcodes/LCMP)
                 (.visitJumpInsn Opcodes/IFLE $test-2)
                 ;; Case #1
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitVarInsn Opcodes/LLOAD 2)
                 (.visitInsn Opcodes/LREM)
                 (.visitInsn Opcodes/LRETURN)
                 ;; Test #2
                 (.visitLabel $test-2)
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitVarInsn Opcodes/LLOAD 2)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_compareUnsigned" "(JJ)I")
                 (.visitJumpInsn Opcodes/IFLT $case-2)
                 ;; Case #3
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_toUnsignedBigInteger" "(J)Ljava/math/BigInteger;")
                 (.visitVarInsn Opcodes/LLOAD 2)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_toUnsignedBigInteger" "(J)Ljava/math/BigInteger;")
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/math/BigInteger" "remainder" "(Ljava/math/BigInteger;)Ljava/math/BigInteger;")
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/math/BigInteger" "longValue" "()J")
                 (.visitInsn Opcodes/LRETURN)
                 ;; Case #2
                 (.visitLabel $case-2)
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitInsn Opcodes/LRETURN)
                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                 (.visitMaxs 0 0)
                 (.visitEnd)))]
      nil)))

(defn ^:private compile-LuxRT-pm-methods [^ClassWriter =class]
  (|let [_ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "pm_fail" "()V" nil nil)
             (.visitCode)
             (.visitTypeInsn Opcodes/NEW "java/lang/IllegalStateException")
             (.visitInsn Opcodes/DUP)
             (.visitLdcInsn "Invalid expression for pattern-matching.")
             (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/IllegalStateException" "<init>" "(Ljava/lang/String;)V")
             (.visitInsn Opcodes/ATHROW)
             (.visitMaxs 0 0)
             (.visitEnd))
         _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "pm_stack_push" "([Ljava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;" nil nil)
             (.visitCode)
             (.visitLdcInsn (int 2))
             (.visitTypeInsn Opcodes/ANEWARRAY "java/lang/Object")
             (.visitInsn Opcodes/DUP)
             (.visitLdcInsn (int 0))
             (.visitVarInsn Opcodes/ALOAD 0)
             (.visitInsn Opcodes/AASTORE)
             (.visitInsn Opcodes/DUP)
             (.visitLdcInsn (int 1))
             (.visitVarInsn Opcodes/ALOAD 1)
             (.visitInsn Opcodes/AASTORE)
             (.visitInsn Opcodes/ARETURN)
             (.visitMaxs 0 0)
             (.visitEnd))
         _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "pm_stack_pop" "([Ljava/lang/Object;)[Ljava/lang/Object;" nil nil)
             (.visitCode)
             (.visitVarInsn Opcodes/ALOAD 0)
             (.visitLdcInsn (int 0))
             (.visitInsn Opcodes/AALOAD)
             (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
             (.visitInsn Opcodes/ARETURN)
             (.visitMaxs 0 0)
             (.visitEnd))
         _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "pm_stack_peek" "([Ljava/lang/Object;)Ljava/lang/Object;" nil nil)
             (.visitCode)
             (.visitVarInsn Opcodes/ALOAD 0)
             (.visitLdcInsn (int 1))
             (.visitInsn Opcodes/AALOAD)
             (.visitInsn Opcodes/ARETURN)
             (.visitMaxs 0 0)
             (.visitEnd))]
    nil))

(def compile-LuxRT-class
  (|do [_ (return nil)
        :let [full-name &&/lux-utils-class
              super-class (&host-generics/->bytecode-class-name "java.lang.Object")
              tag-sig (&host-generics/->type-signature "java.lang.String")
              =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                       (.visit &host/bytecode-version (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
                               full-name nil super-class (into-array String [])))
              =unit-tag (doto (.visitField =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) &&/unit-tag-field tag-sig nil &/unit-tag)
                          (.visitEnd))
              =init-method (doto (.visitMethod =class Opcodes/ACC_PRIVATE init-method "()V" nil nil)
                             (.visitCode)
                             (.visitVarInsn Opcodes/ALOAD 0)
                             (.visitMethodInsn Opcodes/INVOKESPECIAL super-class init-method "()V")
                             (.visitInsn Opcodes/RETURN)
                             (.visitMaxs 0 0)
                             (.visitEnd))
              _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "log" "(Ljava/lang/Object;)Ljava/lang/Object;" nil nil)
                  (.visitCode)
                  (.visitFieldInsn Opcodes/GETSTATIC "java/lang/System" "out" "Ljava/io/PrintStream;")
                  (.visitLdcInsn "LOG: ")
                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/io/PrintStream" "print" "(Ljava/lang/Object;)V")
                  (.visitFieldInsn Opcodes/GETSTATIC "java/lang/System" "out" "Ljava/io/PrintStream;")
                  (.visitVarInsn Opcodes/ALOAD 0)
                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/io/PrintStream" "println" "(Ljava/lang/Object;)V")
                  (.visitVarInsn Opcodes/ALOAD 0)
                  (.visitInsn Opcodes/ARETURN)
                  (.visitMaxs 0 0)
                  (.visitEnd))
              _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "make_none" "()Ljava/lang/Object;" nil nil)
                  (.visitCode)
                  (.visitLdcInsn (->> #'&/$None meta ::&/idx int)) ;; I
                  (.visitInsn Opcodes/ACONST_NULL) ;; I?
                  (.visitLdcInsn &/unit-tag) ;; I?U
                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "sum_make" "(ILjava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;")
                  (.visitInsn Opcodes/ARETURN)
                  (.visitMaxs 0 0)
                  (.visitEnd))
              _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "make_some" "(Ljava/lang/Object;)Ljava/lang/Object;" nil nil)
                  (.visitCode)
                  (.visitLdcInsn (->> #'&/$Some meta ::&/idx int)) ;; I
                  (.visitInsn Opcodes/ACONST_NULL) ;; I?
                  (.visitVarInsn Opcodes/ALOAD 0) ;; I?O
                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "sum_make" "(ILjava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;")
                  (.visitInsn Opcodes/ARETURN)
                  (.visitMaxs 0 0)
                  (.visitEnd))
              _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "clean_separators" "(Ljava/lang/String;)Ljava/lang/String;" nil nil)
                  (.visitCode)
                  (.visitVarInsn Opcodes/ALOAD 0)
                  (.visitLdcInsn ",|_")
                  (.visitLdcInsn "")
                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "replaceAll" "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;")
                  (.visitInsn Opcodes/ARETURN)
                  (.visitMaxs 0 0)
                  (.visitEnd))
              _ (doto =class
                  (compile-LuxRT-pm-methods)
                  (compile-LuxRT-adt-methods)
                  (compile-LuxRT-nat-methods)
                  (compile-LuxRT-frac-methods))]]
    (&&/save-class! (second (string/split &&/lux-utils-class #"/"))
                    (.toByteArray (doto =class .visitEnd)))))

(defn ^:private compile-jvm-try [compile ?values special-args]
  (|do [:let [(&/$Cons ?body (&/$Cons ?catch (&/$Nil))) ?values
              ;; (&/$Nil) special-args
              ]
        ^MethodVisitor *writer* &/get-writer
        :let [$from (new Label)
              $to (new Label)
              $handler (new Label)
              $end (new Label)]
        :let [_ (doto *writer*
                  (.visitTryCatchBlock $from $to $handler "java/lang/Exception")
                  (.visitLabel $from))]
        _ (compile ?body)
        :let [_ (doto *writer*
                  (.visitJumpInsn Opcodes/GOTO $end)
                  (.visitLabel $to)
                  (.visitLabel $handler))]
        _ (compile ?catch)
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/CHECKCAST &&/function-class)
                  (.visitInsn Opcodes/SWAP)
                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL &&/function-class &&/apply-method (&&/apply-signature 1)))]
        :let [_ (.visitLabel *writer* $end)]]
    (return nil)))

(do-template [<name> <op> <from-class> <from-method> <from-sig> <to-class> <to-sig>]
  (defn <name> [compile _?value special-args]
    (|do [:let [(&/$Cons ?value (&/$Nil)) _?value]
          ^MethodVisitor *writer* &/get-writer
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/NEW (&host-generics/->bytecode-class-name <to-class>))
                    (.visitInsn Opcodes/DUP))]
          _ (compile ?value)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name <from-class>))
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host-generics/->bytecode-class-name <from-class>) <from-method> <from-sig>)
                    (.visitInsn <op>)
                    (.visitMethodInsn Opcodes/INVOKESPECIAL (&host-generics/->bytecode-class-name <to-class>) init-method <to-sig>))]]
      (return nil)))

  ^:private compile-jvm-d2f Opcodes/D2F "java.lang.Double"  "doubleValue"  "()D" "java.lang.Float"     "(F)V"
  ^:private compile-jvm-d2i Opcodes/D2I "java.lang.Double"  "doubleValue"  "()D" "java.lang.Integer"   "(I)V"
  ^:private compile-jvm-d2l Opcodes/D2L "java.lang.Double"  "doubleValue"  "()D" "java.lang.Long"      "(J)V"

  ^:private compile-jvm-f2d Opcodes/F2D "java.lang.Float"   "floatValue"   "()F" "java.lang.Double"    "(D)V"
  ^:private compile-jvm-f2i Opcodes/F2I "java.lang.Float"   "floatValue"   "()F" "java.lang.Integer"   "(I)V"
  ^:private compile-jvm-f2l Opcodes/F2L "java.lang.Float"   "floatValue"   "()F" "java.lang.Long"      "(J)V"

  ^:private compile-jvm-i2b Opcodes/I2B "java.lang.Integer" "intValue"     "()I" "java.lang.Byte"      "(B)V"
  ^:private compile-jvm-i2c Opcodes/I2C "java.lang.Integer" "intValue"     "()I" "java.lang.Character" "(C)V"
  ^:private compile-jvm-i2d Opcodes/I2D "java.lang.Integer" "intValue"     "()I" "java.lang.Double"    "(D)V"
  ^:private compile-jvm-i2f Opcodes/I2F "java.lang.Integer" "intValue"     "()I" "java.lang.Float"     "(F)V"
  ^:private compile-jvm-i2l Opcodes/I2L "java.lang.Integer" "intValue"     "()I" "java.lang.Long"      "(J)V"
  ^:private compile-jvm-i2s Opcodes/I2S "java.lang.Integer" "intValue"     "()I" "java.lang.Short"     "(S)V"

  ^:private compile-jvm-l2d Opcodes/L2D "java.lang.Long"    "longValue"    "()J" "java.lang.Double"    "(D)V"
  ^:private compile-jvm-l2f Opcodes/L2F "java.lang.Long"    "longValue"    "()J" "java.lang.Float"     "(F)V"
  ^:private compile-jvm-l2i Opcodes/L2I "java.lang.Long"    "longValue"    "()J" "java.lang.Integer"   "(I)V"
  ^:private compile-jvm-l2s Opcodes/L2I "java.lang.Long"    "longValue"    "()J" "java.lang.Short"     "(S)V"
  ^:private compile-jvm-l2b Opcodes/L2I "java.lang.Long"    "longValue"    "()J" "java.lang.Byte"      "(B)V"

  ^:private compile-jvm-c2b Opcodes/I2B "java.lang.Character" "charValue"  "()C" "java.lang.Byte"      "(B)V"
  ^:private compile-jvm-c2s Opcodes/I2S "java.lang.Character" "charValue"  "()C" "java.lang.Short"     "(S)V"
  ^:private compile-jvm-c2i Opcodes/NOP "java.lang.Character" "charValue"  "()C" "java.lang.Integer"   "(I)V"
  ^:private compile-jvm-c2l Opcodes/I2L "java.lang.Character" "charValue"  "()C" "java.lang.Long"      "(J)V"

  ^:private compile-jvm-s2l Opcodes/I2L "java.lang.Short"     "shortValue" "()S" "java.lang.Long"      "(J)V"
  
  ^:private compile-jvm-b2l Opcodes/I2L "java.lang.Byte"      "byteValue"  "()B" "java.lang.Long"      "(J)V"
  )

(do-template [<name> <op> <from1-method> <from1-sig> <from1-class> <from2-method> <from2-sig> <from2-class> <to-class> <to-sig>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          ^MethodVisitor *writer* &/get-writer
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/NEW (&host-generics/->bytecode-class-name <to-class>))
                    (.visitInsn Opcodes/DUP))]
          _ (compile ?x)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name <from1-class>))
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host-generics/->bytecode-class-name <from1-class>) <from1-method> <from1-sig>))]
          _ (compile ?y)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name <from2-class>))
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host-generics/->bytecode-class-name <from2-class>) <from2-method> <from2-sig>))]
          :let [_ (doto *writer*
                    (.visitInsn <op>)
                    (.visitMethodInsn Opcodes/INVOKESPECIAL (&host-generics/->bytecode-class-name <to-class>) init-method <to-sig>))]]
      (return nil)))

  ^:private compile-jvm-iand  Opcodes/IAND  "intValue"  "()I" "java.lang.Integer" "intValue"  "()I" "java.lang.Integer" "java.lang.Integer" "(I)V"
  ^:private compile-jvm-ior   Opcodes/IOR   "intValue"  "()I" "java.lang.Integer" "intValue"  "()I" "java.lang.Integer" "java.lang.Integer" "(I)V"
  ^:private compile-jvm-ixor  Opcodes/IXOR  "intValue"  "()I" "java.lang.Integer" "intValue"  "()I" "java.lang.Integer" "java.lang.Integer" "(I)V"
  ^:private compile-jvm-ishl  Opcodes/ISHL  "intValue"  "()I" "java.lang.Integer" "intValue"  "()I" "java.lang.Integer" "java.lang.Integer" "(I)V"
  ^:private compile-jvm-ishr  Opcodes/ISHR  "intValue"  "()I" "java.lang.Integer" "intValue"  "()I" "java.lang.Integer" "java.lang.Integer" "(I)V"
  ^:private compile-jvm-iushr Opcodes/IUSHR "intValue"  "()I" "java.lang.Integer" "intValue"  "()I" "java.lang.Integer" "java.lang.Integer" "(I)V"
  
  ^:private compile-jvm-land  Opcodes/LAND  "longValue" "()J" "java.lang.Long"    "longValue" "()J" "java.lang.Long"    "java.lang.Long"    "(J)V"
  ^:private compile-jvm-lor   Opcodes/LOR   "longValue" "()J" "java.lang.Long"    "longValue" "()J" "java.lang.Long"    "java.lang.Long"    "(J)V"
  ^:private compile-jvm-lxor  Opcodes/LXOR  "longValue" "()J" "java.lang.Long"    "longValue" "()J" "java.lang.Long"    "java.lang.Long"    "(J)V"
  ^:private compile-jvm-lshl  Opcodes/LSHL  "longValue" "()J" "java.lang.Long"    "intValue"  "()I" "java.lang.Integer" "java.lang.Long"    "(J)V"
  ^:private compile-jvm-lshr  Opcodes/LSHR  "longValue" "()J" "java.lang.Long"    "intValue"  "()I" "java.lang.Integer" "java.lang.Long"    "(J)V"
  ^:private compile-jvm-lushr Opcodes/LUSHR "longValue" "()J" "java.lang.Long"    "intValue"  "()I" "java.lang.Integer" "java.lang.Long"    "(J)V"
  )

(do-template [<name> <opcode> <wrapper-class> <value-method> <value-method-sig> <wrap>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          :let [+wrapper-class+ (&host-generics/->bytecode-class-name <wrapper-class>)]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?x)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ <value-method> <value-method-sig>))]
          _ (compile ?y)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ <value-method> <value-method-sig>))
                _ (doto *writer*
                    (.visitInsn <opcode>)
                    (<wrap>))]]
      (return nil)))

  ^:private compile-jvm-iadd Opcodes/IADD "java.lang.Integer" "intValue"    "()I" &&/wrap-int
  ^:private compile-jvm-isub Opcodes/ISUB "java.lang.Integer" "intValue"    "()I" &&/wrap-int
  ^:private compile-jvm-imul Opcodes/IMUL "java.lang.Integer" "intValue"    "()I" &&/wrap-int
  ^:private compile-jvm-idiv Opcodes/IDIV "java.lang.Integer" "intValue"    "()I" &&/wrap-int
  ^:private compile-jvm-irem Opcodes/IREM "java.lang.Integer" "intValue"    "()I" &&/wrap-int
  
  ^:private compile-jvm-ladd Opcodes/LADD "java.lang.Long"    "longValue"   "()J" &&/wrap-long
  ^:private compile-jvm-lsub Opcodes/LSUB "java.lang.Long"    "longValue"   "()J" &&/wrap-long
  ^:private compile-jvm-lmul Opcodes/LMUL "java.lang.Long"    "longValue"   "()J" &&/wrap-long
  ^:private compile-jvm-ldiv Opcodes/LDIV "java.lang.Long"    "longValue"   "()J" &&/wrap-long
  ^:private compile-jvm-lrem Opcodes/LREM "java.lang.Long"    "longValue"   "()J" &&/wrap-long

  ^:private compile-jvm-fadd Opcodes/FADD "java.lang.Float"   "floatValue"  "()F" &&/wrap-float
  ^:private compile-jvm-fsub Opcodes/FSUB "java.lang.Float"   "floatValue"  "()F" &&/wrap-float
  ^:private compile-jvm-fmul Opcodes/FMUL "java.lang.Float"   "floatValue"  "()F" &&/wrap-float
  ^:private compile-jvm-fdiv Opcodes/FDIV "java.lang.Float"   "floatValue"  "()F" &&/wrap-float
  ^:private compile-jvm-frem Opcodes/FREM "java.lang.Float"   "floatValue"  "()F" &&/wrap-float
  
  ^:private compile-jvm-dadd Opcodes/DADD "java.lang.Double"  "doubleValue" "()D" &&/wrap-double
  ^:private compile-jvm-dsub Opcodes/DSUB "java.lang.Double"  "doubleValue" "()D" &&/wrap-double
  ^:private compile-jvm-dmul Opcodes/DMUL "java.lang.Double"  "doubleValue" "()D" &&/wrap-double
  ^:private compile-jvm-ddiv Opcodes/DDIV "java.lang.Double"  "doubleValue" "()D" &&/wrap-double
  ^:private compile-jvm-drem Opcodes/DREM "java.lang.Double"  "doubleValue" "()D" &&/wrap-double
  )

(do-template [<name> <opcode> <wrapper-class> <value-method> <value-method-sig>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          :let [+wrapper-class+ (&host-generics/->bytecode-class-name <wrapper-class>)]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?x)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ <value-method> <value-method-sig>))]
          _ (compile ?y)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ <value-method> <value-method-sig>))
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

  ^:private compile-jvm-ieq Opcodes/IF_ICMPEQ "java.lang.Integer" "intValue" "()I"
  ^:private compile-jvm-ilt Opcodes/IF_ICMPLT "java.lang.Integer" "intValue" "()I"
  ^:private compile-jvm-igt Opcodes/IF_ICMPGT "java.lang.Integer" "intValue" "()I"

  ^:private compile-jvm-ceq Opcodes/IF_ICMPEQ "java.lang.Character" "charValue" "()C"
  ^:private compile-jvm-clt Opcodes/IF_ICMPLT "java.lang.Character" "charValue" "()C"
  ^:private compile-jvm-cgt Opcodes/IF_ICMPGT "java.lang.Character" "charValue" "()C"
  )

(do-template [<name> <cmpcode> <cmp-output> <wrapper-class> <value-method> <value-method-sig>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          :let [+wrapper-class+ (&host-generics/->bytecode-class-name <wrapper-class>)]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?x)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ <value-method> <value-method-sig>))]
          _ (compile ?y)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ <value-method> <value-method-sig>))
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

  ^:private compile-jvm-leq Opcodes/LCMP   0 "java.lang.Long"   "longValue"   "()J"
  ^:private compile-jvm-llt Opcodes/LCMP  -1 "java.lang.Long"   "longValue"   "()J"
  ^:private compile-jvm-lgt Opcodes/LCMP   1 "java.lang.Long"   "longValue"   "()J"

  ^:private compile-jvm-feq Opcodes/FCMPG  0 "java.lang.Float"  "floatValue"  "()F"
  ^:private compile-jvm-flt Opcodes/FCMPG -1 "java.lang.Float"  "floatValue"  "()F"
  ^:private compile-jvm-fgt Opcodes/FCMPG  1 "java.lang.Float"  "floatValue"  "()F"
  
  ^:private compile-jvm-deq Opcodes/DCMPG  0 "java.lang.Double" "doubleValue" "()D"
  ^:private compile-jvm-dlt Opcodes/DCMPG -1 "java.lang.Double" "doubleValue" "()D"
  ^:private compile-jvm-dgt Opcodes/DCMPG  1 "java.lang.Double" "doubleValue" "()D"
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

(do-template [<name> <op>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?monitor (&/$Nil)) ?values
                ;; (&/$Nil) special-args
                ]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?monitor)
          :let [_ (doto *writer*
                    (.visitInsn <op>)
                    (.visitInsn Opcodes/ACONST_NULL))]]
      (return nil)))

  ^:private compile-jvm-monitorenter Opcodes/MONITORENTER
  ^:private compile-jvm-monitorexit  Opcodes/MONITOREXIT
  )

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

(defn ^:private compile-jvm-try [compile ?values special-args]
  (|do [:let [(&/$Cons ?body (&/$Cons ?catch (&/$Nil))) ?values
              ;; (&/$Nil) special-args
              ]
        ^MethodVisitor *writer* &/get-writer
        :let [$from (new Label)
              $to (new Label)
              $handler (new Label)
              $end (new Label)]
        :let [_ (doto *writer*
                  (.visitTryCatchBlock $from $to $handler "java/lang/Exception")
                  (.visitLabel $from))]
        _ (compile ?body)
        :let [_ (doto *writer*
                  (.visitJumpInsn Opcodes/GOTO $end)
                  (.visitLabel $to)
                  (.visitLabel $handler))]
        _ (compile ?catch)
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/CHECKCAST &&/function-class)
                  (.visitInsn Opcodes/SWAP)
                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL &&/function-class &&/apply-method (&&/apply-signature 1)))]
        :let [_ (.visitLabel *writer* $end)]]
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

(defn ^:private compile-array-get [compile ?values special-args]
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
        :let [_ (.visitInsn *writer* Opcodes/AALOAD)]
        :let [$is-null (new Label)
              $end (new Label)
              _ (doto *writer*
                  (.visitInsn Opcodes/DUP)
                  (.visitJumpInsn Opcodes/IFNULL $is-null)
                  (.visitLdcInsn (int 1))
                  (.visitLdcInsn "")
                  (.visitInsn Opcodes/DUP2_X1) ;; I?2I?
                  (.visitInsn Opcodes/POP2) ;; I?2
                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "sum_make" "(ILjava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;")
                  (.visitJumpInsn Opcodes/GOTO $end)
                  (.visitLabel $is-null)
                  (.visitInsn Opcodes/POP)
                  (.visitLdcInsn (int 0))
                  (.visitInsn Opcodes/ACONST_NULL)
                  (.visitLdcInsn &/unit-tag)
                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "sum_make" "(ILjava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;")
                  (.visitLabel $end))]]
    (return nil)))

(do-template [<name> <op>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?input (&/$Cons ?mask (&/$Nil))) ?values]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?input)
          :let [_ (&&/unwrap-long *writer*)]
          _ (compile ?mask)
          :let [_ (&&/unwrap-long *writer*)]
          :let [_ (doto *writer*
                    (.visitInsn <op>)
                    &&/wrap-long)]]
      (return nil)))

  ^:private compile-bit-and Opcodes/LAND
  ^:private compile-bit-or  Opcodes/LOR
  ^:private compile-bit-xor Opcodes/LXOR
  )

(defn ^:private compile-bit-count [compile ?values special-args]
  (|do [:let [(&/$Cons ?input (&/$Nil)) ?values]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?input)
        :let [_ (&&/unwrap-long *writer*)]
        :let [_ (doto *writer*
                  (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Long" "bitCount" "(J)I")
                  (.visitInsn Opcodes/I2L)
                  &&/wrap-long)]]
    (return nil)))

(do-template [<name> <op>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?input (&/$Cons ?shift (&/$Nil))) ?values]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?input)
          :let [_ (&&/unwrap-long *writer*)]
          _ (compile ?shift)
          :let [_ (doto *writer*
                    &&/unwrap-long
                    (.visitInsn Opcodes/L2I))]
          :let [_ (doto *writer*
                    (.visitInsn <op>)
                    &&/wrap-long)]]
      (return nil)))

  ^:private compile-bit-shift-left           Opcodes/LSHL
  ^:private compile-bit-shift-right          Opcodes/LSHR
  ^:private compile-bit-unsigned-shift-right Opcodes/LUSHR
  )

(defn ^:private compile-lux-== [compile ?values special-args]
  (|do [:let [(&/$Cons ?left (&/$Cons ?right (&/$Nil))) ?values]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?left)
        _ (compile ?right)
        :let [$then (new Label)
              $end (new Label)
              _ (doto *writer*
                  (.visitJumpInsn Opcodes/IF_ACMPEQ $then)
                  ;; else
                  (.visitFieldInsn Opcodes/GETSTATIC "java/lang/Boolean" "FALSE" "Ljava/lang/Boolean;")
                  (.visitJumpInsn Opcodes/GOTO $end)
                  (.visitLabel $then)
                  (.visitFieldInsn Opcodes/GETSTATIC "java/lang/Boolean" "TRUE" "Ljava/lang/Boolean;")
                  (.visitLabel $end))]]
    (return nil)))

(do-template [<name> <opcode> <wrapper-class> <value-method> <value-method-sig> <wrap>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          :let [+wrapper-class+ (&host-generics/->bytecode-class-name <wrapper-class>)]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?x)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ <value-method> <value-method-sig>))]
          _ (compile ?y)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ <value-method> <value-method-sig>))
                _ (doto *writer*
                    (.visitInsn <opcode>)
                    (<wrap>))]]
      (return nil)))

  ^:private compile-nat-add    Opcodes/LADD "java.lang.Long"    "longValue"   "()J" &&/wrap-long
  ^:private compile-nat-sub    Opcodes/LSUB "java.lang.Long"    "longValue"   "()J" &&/wrap-long
  ^:private compile-nat-mul    Opcodes/LMUL "java.lang.Long"    "longValue"   "()J" &&/wrap-long

  ^:private compile-frac-add   Opcodes/LADD "java.lang.Long"    "longValue"   "()J" &&/wrap-long
  ^:private compile-frac-sub   Opcodes/LSUB "java.lang.Long"    "longValue"   "()J" &&/wrap-long
  ^:private compile-frac-rem   Opcodes/LSUB "java.lang.Long"    "longValue"   "()J" &&/wrap-long
  ^:private compile-frac-scale Opcodes/LMUL "java.lang.Long"    "longValue"   "()J" &&/wrap-long
  )

(do-template [<name> <comp-method>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          :let [+wrapper-class+ (&host-generics/->bytecode-class-name "java.lang.Long")]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?x)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ "longValue" "()J"))]
          _ (compile ?y)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ "longValue" "()J"))
                _ (doto *writer*
                    (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" <comp-method> "(JJ)J")
                    (&&/wrap-long))]]
      (return nil)))

  ^:private compile-nat-div "div_nat"
  ^:private compile-nat-rem "rem_nat"
  )

(do-template [<name> <cmp-output>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          :let [+wrapper-class+ (&host-generics/->bytecode-class-name "java.lang.Long")]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?x)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ "longValue" "()J"))]
          _ (compile ?y)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ "longValue" "()J"))
                $then (new Label)
                $end (new Label)
                _ (doto *writer*
                    (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_compareUnsigned" "(JJ)I")
                    (.visitLdcInsn (int <cmp-output>))
                    (.visitJumpInsn Opcodes/IF_ICMPEQ $then)
                    (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "FALSE"  (&host-generics/->type-signature "java.lang.Boolean"))
                    (.visitJumpInsn Opcodes/GOTO $end)
                    (.visitLabel $then)
                    (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "TRUE" (&host-generics/->type-signature "java.lang.Boolean"))
                    (.visitLabel $end))]]
      (return nil)))

  ^:private compile-nat-eq   0
  
  ^:private compile-frac-eq  0
  ^:private compile-frac-lt -1
  )

(let [+wrapper-class+ (&host-generics/->bytecode-class-name "java.lang.Long")]
  (defn ^:private compile-nat-lt [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?x)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ "longValue" "()J"))]
          _ (compile ?y)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ "longValue" "()J"))
                $then (new Label)
                $end (new Label)
                _ (doto *writer*
                    (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_compareUnsigned" "(JJ)I")
                    (.visitLdcInsn (int -1))
                    (.visitJumpInsn Opcodes/IF_ICMPEQ $then)
                    (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "FALSE"  (&host-generics/->type-signature "java.lang.Boolean"))
                    (.visitJumpInsn Opcodes/GOTO $end)
                    (.visitLabel $then)
                    (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "TRUE" (&host-generics/->type-signature "java.lang.Boolean"))
                    (.visitLabel $end))]]
      (return nil))))

(do-template [<name> <instr> <wrapper>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Nil) ?values]
          ^MethodVisitor *writer* &/get-writer
          :let [_ (doto *writer*
                    <instr>
                    <wrapper>)]]
      (return nil)))

  ^:private compile-nat-min-value (.visitLdcInsn 0)  &&/wrap-long
  ^:private compile-nat-max-value (.visitLdcInsn -1) &&/wrap-long

  ^:private compile-frac-min-value (.visitLdcInsn 0)  &&/wrap-long
  ^:private compile-frac-max-value (.visitLdcInsn -1) &&/wrap-long
  )

(do-template [<encode-name> <encode-method> <decode-name> <decode-method>]
  (do (defn <encode-name> [compile ?values special-args]
        (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
              ^MethodVisitor *writer* &/get-writer
              _ (compile ?x)
              :let [_ (doto *writer*
                        &&/unwrap-long
                        (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" <encode-method> "(J)Ljava/lang/String;"))]]
          (return nil)))

    (let [+wrapper-class+ (&host-generics/->bytecode-class-name "java.lang.String")]
      (defn <decode-name> [compile ?values special-args]
        (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
              ^MethodVisitor *writer* &/get-writer
              _ (compile ?x)
              :let [_ (doto *writer*
                        (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                        (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" <decode-method> "(Ljava/lang/String;)Ljava/lang/Object;"))]]
          (return nil)))))

  ^:private compile-nat-encode  "encode_nat"  ^:private compile-nat-decode  "decode_nat"
  ^:private compile-frac-encode "encode_frac" ^:private compile-frac-decode "decode_frac"
  )

(do-template [<name> <method>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          :let [+wrapper-class+ (&host-generics/->bytecode-class-name "java.lang.Long")]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?x)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                    &&/unwrap-long)]
          _ (compile ?y)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                    &&/unwrap-long)]
          :let [_ (doto *writer*
                    (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" <method> "(JJ)J")
                    &&/wrap-long)]]
      (return nil)))

  ^:private compile-frac-mul   "mul_frac"
  ^:private compile-frac-div   "div_frac"
  )

(do-template [<name> <class> <method> <sig> <unwrap> <wrap>]
  (let [+wrapper-class+ (&host-generics/->bytecode-class-name <class>)]
    (defn <name> [compile ?values special-args]
      (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
            ^MethodVisitor *writer* &/get-writer
            _ (compile ?x)
            :let [_ (doto *writer*
                      <unwrap>
                      (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" <method> <sig>)
                      <wrap>)]]
        (return nil))))

  ^:private compile-frac-to-real "java.lang.Long"   "frac-to-real" "(J)D" &&/unwrap-long   &&/wrap-double
  ^:private compile-real-to-frac "java.lang.Double" "real-to-frac" "(D)J" &&/unwrap-double &&/wrap-long
  )

(let [widen (fn [^MethodVisitor *writer*]
              (doto *writer*
                (.visitInsn Opcodes/I2L)))
      shrink (fn [^MethodVisitor *writer*]
               (doto *writer*
                 (.visitInsn Opcodes/L2I)
                 (.visitInsn Opcodes/I2C)))]
  (do-template [<name> <unwrap> <wrap> <adjust>]
    (defn <name> [compile ?values special-args]
      (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
            ^MethodVisitor *writer* &/get-writer
            _ (compile ?x)
            :let [_ (doto *writer*
                      <unwrap>
                      <adjust>
                      <wrap>)]]
        (return nil)))

    ^:private compile-nat-to-char &&/unwrap-long &&/wrap-char shrink
    ^:private compile-char-to-nat &&/unwrap-char &&/wrap-long widen
    ))

(do-template [<name>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?x)]
      (return nil)))

  ^:private compile-nat-to-int
  ^:private compile-int-to-nat
  )

(defn compile-host [compile proc-category proc-name ?values special-args]
  (case proc-category
    "lux"
    (case proc-name
      "=="                   (compile-lux-== compile ?values special-args))
    
    "bit"
    (case proc-name
      "count"                (compile-bit-count compile ?values special-args)
      "and"                  (compile-bit-and compile ?values special-args)
      "or"                   (compile-bit-or compile ?values special-args)
      "xor"                  (compile-bit-xor compile ?values special-args)
      "shift-left"           (compile-bit-shift-left compile ?values special-args)
      "shift-right"          (compile-bit-shift-right compile ?values special-args)
      "unsigned-shift-right" (compile-bit-unsigned-shift-right compile ?values special-args))
    
    "array"
    (case proc-name
      "get" (compile-array-get compile ?values special-args))

    "nat"
    (case proc-name
      "+"         (compile-nat-add compile ?values special-args)
      "-"         (compile-nat-sub compile ?values special-args)
      "*"         (compile-nat-mul compile ?values special-args)
      "/"         (compile-nat-div compile ?values special-args)
      "%"         (compile-nat-rem compile ?values special-args)
      "="         (compile-nat-eq compile ?values special-args)
      "<"         (compile-nat-lt compile ?values special-args)
      "encode"    (compile-nat-encode compile ?values special-args)
      "decode"    (compile-nat-decode compile ?values special-args)
      "max-value" (compile-nat-max-value compile ?values special-args)
      "min-value" (compile-nat-min-value compile ?values special-args)
      "to-int"    (compile-nat-to-int compile ?values special-args)
      "to-char"   (compile-nat-to-char compile ?values special-args)
      )
    
    "frac"
    (case proc-name
      "+"         (compile-frac-add compile ?values special-args)
      "-"         (compile-frac-sub compile ?values special-args)
      "*"         (compile-frac-mul compile ?values special-args)
      "/"         (compile-frac-div compile ?values special-args)
      "%"         (compile-frac-rem compile ?values special-args)
      "="         (compile-frac-eq compile ?values special-args)
      "<"         (compile-frac-lt compile ?values special-args)
      "encode"    (compile-frac-encode compile ?values special-args)
      "decode"    (compile-frac-decode compile ?values special-args)
      "max-value" (compile-frac-max-value compile ?values special-args)
      "min-value" (compile-frac-min-value compile ?values special-args)
      "to-real"   (compile-frac-to-real compile ?values special-args)
      "scale"     (compile-frac-scale compile ?values special-args)
      )

    "int"
    (case proc-name
      "to-nat"    (compile-int-to-nat compile ?values special-args)
      )

    "real"
    (case proc-name
      "to-frac"    (compile-real-to-frac compile ?values special-args)
      )

    "char"
    (case proc-name
      "to-nat"    (compile-char-to-nat compile ?values special-args)
      )
    
    "jvm"
    (case proc-name
      "synchronized"    (compile-jvm-synchronized compile ?values special-args)
      "load-class"      (compile-jvm-load-class compile ?values special-args)
      "instanceof"      (compile-jvm-instanceof compile ?values special-args)
      "try"             (compile-jvm-try compile ?values special-args)
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
      "monitorenter"    (compile-jvm-monitorenter compile ?values special-args)
      "monitorexit"     (compile-jvm-monitorexit compile ?values special-args)
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
      (fail (str "[Compiler Error] Unknown host procedure: " [proc-category proc-name])))

    ;; else
    (fail (str "[Compiler Error] Unknown host procedure: " [proc-category proc-name]))))
