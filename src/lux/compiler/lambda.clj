(ns lux.compiler.lambda
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail*]]
                 [type :as &type]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser]
                 [host :as &host])
            [lux.analyser.base :as &a]
            (lux.compiler [base :as &&])
            ;; :reload
            )
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Utils]
(def ^:private clo-field-sig (&host/->type-signature "java.lang.Object"))
(def ^:private lambda-return-sig (&host/->type-signature "java.lang.Object"))
(def ^:private <init>-return "V")

(def ^:private lambda-impl-signature
  (str "("  clo-field-sig ")" lambda-return-sig))

(defn ^:private lambda-<init>-signature [env]
  (str "(" (&/fold str "" (&/|repeat (&/|length env) clo-field-sig)) ")"
       <init>-return))

(defn ^:private add-lambda-<init> [class class-name env]
  (doto (.visitMethod class Opcodes/ACC_PUBLIC "<init>" (lambda-<init>-signature env) nil nil)
    (.visitCode)
    (.visitVarInsn Opcodes/ALOAD 0)
    (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V")
    (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
          (.visitVarInsn Opcodes/ALOAD (inc ?captured-id))
          (.visitFieldInsn Opcodes/PUTFIELD class-name captured-name clo-field-sig))
        (->> (let [captured-name (str &&/closure-prefix ?captured-id)
                   ;; _ (prn 'add-lambda-<init> class-name ?captured-id)
                   ])
             (matchv ::M/objects [?name+?captured]
               [[?name [["captured" [_ ?captured-id ?source]] _]]])
             (doseq [?name+?captured (&/->seq env)])))
    (.visitInsn Opcodes/RETURN)
    (.visitMaxs 0 0)
    (.visitEnd)))

(defn ^:private add-lambda-apply [class class-name env]
  (doto (.visitMethod class Opcodes/ACC_PUBLIC "apply" &&/apply-signature nil nil)
    (.visitCode)
    (.visitVarInsn Opcodes/ALOAD 0)
    (.visitVarInsn Opcodes/ALOAD 1)
    (.visitMethodInsn Opcodes/INVOKEVIRTUAL class-name "impl" lambda-impl-signature)
    (.visitInsn Opcodes/ARETURN)
    (.visitMaxs 0 0)
    (.visitEnd)))

(defn ^:private add-lambda-impl [class compile impl-signature impl-body]
  (&/with-writer (doto (.visitMethod class Opcodes/ACC_PUBLIC "impl" impl-signature nil nil)
                   (.visitCode))
    (|do [*writer* &/get-writer
           :let [num-locals (&&/total-locals impl-body)
                 $start (new Label)
                 $end (new Label)
                 _ (doto *writer*
                     (-> (.visitLocalVariable (str &&/local-prefix idx) (&host/->java-sig (&/V "lux;DataT" "java.lang.Object")) nil $start $end (+ 2 idx))
                         (->> (dotimes [idx num-locals])))
                     (.visitLabel $start))]
           ret (compile impl-body)
           :let [_ (doto *writer*
                     (.visitLabel $end)
                     (.visitInsn Opcodes/ARETURN)
                     (.visitMaxs 0 0)
                     (.visitEnd))]]
      (return ret))))

(defn ^:private instance-closure [compile lambda-class closed-over init-signature]
  ;; (prn 'instance-closure lambda-class (&/|length closed-over) init-signature)
  (|do [*writer* &/get-writer
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/NEW lambda-class)
                  (.visitInsn Opcodes/DUP))
              ;; _ (prn 'closed-over/pre
              ;;        (&/->seq (&/|map #(matchv ::M/objects [(&/|second %1)]
              ;;                            [[["captured" [_ ?cid _]] _]]
              ;;                            ?cid)
              ;;                         closed-over)))
              ;; _ (prn 'closed-over/post
              ;;        (->> closed-over
              ;;             &/->seq
              ;;             (sort #(matchv ::M/objects [(&/|second %1) (&/|second %2)]
              ;;                      [[["captured" [_ ?cid1 _]] _]
              ;;                       [["captured" [_ ?cid2 _]] _]]
              ;;                      (< ?cid1 ?cid2)))
              ;;             &/->list
              ;;             (&/|map #(matchv ::M/objects [(&/|second %1)]
              ;;                        [[["captured" [_ ?cid _]] _]]
              ;;                        ?cid))
              ;;             &/->seq))
              ]
        _ (->> closed-over
               &/->seq
               (sort #(matchv ::M/objects [(&/|second %1) (&/|second %2)]
                        [[["captured" [_ ?cid1 _]] _]
                         [["captured" [_ ?cid2 _]] _]]
                        (< ?cid1 ?cid2)))
               &/->list
               (&/map% (fn [?name+?captured]
                         (matchv ::M/objects [?name+?captured]
                           [[?name [["captured" [_ _ ?source]] _]]]
                           (do ;; (prn '?source (aget ?source 1 0 0)
                               ;;      (cond (= "captured" (aget ?source 1 0 0))
                               ;;            ["captured" (aget ?source 1 0 1 1)]

                               ;;            (= "local" (aget ?source 1 0 0))
                               ;;            ["local" (aget ?source 1 0 1)]

                               ;;            :else
                               ;;            '???))
                             (compile ?source))))))
        :let [_ (.visitMethodInsn *writer* Opcodes/INVOKESPECIAL lambda-class "<init>" init-signature)]]
    (return nil)))

;; [Exports]
(defn compile-lambda [compile ?scope ?env ?body]
  ;; (prn 'compile-lambda ?scope (&host/location ?scope) ?env)
  (|do [:let [lambda-class (&host/location ?scope)
               =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                        (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
                                lambda-class nil "java/lang/Object" (into-array [(&host/->class &host/function-class)]))
                        (-> (doto (.visitField (+ Opcodes/ACC_PRIVATE Opcodes/ACC_FINAL) captured-name clo-field-sig nil nil)
                              (.visitEnd))
                            (->> (let [captured-name (str &&/closure-prefix ?captured-id)])
                                 (matchv ::M/objects [?name+?captured]
                                   [[?name [["captured" [_ ?captured-id ?source]] _]]])
                                 (doseq [?name+?captured (&/->seq ?env)
                                         ;; :let [_ (prn '?name+?captured (alength ?name+?captured))
                                         ;;       _ (prn '?name+?captured (aget ?name+?captured 1 0))
                                         ;;       _ (prn '?name+?captured (aget ?name+?captured 1 1 0 0))]
                                         ])))
                        (add-lambda-apply lambda-class ?env)
                        (add-lambda-<init> lambda-class ?env)
                        )]
         _ (add-lambda-impl =class compile lambda-impl-signature ?body)
         :let [_ (.visitEnd =class)]
         _ (&&/save-class! lambda-class (.toByteArray =class))]
    (instance-closure compile lambda-class ?env (lambda-<init>-signature ?env))))
