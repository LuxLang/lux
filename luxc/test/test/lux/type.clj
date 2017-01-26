(ns test.lux.type
  (:use clojure.test)
  (:require (lux [base :as & :refer [|do return* return fail fail* |let |case]]
                 [type :as &type])
            :reload-all
            ))

;; [Tests]
(deftest check-base-types
  (|case (&/run-state (|do [_ (&type/check &/$UnitT &/$UnitT)

                            _ (&type/check &/$VoidT &/$VoidT)]
                        (return nil))
                      (&/init-state nil))
    (&/$Right state nil)
    (is true)
    
    (&/$Left error)
    (is false error)
    ))

(deftest check-simple-host-types
  (|case (&/run-state (|do [_ (&type/check (&/$HostT "java.lang.Boolean" &/$Nil)
                                           (&/$HostT "java.lang.Boolean" &/$Nil))
                            
                            _ (&type/check (&/$HostT "java.lang.Object" &/$Nil)
                                           (&/$HostT "java.lang.Boolean" &/$Nil))]
                        (return nil))
                      (&/init-state nil))
    (&/$Right state nil)
    (is true)
    
    (&/$Left error)
    (is false error)
    ))

(deftest check-complex-host-types
  (|case (&/run-state (|do [_ (&type/check (&/$HostT "java.util.List" (&/|list (&/$HostT "java.lang.Boolean" &/$Nil)))
                                           (&/$HostT "java.util.List" (&/|list (&/$HostT "java.lang.Boolean" &/$Nil))))
                            
                            _ (&type/check (&/$HostT "java.util.List" (&/|list (&/$HostT "java.lang.Object" &/$Nil)))
                                           (&/$HostT "java.util.List" (&/|list (&/$HostT "java.lang.Boolean" &/$Nil))))

                            _ (&type/check (&/$HostT "java.util.List" (&/|list (&/$HostT "java.lang.Boolean" &/$Nil)))
                                           (&/$HostT "java.util.ArrayList" (&/|list (&/$HostT "java.lang.Boolean" &/$Nil))))]
                        (return nil))
                      (&/init-state nil))
    (&/$Right state nil)
    (is true)
    
    (&/$Left error)
    (is false error)
    ))

(deftest check-named-types
  (|case (&/run-state (|do [_ (&type/check (&/$NamedT (&/T ["lux" "Bool"]) (&/$HostT "java.lang.Boolean" &/$Nil))
                                           (&/$HostT "java.lang.Boolean" &/$Nil))
                            
                            _ (&type/check (&/$HostT "java.lang.Boolean" &/$Nil)
                                           (&/$NamedT (&/T ["lux" "Bool"]) (&/$HostT "java.lang.Boolean" &/$Nil)))
                            
                            _ (&type/check (&/$NamedT (&/T ["lux" "Bool"]) (&/$HostT "java.lang.Boolean" &/$Nil))
                                           (&/$NamedT (&/T ["lux" "Bool"]) (&/$HostT "java.lang.Boolean" &/$Nil)))]
                        (return nil))
                      (&/init-state nil))
    (&/$Right state nil)
    (is true)
    
    (&/$Left error)
    (is false error)
    ))

(deftest check-sum-types
  (|case (&/run-state (|do [_ (&type/check (&/$SumT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                    (&/$HostT "java.lang.Boolean" &/$Nil))
                                           (&/$SumT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                    (&/$HostT "java.lang.Boolean" &/$Nil)))

                            _ (&type/check (&/$SumT (&/$HostT "java.lang.Object" &/$Nil)
                                                    (&/$HostT "java.lang.Boolean" &/$Nil))
                                           (&/$SumT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                    (&/$HostT "java.lang.Boolean" &/$Nil)))

                            _ (&type/check (&/$SumT (&/$HostT "java.lang.Object" &/$Nil)
                                                    (&/$HostT "java.lang.Object" &/$Nil))
                                           (&/$SumT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                    (&/$HostT "java.lang.Boolean" &/$Nil)))

                            _ (&type/check (&/$SumT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                    (&/$SumT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                             (&/$HostT "java.lang.Boolean" &/$Nil)))
                                           (&/$SumT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                    (&/$SumT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                             (&/$HostT "java.lang.Boolean" &/$Nil))))

                            _ (&type/check (&/$SumT (&/$HostT "java.lang.Object" &/$Nil)
                                                    (&/$SumT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                             (&/$HostT "java.lang.Boolean" &/$Nil)))
                                           (&/$SumT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                    (&/$SumT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                             (&/$HostT "java.lang.Boolean" &/$Nil))))

                            _ (&type/check (&/$SumT (&/$HostT "java.lang.Object" &/$Nil)
                                                    (&/$SumT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                             (&/$HostT "java.lang.Object" &/$Nil)))
                                           (&/$SumT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                    (&/$SumT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                             (&/$HostT "java.lang.Boolean" &/$Nil))))]
                        (return nil))
                      (&/init-state nil))
    (&/$Right state nil)
    (is true)
    
    (&/$Left error)
    (is false error)
    ))

(deftest check-prod-types
  (|case (&/run-state (|do [_ (&type/check (&/$ProdT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                     (&/$HostT "java.lang.Boolean" &/$Nil))
                                           (&/$ProdT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                     (&/$HostT "java.lang.Boolean" &/$Nil)))

                            _ (&type/check (&/$ProdT (&/$HostT "java.lang.Object" &/$Nil)
                                                     (&/$HostT "java.lang.Boolean" &/$Nil))
                                           (&/$ProdT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                     (&/$HostT "java.lang.Boolean" &/$Nil)))

                            _ (&type/check (&/$ProdT (&/$HostT "java.lang.Object" &/$Nil)
                                                     (&/$HostT "java.lang.Object" &/$Nil))
                                           (&/$ProdT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                     (&/$HostT "java.lang.Boolean" &/$Nil)))

                            _ (&type/check (&/$ProdT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                     (&/$ProdT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                               (&/$HostT "java.lang.Boolean" &/$Nil)))
                                           (&/$ProdT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                     (&/$ProdT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                               (&/$HostT "java.lang.Boolean" &/$Nil))))

                            _ (&type/check (&/$ProdT (&/$HostT "java.lang.Object" &/$Nil)
                                                     (&/$ProdT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                               (&/$HostT "java.lang.Boolean" &/$Nil)))
                                           (&/$ProdT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                     (&/$ProdT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                               (&/$HostT "java.lang.Boolean" &/$Nil))))

                            _ (&type/check (&/$ProdT (&/$HostT "java.lang.Object" &/$Nil)
                                                     (&/$ProdT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                               (&/$HostT "java.lang.Object" &/$Nil)))
                                           (&/$ProdT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                     (&/$ProdT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                               (&/$HostT "java.lang.Boolean" &/$Nil))))]
                        (return nil))
                      (&/init-state nil))
    (&/$Right state nil)
    (is true)
    
    (&/$Left error)
    (is false error)
    ))

(deftest check-lambda-types
  (|case (&/run-state (|do [_ (&type/check (&/$LambdaT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                       (&/$HostT "java.lang.Boolean" &/$Nil))
                                           (&/$LambdaT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                       (&/$HostT "java.lang.Boolean" &/$Nil)))

                            _ (&type/check (&/$LambdaT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                       (&/$HostT "java.lang.Boolean" &/$Nil))
                                           (&/$LambdaT (&/$HostT "java.lang.Object" &/$Nil)
                                                       (&/$HostT "java.lang.Boolean" &/$Nil)))

                            _ (&type/check (&/$LambdaT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                       (&/$HostT "java.lang.Object" &/$Nil))
                                           (&/$LambdaT (&/$HostT "java.lang.Object" &/$Nil)
                                                       (&/$HostT "java.lang.Boolean" &/$Nil)))
                            
                            _ (&type/check (&/$LambdaT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                       (&/$LambdaT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                                   (&/$HostT "java.lang.Boolean" &/$Nil)))
                                           (&/$LambdaT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                       (&/$LambdaT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                                   (&/$HostT "java.lang.Boolean" &/$Nil))))

                            _ (&type/check (&/$LambdaT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                       (&/$LambdaT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                                   (&/$HostT "java.lang.Boolean" &/$Nil)))
                                           (&/$LambdaT (&/$HostT "java.lang.Object" &/$Nil)
                                                       (&/$LambdaT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                                   (&/$HostT "java.lang.Boolean" &/$Nil))))

                            _ (&type/check (&/$LambdaT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                       (&/$LambdaT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                                   (&/$HostT "java.lang.Object" &/$Nil)))
                                           (&/$LambdaT (&/$HostT "java.lang.Object" &/$Nil)
                                                       (&/$LambdaT (&/$HostT "java.lang.Boolean" &/$Nil)
                                                                   (&/$HostT "java.lang.Boolean" &/$Nil))))
                            ]
                        (return nil))
                      (&/init-state nil))
    (&/$Right state nil)
    (is true)
    
    (&/$Left error)
    (is false error)
    ))

(deftest check-ex-types
  (|case (&/run-state (|do [_ (&type/check (&/$ExT 0) (&/$ExT 0))]
                        (return nil))
                      (&/init-state nil))
    (&/$Right state nil)
    (is true)
    
    (&/$Left error)
    (is false error)
    ))

(deftest check-univ-quantification
  (|case (&/run-state (|do [_ (&type/check (&/$UnivQ (&/|list)
                                                     (&/$LambdaT &/$VoidT (&/$BoundT 1)))
                                           (&/$UnivQ (&/|list)
                                                     (&/$LambdaT &/$VoidT (&/$BoundT 1))))

                            _ (&type/check (&/$UnivQ (&/|list)
                                                     (&/$SumT
                                                      ;; lux;None
                                                      &/$UnitT
                                                      ;; lux;Some
                                                      (&/$BoundT 1)))
                                           (&/$UnivQ (&/|list)
                                                     (&/$SumT
                                                      ;; lux;None
                                                      &/$UnitT
                                                      ;; lux;Some
                                                      (&/$BoundT 1))))

                            _ (&type/check (&/$UnivQ (&/|list)
                                                     (&/$SumT
                                                      ;; lux;Nil
                                                      &/$UnitT
                                                      ;; lux;Cons
                                                      (&/$ProdT (&/$BoundT 1)
                                                                (&/$AppT (&/$BoundT 0)
                                                                         (&/$BoundT 1)))))
                                           (&/$UnivQ (&/|list)
                                                     (&/$SumT
                                                      ;; lux;Nil
                                                      &/$UnitT
                                                      ;; lux;Cons
                                                      (&/$ProdT (&/$BoundT 1)
                                                                (&/$AppT (&/$BoundT 0)
                                                                         (&/$BoundT 1))))))]
                        (return nil))
                      (&/init-state nil))
    (&/$Right state nil)
    (is true)
    
    (&/$Left error)
    (is false error)
    ))

(deftest check-ex-quantification
  (|case (&/run-state (|do [_ (&type/check (&/$ExQ (&/|list)
                                                   (&/$LambdaT &/$VoidT (&/$BoundT 1)))
                                           (&/$ExQ (&/|list)
                                                   (&/$LambdaT &/$VoidT (&/$BoundT 1))))

                            _ (&type/check (&/$ExQ (&/|list)
                                                   (&/$SumT
                                                    ;; lux;None
                                                    &/$UnitT
                                                    ;; lux;Some
                                                    (&/$BoundT 1)))
                                           (&/$ExQ (&/|list)
                                                   (&/$SumT
                                                    ;; lux;None
                                                    &/$UnitT
                                                    ;; lux;Some
                                                    (&/$BoundT 1))))

                            _ (&type/check (&/$ExQ (&/|list)
                                                   (&/$SumT
                                                    ;; lux;Nil
                                                    &/$UnitT
                                                    ;; lux;Cons
                                                    (&/$ProdT (&/$BoundT 1)
                                                              (&/$AppT (&/$BoundT 0)
                                                                       (&/$BoundT 1)))))
                                           (&/$ExQ (&/|list)
                                                   (&/$SumT
                                                    ;; lux;Nil
                                                    &/$UnitT
                                                    ;; lux;Cons
                                                    (&/$ProdT (&/$BoundT 1)
                                                              (&/$AppT (&/$BoundT 0)
                                                                       (&/$BoundT 1))))))]
                        (return nil))
                      (&/init-state nil))
    (&/$Right state nil)
    (is true)
    
    (&/$Left error)
    (is false error)
    ))

(deftest check-app-type
  (|case (&/run-state (|do [_ (&type/check (&/$AppT (&/$UnivQ (&/|list)
                                                              (&/$LambdaT &/$VoidT (&/$BoundT 1)))
                                                    (&/$HostT "java.lang.Boolean" &/$Nil))
                                           (&/$AppT (&/$UnivQ (&/|list)
                                                              (&/$LambdaT &/$VoidT (&/$BoundT 1)))
                                                    (&/$HostT "java.lang.Boolean" &/$Nil)))

                            _ (&type/check (&/$AppT (&/$UnivQ (&/|list)
                                                              (&/$SumT
                                                               ;; lux;None
                                                               &/$UnitT
                                                               ;; lux;Some
                                                               (&/$BoundT 1)))
                                                    (&/$HostT "java.lang.Object" &/$Nil))
                                           (&/$AppT (&/$UnivQ (&/|list)
                                                              (&/$SumT
                                                               ;; lux;None
                                                               &/$UnitT
                                                               ;; lux;Some
                                                               (&/$BoundT 1)))
                                                    (&/$HostT "java.lang.Boolean" &/$Nil)))

                            _ (&type/check (&/$AppT (&/$ExQ (&/|list)
                                                            (&/$LambdaT &/$VoidT (&/$BoundT 1)))
                                                    (&/$HostT "java.lang.Boolean" &/$Nil))
                                           (&/$AppT (&/$ExQ (&/|list)
                                                            (&/$LambdaT &/$VoidT (&/$BoundT 1)))
                                                    (&/$HostT "java.lang.Boolean" &/$Nil)))

                            _ (&type/check (&/$AppT (&/$ExQ (&/|list)
                                                            (&/$SumT
                                                             ;; lux;None
                                                             &/$UnitT
                                                             ;; lux;Some
                                                             (&/$BoundT 1)))
                                                    (&/$HostT "java.lang.Object" &/$Nil))
                                           (&/$AppT (&/$ExQ (&/|list)
                                                            (&/$SumT
                                                             ;; lux;None
                                                             &/$UnitT
                                                             ;; lux;Some
                                                             (&/$BoundT 1)))
                                                    (&/$HostT "java.lang.Boolean" &/$Nil)))]
                        (return nil))
                      (&/init-state nil))
    (&/$Right state nil)
    (is true)
    
    (&/$Left error)
    (is false error)
    ))

(deftest check-var-type
  (|case (&/run-state (|do [_ (&type/with-var
                                (fn [$var]
                                  (|do [_ (&type/check $var (&/$HostT "java.lang.Boolean" &/$Nil))
                                        (&/$HostT "java.lang.Boolean" (&/$Nil)) (&type/deref+ $var)]
                                    (return nil))))

                            _ (&type/with-var
                                (fn [$var]
                                  (|do [_ (&type/check (&/$AppT (&/$UnivQ (&/|list)
                                                                          (&/$LambdaT &/$VoidT (&/$BoundT 1)))
                                                                $var)
                                                       (&/$AppT (&/$UnivQ (&/|list)
                                                                          (&/$LambdaT &/$VoidT (&/$BoundT 1)))
                                                                (&/$HostT "java.lang.Boolean" &/$Nil)))
                                        (&/$HostT "java.lang.Boolean" (&/$Nil)) (&type/deref+ $var)]
                                    (return nil))))

                            _ (&type/with-var
                                (fn [$var]
                                  (|do [_ (&type/check (&/$HostT "java.lang.Boolean" &/$Nil) $var)
                                        (&/$HostT "java.lang.Boolean" (&/$Nil)) (&type/deref+ $var)]
                                    (return nil))))

                            _ (&type/with-var
                                (fn [$var]
                                  (|do [_ (&type/check (&/$AppT (&/$UnivQ (&/|list)
                                                                          (&/$LambdaT &/$VoidT (&/$BoundT 1)))
                                                                (&/$HostT "java.lang.Boolean" &/$Nil))
                                                       (&/$AppT (&/$UnivQ (&/|list)
                                                                          (&/$LambdaT &/$VoidT (&/$BoundT 1)))
                                                                $var))
                                        (&/$HostT "java.lang.Boolean" (&/$Nil)) (&type/deref+ $var)]
                                    (return nil))))

                            _ (&type/with-var
                                (fn [$var1]
                                  (&type/with-var
                                    (fn [$var2]
                                      (|do [_ (&type/check $var1 $var2)]
                                        (return nil))))))

                            _ (&type/with-var
                                (fn [$var1]
                                  (&type/with-var
                                    (fn [$var2]
                                      (|do [_ (&type/check $var2 $var1)]
                                        (return nil))))))

                            _ (&type/with-var
                                (fn [$var1]
                                  (&type/with-var
                                    (fn [$var2]
                                      (|do [_ (&type/check $var1 $var2)
                                            _ (&type/check $var1 (&/$HostT "java.lang.Boolean" (&/|list)))
                                            =var1 (&type/deref+ $var1)
                                            _ (&/assert! (&type/type= =var1 $var2) "")
                                            =var2 (&type/deref+ $var2)
                                            _ (&/assert! (&type/type= =var2 (&/$HostT "java.lang.Boolean" (&/|list))) "")]
                                        (return nil))))))

                            _ (&type/with-var
                                (fn [$var1]
                                  (&type/with-var
                                    (fn [$var2]
                                      (|do [_ (&type/check $var2 $var1)
                                            _ (&type/check $var1 (&/$HostT "java.lang.Boolean" (&/|list)))
                                            =var2 (&type/deref+ $var2)
                                            _ (&/assert! (&type/type= =var2 $var1) "")
                                            =var1 (&type/deref+ $var1)
                                            _ (&/assert! (&type/type= =var1 (&/$HostT "java.lang.Boolean" (&/|list))) "")]
                                        (return nil))))))
                            
                            _ (&type/with-var
                                (fn [$var1]
                                  (&type/with-var
                                    (fn [$var2]
                                      (|do [_ (&type/check $var1 $var2)
                                            _ (&type/check $var2 (&/$HostT "java.lang.Boolean" (&/|list)))
                                            =var1 (&type/deref+ $var1)
                                            _ (&/assert! (&type/type= =var1 $var2) "")
                                            =var2 (&type/deref+ $var2)
                                            _ (&/assert! (&type/type= =var2 (&/$HostT "java.lang.Boolean" (&/|list))) "")]
                                        (return nil))))))

                            _ (&type/with-var
                                (fn [$var1]
                                  (&type/with-var
                                    (fn [$var2]
                                      (|do [_ (&type/check $var2 $var1)
                                            _ (&type/check $var2 (&/$HostT "java.lang.Boolean" (&/|list)))
                                            =var2 (&type/deref+ $var2)
                                            _ (&/assert! (&type/type= =var2 $var1) "")
                                            =var1 (&type/deref+ $var1)
                                            _ (&/assert! (&type/type= =var1 (&/$HostT "java.lang.Boolean" (&/|list))) "")]
                                        (return nil))))))]
                        (return nil))
                      (&/init-state nil))
    (&/$Right state nil)
    (is true)
    
    (&/$Left error)
    (is false error)
    ))

(comment
  (run-all-tests)
  )
