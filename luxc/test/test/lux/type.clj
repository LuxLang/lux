(ns test.lux.type
  (:use clojure.test)
  (:require (lux [base :as & :refer [|do return* return fail fail* |let |case]]
                 [type :as &type])
            :reload-all
            ))

;; [Tests]
(deftest check-base-types
  (|case (&/run-state (|do [_ (&type/check &/$Unit &/$Unit)

                            _ (&type/check &/$Void &/$Void)]
                        (return nil))
                      (&/init-state nil))
    (&/$Right state nil)
    (is true)
    
    (&/$Left error)
    (is false error)
    ))

(deftest check-simple-host-types
  (|case (&/run-state (|do [_ (&type/check (&/$Host "java.lang.Boolean" &/$Nil)
                                           (&/$Host "java.lang.Boolean" &/$Nil))
                            
                            _ (&type/check (&/$Host "java.lang.Object" &/$Nil)
                                           (&/$Host "java.lang.Boolean" &/$Nil))]
                        (return nil))
                      (&/init-state nil))
    (&/$Right state nil)
    (is true)
    
    (&/$Left error)
    (is false error)
    ))

(deftest check-complex-host-types
  (|case (&/run-state (|do [_ (&type/check (&/$Host "java.util.List" (&/|list (&/$Host "java.lang.Boolean" &/$Nil)))
                                           (&/$Host "java.util.List" (&/|list (&/$Host "java.lang.Boolean" &/$Nil))))
                            
                            _ (&type/check (&/$Host "java.util.List" (&/|list (&/$Host "java.lang.Object" &/$Nil)))
                                           (&/$Host "java.util.List" (&/|list (&/$Host "java.lang.Boolean" &/$Nil))))

                            _ (&type/check (&/$Host "java.util.List" (&/|list (&/$Host "java.lang.Boolean" &/$Nil)))
                                           (&/$Host "java.util.ArrayList" (&/|list (&/$Host "java.lang.Boolean" &/$Nil))))]
                        (return nil))
                      (&/init-state nil))
    (&/$Right state nil)
    (is true)
    
    (&/$Left error)
    (is false error)
    ))

(deftest check-named-types
  (|case (&/run-state (|do [_ (&type/check (&/$Named (&/T ["lux" "Bool"]) (&/$Host "java.lang.Boolean" &/$Nil))
                                           (&/$Host "java.lang.Boolean" &/$Nil))
                            
                            _ (&type/check (&/$Host "java.lang.Boolean" &/$Nil)
                                           (&/$Named (&/T ["lux" "Bool"]) (&/$Host "java.lang.Boolean" &/$Nil)))
                            
                            _ (&type/check (&/$Named (&/T ["lux" "Bool"]) (&/$Host "java.lang.Boolean" &/$Nil))
                                           (&/$Named (&/T ["lux" "Bool"]) (&/$Host "java.lang.Boolean" &/$Nil)))]
                        (return nil))
                      (&/init-state nil))
    (&/$Right state nil)
    (is true)
    
    (&/$Left error)
    (is false error)
    ))

(deftest check-sum-types
  (|case (&/run-state (|do [_ (&type/check (&/$Sum (&/$Host "java.lang.Boolean" &/$Nil)
                                                   (&/$Host "java.lang.Boolean" &/$Nil))
                                           (&/$Sum (&/$Host "java.lang.Boolean" &/$Nil)
                                                   (&/$Host "java.lang.Boolean" &/$Nil)))

                            _ (&type/check (&/$Sum (&/$Host "java.lang.Object" &/$Nil)
                                                   (&/$Host "java.lang.Boolean" &/$Nil))
                                           (&/$Sum (&/$Host "java.lang.Boolean" &/$Nil)
                                                   (&/$Host "java.lang.Boolean" &/$Nil)))

                            _ (&type/check (&/$Sum (&/$Host "java.lang.Object" &/$Nil)
                                                   (&/$Host "java.lang.Object" &/$Nil))
                                           (&/$Sum (&/$Host "java.lang.Boolean" &/$Nil)
                                                   (&/$Host "java.lang.Boolean" &/$Nil)))

                            _ (&type/check (&/$Sum (&/$Host "java.lang.Boolean" &/$Nil)
                                                   (&/$Sum (&/$Host "java.lang.Boolean" &/$Nil)
                                                           (&/$Host "java.lang.Boolean" &/$Nil)))
                                           (&/$Sum (&/$Host "java.lang.Boolean" &/$Nil)
                                                   (&/$Sum (&/$Host "java.lang.Boolean" &/$Nil)
                                                           (&/$Host "java.lang.Boolean" &/$Nil))))

                            _ (&type/check (&/$Sum (&/$Host "java.lang.Object" &/$Nil)
                                                   (&/$Sum (&/$Host "java.lang.Boolean" &/$Nil)
                                                           (&/$Host "java.lang.Boolean" &/$Nil)))
                                           (&/$Sum (&/$Host "java.lang.Boolean" &/$Nil)
                                                   (&/$Sum (&/$Host "java.lang.Boolean" &/$Nil)
                                                           (&/$Host "java.lang.Boolean" &/$Nil))))

                            _ (&type/check (&/$Sum (&/$Host "java.lang.Object" &/$Nil)
                                                   (&/$Sum (&/$Host "java.lang.Boolean" &/$Nil)
                                                           (&/$Host "java.lang.Object" &/$Nil)))
                                           (&/$Sum (&/$Host "java.lang.Boolean" &/$Nil)
                                                   (&/$Sum (&/$Host "java.lang.Boolean" &/$Nil)
                                                           (&/$Host "java.lang.Boolean" &/$Nil))))]
                        (return nil))
                      (&/init-state nil))
    (&/$Right state nil)
    (is true)
    
    (&/$Left error)
    (is false error)
    ))

(deftest check-prod-types
  (|case (&/run-state (|do [_ (&type/check (&/$Product (&/$Host "java.lang.Boolean" &/$Nil)
                                                       (&/$Host "java.lang.Boolean" &/$Nil))
                                           (&/$Product (&/$Host "java.lang.Boolean" &/$Nil)
                                                       (&/$Host "java.lang.Boolean" &/$Nil)))

                            _ (&type/check (&/$Product (&/$Host "java.lang.Object" &/$Nil)
                                                       (&/$Host "java.lang.Boolean" &/$Nil))
                                           (&/$Product (&/$Host "java.lang.Boolean" &/$Nil)
                                                       (&/$Host "java.lang.Boolean" &/$Nil)))

                            _ (&type/check (&/$Product (&/$Host "java.lang.Object" &/$Nil)
                                                       (&/$Host "java.lang.Object" &/$Nil))
                                           (&/$Product (&/$Host "java.lang.Boolean" &/$Nil)
                                                       (&/$Host "java.lang.Boolean" &/$Nil)))

                            _ (&type/check (&/$Product (&/$Host "java.lang.Boolean" &/$Nil)
                                                       (&/$Product (&/$Host "java.lang.Boolean" &/$Nil)
                                                                   (&/$Host "java.lang.Boolean" &/$Nil)))
                                           (&/$Product (&/$Host "java.lang.Boolean" &/$Nil)
                                                       (&/$Product (&/$Host "java.lang.Boolean" &/$Nil)
                                                                   (&/$Host "java.lang.Boolean" &/$Nil))))

                            _ (&type/check (&/$Product (&/$Host "java.lang.Object" &/$Nil)
                                                       (&/$Product (&/$Host "java.lang.Boolean" &/$Nil)
                                                                   (&/$Host "java.lang.Boolean" &/$Nil)))
                                           (&/$Product (&/$Host "java.lang.Boolean" &/$Nil)
                                                       (&/$Product (&/$Host "java.lang.Boolean" &/$Nil)
                                                                   (&/$Host "java.lang.Boolean" &/$Nil))))

                            _ (&type/check (&/$Product (&/$Host "java.lang.Object" &/$Nil)
                                                       (&/$Product (&/$Host "java.lang.Boolean" &/$Nil)
                                                                   (&/$Host "java.lang.Object" &/$Nil)))
                                           (&/$Product (&/$Host "java.lang.Boolean" &/$Nil)
                                                       (&/$Product (&/$Host "java.lang.Boolean" &/$Nil)
                                                                   (&/$Host "java.lang.Boolean" &/$Nil))))]
                        (return nil))
                      (&/init-state nil))
    (&/$Right state nil)
    (is true)
    
    (&/$Left error)
    (is false error)
    ))

(deftest check-lambda-types
  (|case (&/run-state (|do [_ (&type/check (&/$Lambda (&/$Host "java.lang.Boolean" &/$Nil)
                                                      (&/$Host "java.lang.Boolean" &/$Nil))
                                           (&/$Lambda (&/$Host "java.lang.Boolean" &/$Nil)
                                                      (&/$Host "java.lang.Boolean" &/$Nil)))

                            _ (&type/check (&/$Lambda (&/$Host "java.lang.Boolean" &/$Nil)
                                                      (&/$Host "java.lang.Boolean" &/$Nil))
                                           (&/$Lambda (&/$Host "java.lang.Object" &/$Nil)
                                                      (&/$Host "java.lang.Boolean" &/$Nil)))

                            _ (&type/check (&/$Lambda (&/$Host "java.lang.Boolean" &/$Nil)
                                                      (&/$Host "java.lang.Object" &/$Nil))
                                           (&/$Lambda (&/$Host "java.lang.Object" &/$Nil)
                                                      (&/$Host "java.lang.Boolean" &/$Nil)))
                            
                            _ (&type/check (&/$Lambda (&/$Host "java.lang.Boolean" &/$Nil)
                                                      (&/$Lambda (&/$Host "java.lang.Boolean" &/$Nil)
                                                                 (&/$Host "java.lang.Boolean" &/$Nil)))
                                           (&/$Lambda (&/$Host "java.lang.Boolean" &/$Nil)
                                                      (&/$Lambda (&/$Host "java.lang.Boolean" &/$Nil)
                                                                 (&/$Host "java.lang.Boolean" &/$Nil))))

                            _ (&type/check (&/$Lambda (&/$Host "java.lang.Boolean" &/$Nil)
                                                      (&/$Lambda (&/$Host "java.lang.Boolean" &/$Nil)
                                                                 (&/$Host "java.lang.Boolean" &/$Nil)))
                                           (&/$Lambda (&/$Host "java.lang.Object" &/$Nil)
                                                      (&/$Lambda (&/$Host "java.lang.Boolean" &/$Nil)
                                                                 (&/$Host "java.lang.Boolean" &/$Nil))))

                            _ (&type/check (&/$Lambda (&/$Host "java.lang.Boolean" &/$Nil)
                                                      (&/$Lambda (&/$Host "java.lang.Boolean" &/$Nil)
                                                                 (&/$Host "java.lang.Object" &/$Nil)))
                                           (&/$Lambda (&/$Host "java.lang.Object" &/$Nil)
                                                      (&/$Lambda (&/$Host "java.lang.Boolean" &/$Nil)
                                                                 (&/$Host "java.lang.Boolean" &/$Nil))))
                            ]
                        (return nil))
                      (&/init-state nil))
    (&/$Right state nil)
    (is true)
    
    (&/$Left error)
    (is false error)
    ))

(deftest check-ex-types
  (|case (&/run-state (|do [_ (&type/check (&/$Ex 0) (&/$Ex 0))]
                        (return nil))
                      (&/init-state nil))
    (&/$Right state nil)
    (is true)
    
    (&/$Left error)
    (is false error)
    ))

(deftest check-univ-quantification
  (|case (&/run-state (|do [_ (&type/check (&/$UnivQ (&/|list)
                                                     (&/$Lambda &/$Void (&/$Bound 1)))
                                           (&/$UnivQ (&/|list)
                                                     (&/$Lambda &/$Void (&/$Bound 1))))

                            _ (&type/check (&/$UnivQ (&/|list)
                                                     (&/$Sum
                                                      ;; lux;None
                                                      &/$Unit
                                                      ;; lux;Some
                                                      (&/$Bound 1)))
                                           (&/$UnivQ (&/|list)
                                                     (&/$Sum
                                                      ;; lux;None
                                                      &/$Unit
                                                      ;; lux;Some
                                                      (&/$Bound 1))))

                            _ (&type/check (&/$UnivQ (&/|list)
                                                     (&/$Sum
                                                      ;; lux;Nil
                                                      &/$Unit
                                                      ;; lux;Cons
                                                      (&/$Product (&/$Bound 1)
                                                                  (&/$App (&/$Bound 0)
                                                                          (&/$Bound 1)))))
                                           (&/$UnivQ (&/|list)
                                                     (&/$Sum
                                                      ;; lux;Nil
                                                      &/$Unit
                                                      ;; lux;Cons
                                                      (&/$Product (&/$Bound 1)
                                                                  (&/$App (&/$Bound 0)
                                                                          (&/$Bound 1))))))]
                        (return nil))
                      (&/init-state nil))
    (&/$Right state nil)
    (is true)
    
    (&/$Left error)
    (is false error)
    ))

(deftest check-ex-quantification
  (|case (&/run-state (|do [_ (&type/check (&/$ExQ (&/|list)
                                                   (&/$Lambda &/$Void (&/$Bound 1)))
                                           (&/$ExQ (&/|list)
                                                   (&/$Lambda &/$Void (&/$Bound 1))))

                            _ (&type/check (&/$ExQ (&/|list)
                                                   (&/$Sum
                                                    ;; lux;None
                                                    &/$Unit
                                                    ;; lux;Some
                                                    (&/$Bound 1)))
                                           (&/$ExQ (&/|list)
                                                   (&/$Sum
                                                    ;; lux;None
                                                    &/$Unit
                                                    ;; lux;Some
                                                    (&/$Bound 1))))

                            _ (&type/check (&/$ExQ (&/|list)
                                                   (&/$Sum
                                                    ;; lux;Nil
                                                    &/$Unit
                                                    ;; lux;Cons
                                                    (&/$Product (&/$Bound 1)
                                                                (&/$App (&/$Bound 0)
                                                                        (&/$Bound 1)))))
                                           (&/$ExQ (&/|list)
                                                   (&/$Sum
                                                    ;; lux;Nil
                                                    &/$Unit
                                                    ;; lux;Cons
                                                    (&/$Product (&/$Bound 1)
                                                                (&/$App (&/$Bound 0)
                                                                        (&/$Bound 1))))))]
                        (return nil))
                      (&/init-state nil))
    (&/$Right state nil)
    (is true)
    
    (&/$Left error)
    (is false error)
    ))

(deftest check-app-type
  (|case (&/run-state (|do [_ (&type/check (&/$App (&/$UnivQ (&/|list)
                                                             (&/$Lambda &/$Void (&/$Bound 1)))
                                                   (&/$Host "java.lang.Boolean" &/$Nil))
                                           (&/$App (&/$UnivQ (&/|list)
                                                             (&/$Lambda &/$Void (&/$Bound 1)))
                                                   (&/$Host "java.lang.Boolean" &/$Nil)))

                            _ (&type/check (&/$App (&/$UnivQ (&/|list)
                                                             (&/$Sum
                                                              ;; lux;None
                                                              &/$Unit
                                                              ;; lux;Some
                                                              (&/$Bound 1)))
                                                   (&/$Host "java.lang.Object" &/$Nil))
                                           (&/$App (&/$UnivQ (&/|list)
                                                             (&/$Sum
                                                              ;; lux;None
                                                              &/$Unit
                                                              ;; lux;Some
                                                              (&/$Bound 1)))
                                                   (&/$Host "java.lang.Boolean" &/$Nil)))

                            _ (&type/check (&/$App (&/$ExQ (&/|list)
                                                           (&/$Lambda &/$Void (&/$Bound 1)))
                                                   (&/$Host "java.lang.Boolean" &/$Nil))
                                           (&/$App (&/$ExQ (&/|list)
                                                           (&/$Lambda &/$Void (&/$Bound 1)))
                                                   (&/$Host "java.lang.Boolean" &/$Nil)))

                            _ (&type/check (&/$App (&/$ExQ (&/|list)
                                                           (&/$Sum
                                                            ;; lux;None
                                                            &/$Unit
                                                            ;; lux;Some
                                                            (&/$Bound 1)))
                                                   (&/$Host "java.lang.Object" &/$Nil))
                                           (&/$App (&/$ExQ (&/|list)
                                                           (&/$Sum
                                                            ;; lux;None
                                                            &/$Unit
                                                            ;; lux;Some
                                                            (&/$Bound 1)))
                                                   (&/$Host "java.lang.Boolean" &/$Nil)))]
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
                                  (|do [_ (&type/check $var (&/$Host "java.lang.Boolean" &/$Nil))
                                        (&/$Host "java.lang.Boolean" (&/$Nil)) (&type/deref+ $var)]
                                    (return nil))))

                            _ (&type/with-var
                                (fn [$var]
                                  (|do [_ (&type/check (&/$App (&/$UnivQ (&/|list)
                                                                         (&/$Lambda &/$Void (&/$Bound 1)))
                                                               $var)
                                                       (&/$App (&/$UnivQ (&/|list)
                                                                         (&/$Lambda &/$Void (&/$Bound 1)))
                                                               (&/$Host "java.lang.Boolean" &/$Nil)))
                                        (&/$Host "java.lang.Boolean" (&/$Nil)) (&type/deref+ $var)]
                                    (return nil))))

                            _ (&type/with-var
                                (fn [$var]
                                  (|do [_ (&type/check (&/$Host "java.lang.Boolean" &/$Nil) $var)
                                        (&/$Host "java.lang.Boolean" (&/$Nil)) (&type/deref+ $var)]
                                    (return nil))))

                            _ (&type/with-var
                                (fn [$var]
                                  (|do [_ (&type/check (&/$App (&/$UnivQ (&/|list)
                                                                         (&/$Lambda &/$Void (&/$Bound 1)))
                                                               (&/$Host "java.lang.Boolean" &/$Nil))
                                                       (&/$App (&/$UnivQ (&/|list)
                                                                         (&/$Lambda &/$Void (&/$Bound 1)))
                                                               $var))
                                        (&/$Host "java.lang.Boolean" (&/$Nil)) (&type/deref+ $var)]
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
                                            _ (&type/check $var1 (&/$Host "java.lang.Boolean" (&/|list)))
                                            =var1 (&type/deref+ $var1)
                                            _ (&/assert! (&type/type= =var1 $var2) "")
                                            =var2 (&type/deref+ $var2)
                                            _ (&/assert! (&type/type= =var2 (&/$Host "java.lang.Boolean" (&/|list))) "")]
                                        (return nil))))))

                            _ (&type/with-var
                                (fn [$var1]
                                  (&type/with-var
                                    (fn [$var2]
                                      (|do [_ (&type/check $var2 $var1)
                                            _ (&type/check $var1 (&/$Host "java.lang.Boolean" (&/|list)))
                                            =var2 (&type/deref+ $var2)
                                            _ (&/assert! (&type/type= =var2 $var1) "")
                                            =var1 (&type/deref+ $var1)
                                            _ (&/assert! (&type/type= =var1 (&/$Host "java.lang.Boolean" (&/|list))) "")]
                                        (return nil))))))
                            
                            _ (&type/with-var
                                (fn [$var1]
                                  (&type/with-var
                                    (fn [$var2]
                                      (|do [_ (&type/check $var1 $var2)
                                            _ (&type/check $var2 (&/$Host "java.lang.Boolean" (&/|list)))
                                            =var1 (&type/deref+ $var1)
                                            _ (&/assert! (&type/type= =var1 $var2) "")
                                            =var2 (&type/deref+ $var2)
                                            _ (&/assert! (&type/type= =var2 (&/$Host "java.lang.Boolean" (&/|list))) "")]
                                        (return nil))))))

                            _ (&type/with-var
                                (fn [$var1]
                                  (&type/with-var
                                    (fn [$var2]
                                      (|do [_ (&type/check $var2 $var1)
                                            _ (&type/check $var2 (&/$Host "java.lang.Boolean" (&/|list)))
                                            =var2 (&type/deref+ $var2)
                                            _ (&/assert! (&type/type= =var2 $var1) "")
                                            =var1 (&type/deref+ $var1)
                                            _ (&/assert! (&type/type= =var1 (&/$Host "java.lang.Boolean" (&/|list))) "")]
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
