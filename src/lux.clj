(ns lux
  (:require (lux [lexer :as &lexer]
                 [parser :as &parser]
                 [type :as &type]
                 [analyser :as &analyser]
                 [compiler :as &compiler])
            :reload))

(comment
  ;; TODO: Make macros monadic.
  ;; TODO: Finish type system.
  ;; TODO: Re-implement compiler in language.
  ;; TODO: Add signatures & structures OR type-classes.
  ;; TODO: Add type-level computations.
  ;; TODO: Add thunks.
  ;; TODO: Do tail-call optimization.
  ;; TODO: Adding metadata to global vars.
  ;; TODO: Add records.
  ;; TODO: throw, try, catch, finally
  ;; TODO: Add extra arities (apply2, apply3, ..., apply16)
  ;; TODO: Allow setting fields.
  ;; TODO: monitor enter & monitor exit.
  ;; TODO: Reinplement "if" as a macro on top of case.
  ;; TODO: Remember to optimized calling global functions.
  ;; TODO: Reader macros.
  ;; TODO: 
  ;; TODO: 
  ;; TODO: 
  ;; TODO: 
  
  (time (&compiler/compile-all ["lux" ;; "test2"
                                ]))

  

  (deftype (Session c p s)
      (-> (-> p s c) c))
  
  ;; (: bind (All [m a b]
  ;;              (-> (-> a (m b)) (m a) (m b))))
  
  (do (defn >> [v]
        (fn [session]
          (session v)))

    (defn >> [v]
      (client v (fn [_ client*]
                  (k _ client*))))

    (def <<
      (server nil (fn [v server*]
                    (k v server*))))

    (defn pipe [])

    (<< (fn [x server*]
          (server* nil (fn [y server**]
                         (server** (+ x y) k)))))

    (def (select' k)
      (lambda [msg session]
         (session nil (k msg))))

    (def (choose choice)
      (lambda [msg session]
         (session choice ...)))

    (def <<
      (lambda [next peer]
         (peer [] (lambda [x peer']
                     (next x peer')))))

    (def (>> x)
      (lambda [next peer]
         (peer x (lambda [_ peer']
                    (next [] peer')))))
    
    (def server
      (loop [_ []]
        (select #Add
                (do [x <<
                     y <<
                     _ (>> (+ x y))]
                  (recur []))
                
                #Neg
                (do [x <<
                     _ (>> (neg x))]
                  (recur []))
                
                #Quit
                end)))

    (def client
      (do [_ (choose #Add)
           _ (>> 5)
           _ (>> 10)
           x+y <<]
        (choose #Quit)))
    
    (def <END>
      (fn [session]
        nil))

    (bind << (fn [x]
               (bind << (fn [y]
                          (>> (+ x y))))))

    (do [x <<
         y <<]
      (>> (+ x y)))
    
    (defn <$> [consumer producer init]
      (let [[x producer*] (producer init)
            [y consumer*] (consumer x)]
        [consumer* producer* y]))

    ((<$> (<< <END>) ((>> 5) <END>)))
    )
  
  
  ;; jar cvf test2.jar *.class test2 && java -cp "test2.jar" test2
  ;; cd output && jar cvf test2.jar * && java -cp "test2.jar" test2 && cd ..
  )

;; (def (workday? d)
;;   (case d
;;     (or [#Monday #Tuesday #Wednesday #Thursday #Friday]
;;         true)
;;     (or [#Saturday #Sunday]
;;         false)))
