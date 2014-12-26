(ns lang
  (:require (lang [lexer :as &lexer]
                  [parser :as &parser]
                  [type :as &type]
                  [analyser :as &analyser]
                  [compiler :as &compiler])
            :reload))

(defn write-file [file data]
  (with-open [stream (java.io.BufferedOutputStream. (java.io.FileOutputStream. file))]
    ;; (prn 'write-file 'file file 'stream stream 'data data)
    (.write stream data)))

(comment
  ;; TODO: Add macros.
  ;; TODO: Re-implement compiler in language.
  ;; TODO: Add signatures & structures OR type-classes.
  ;; TODO: Add type-level computations.
  ;; TODO: Add thunks.
  ;; TODO: Do tail-call optimization.
  ;; TODO: Adding metadata to global vars.
  ;; TODO: Add records.
  ;; TODO: throw, try, catch, finally
  ;; TODO: Finish implementing pattern matching.
  ;; TODO: Tuple8 and Tuple8X (for arbitrary-size tuples).
  ;; TODO: Add extra arities (apply2, apply3, ..., apply16)
  ;; TODO: When doing partial application, skip "apply" and just call constructor appropiatedly.
  ;; TODO: Add "new". Allow setting fields.
  ;; TODO: Don't take into account newlines in strings unless they come from \n to allow better coding.
  ;; TODO: 
  ;; TODO: 
  ;; TODO: 
  ;; TODO: 

  (let [test '([:lang.parser/case-branch [:lang.parser/variant "Nil" ()]
                [:lang.parser/ident "yx"]]
                 [:lang.parser/case-branch [:lang.parser/variant "Cons" ([:lang.parser/ident "x"] [:lang.parser/ident "xs*"])]
                  [:lang.parser/variant "Cons" ([:lang.parser/ident "x"] [:lang.parser/fn-call [:lang.parser/ident "++"] ([:lang.parser/ident "xs*"] [:lang.parser/ident "ys"])])]])
        convert (fn [cases]
                  (list (reduce (fn [acc [_ shape body]]
                                  (clojure.core.match/match shape
                                    [::&parser/variant ?tag ?elems]
                                    (let [=elems (map (fn [elem]
                                                        (clojure.core.match/match elem
                                                          [::&parser/ident ?ident]
                                                          [::ident ?ident]))
                                                      ?elems)]
                                      (conj acc [?tag =elems body]))))
                                []
                                cases)))]
    (convert test))

  (enumerate (list (list '["Nil" [] branch-0]
                         '["Cons" [x xs*] branch-1])))
  
  
  (let [source-code (slurp "test2.lang")
        tokens (&lexer/lex source-code)
        ;; _ (prn 'tokens tokens)
        syntax (&parser/parse tokens)
        ;; _ (prn 'syntax syntax)
        ann-syntax (&analyser/analyse "test2" syntax)
        ;; _ (prn 'ann-syntax ann-syntax)
        class-data (&compiler/compile "test2" ann-syntax)
        ;; _ (prn 'class-data class-data)
        ]
    (write-file "test2.class" class-data))

  ;; jar cvf test2.jar *.class test2 && java -cp "test2.jar" test2
  )

(comment

  (let [data '([::&parser/case-branch [::&parser/variant "Nil" ()]
                [::&parser/ident "ys"]]
                 [::&parser/case-branch [::&parser/variant "Cons" ([::&parser/ident "x"] [::&parser/ident "xs*"])]
                  [::&parser/variant "Cons" ([::&parser/ident "x"] [::&parser/fn-call [::&parser/ident "++"] ([::&parser/ident "xs*"] [::&parser/ident "ys"])])]])
        ;; count-registers (fn count-registers [pattern]
        ;;                   (clojure.core.match/match pattern
        ;;                     [::&parser/ident _]
        ;;                     0
        
        ;;                     [::&parser/variant _ ?members]
        ;;                     (reduce + (count ?members) (map count-registers ?members))))
        gen-impl (fn gen-impl [offset pattern]
                   (clojure.core.match/match pattern
                     [::&parser/ident _]
                     [1 [::case-bind -1 offset]]
                     
                     [::&parser/variant ?tag ?members]
                     (let [regs+insns (mapv (fn [idx member]
                                              (clojure.core.match/match member
                                                [::&parser/ident _]
                                                [1 [::case-sub-bind (+ offset (inc idx))]]))
                                            (range (count ?members))
                                            ?members)]
                       [(reduce + 1 (map first regs+insns)) [::case-try ?tag (mapv second regs+insns)]])
                     (reduce + (count ?members) (map gen-impl ?members))))]
    (reduce (fn [accum branch]
              (clojure.core.match/match branch
                [::&parser/case-branch ?pattern ?body]
                (clojure.core.match/match ?pattern
                  [::&parser/variant ?tag ?members]
                  (let [[extra-registers impl] (gen-impl 0 ?pattern)
                        _ (prn 'impl extra-registers impl)
                        $branch (get-in accum [:paths :total])]
                    (-> accum
                        (update-in [:patterns]
                                   (fn [patterns]
                                     (if (contains? patterns ?tag)
                                       (if (= (get patterns [?tag :arity]) (count ?members))
                                         (update-in patterns [?tag :branches] conj {:test impl
                                                                                    :link $branch})
                                         (assert "Pattern arity doesn't match!"))
                                       (assoc patterns ?tag {:arity (count ?members)
                                                             :branches [{:test impl
                                                                         :link $branch}]}))))
                        (update-in [:paths]
                                   (fn [paths]
                                     (-> paths
                                         (update-in [:total] inc)
                                         (assoc-in [:links $branch] ?body))))
                        (update-in [:registers] + (dec extra-registers)))))
                ))
            {:registers 1
             :patterns {}
             :paths {:total 0
                     :links {}}}
            data))

  '([::&parser/case-branch [::&parser/variant "Nil" ()]
     [::&parser/ident "ys"]]
    [::&parser/case-branch [::&parser/variant "Cons" ([::&parser/ident "x"] [::&parser/ident "xs*"])]
     [::&parser/variant "Cons" ([::&parser/ident "x"] [::&parser/fn-call [::&parser/ident "++"] ([::&parser/ident "xs*"] [::&parser/ident "ys"])])]])

  '([:try "Nil" []]
    [:try "Cons" [[:bind 0 1] [:bind 1 2]]])
  
  (list '["Nil" [] branch-0]
        '["Cons" [x xs*] branch-1])

  [:if [% tag "Nil"]
   branch-0
   [:let [%0 %1]
    branch-1]]
  
  (let [enumerate (fn [xs] (map vector (range (count xs)) xs))
        cases (enumerate (list (list '["Nil" [] branch-0]
                                     '["Cons" [x xs*] branch-1])))
        classify-cases (fn [[idx cases]]
                         [idx (reduce (fn [order [tag members branch]]
                                        (if-let [{:keys [arity branches] :as sub-struct} (get order tag)]
                                          (if (= arity (count members))
                                            (update-in order [tag :branches] conj [members branch])
                                            (assert (str "Arity doesn't match:" (count members) " != " arity)))
                                          (assoc order tag {:arity (count members)
                                                            :branches (vector [members branch])})))
                                      {}
                                      cases)])
        ;; case->struct (fn [cases]
        ;;                (let [struct (classify-case cases)
        ;;                      struct* (seq struct)]
        ;;                  (reduce (fn [inner [tag {:keys [arity branches]}]]
        ;;                            [:if [% tag "Nil"]
        ;;                             branch-0
        ;;                             inner])
        ;;                          (second (last struct*))
        ;;                          (butlast struct*))
        ;;                  ))
        ]
    ;; (classify-case cases)
    (let [;; separated (apply map list cases)
          classifications (map classify-cases cases)
          classifications* (sort-by first > classifications)]
      ((fn [[idx struct]]
         (prn idx struct)
         (if-let [default (get struct nil)]
           (reduce (fn [[dbinds dbranch] [tag [binds branch]]]
                     [:if tag
                      [:let binds
                       branch]
                      [:let dbinds
                       dbranch]])
                   (-> default :branches first)
                   (seq struct))
           (let [struct* (seq struct)]
             (reduce (fn [[dbinds dbranch] [tag sub-struct]]
                       (let [[binds branch] (-> sub-struct :branches first)]
                         [:if tag
                          [:let binds
                           branch]
                          [:let dbinds
                           dbranch]]))
                     (-> struct* last second :branches first)
                     (butlast struct*)))))
       (first classifications*))
      ))

  ;; ([0 {"Cons" {:arity 2, :branches [[[x xs*] branch-1]]}, "Nil" {:arity 0, :branches [[[] branch-0]]}}])
  

  ;; {"Cons" {:arity 2, :branches [[[x xs*] branch-1]]},
  ;;  "Nil" {:arity 0, :branches [[[] branch-0]]}}


  ;; .........................

  ;; (case elems
  ;;   #Nil
  ;;   elems

  ;;   (#Cons head tail)
  ;;   (case head
  ;;     (#Cons (#Symbol "~") (#Cons unquoted #Nil))
  ;;     (#Cons unquoted (template tail))

  ;;     (#Cons (#Symbol "~@") (#Cons spliced #Nil))
  ;;     (#Cons (#Symbol "++") (#Cons spliced (template tail)))

  ;;     _
  ;;     (#Cons head (template tail)))
  ;;   )

  ;; Total registers: 3
  ;; [{:tag "Nil" :data [] :path path-1}
  ;;  {:tag "Cons" :data [[:bind head] [:bind tail]] :path path-2}]

  ;; {path-0 [:branch "Nil" []]
  ;;  path-1 [:branch "Cons" [[:bind head] [:bind tail]]]}

  ;; Total registers: 6
  ;; {path-0 [:branch "Cons" [[:adt "Symbol" [[:string-cmp "~"]]] [:adt "Cons" [[:bind unquoted] [:adt "Nil" []]]]]]
  ;;  path-1 [:branch "Cons" [[:adt "Symbol" [[:string-cmp "~@"]]] [:adt "Cons" [[:bind spliced] [:adt "Nil" []]]]]]
  ;;  path-2 [:default _]}

  ;; {"#default#" [:default path-2 _]
  ;;  "Cons" [:branches [path-0 path-1]
  ;;          [[[:adt "Symbol" [[:string-cmp "~"]]] [:adt "Cons" [[:bind unquoted] [:adt "Nil" []]]]]
  ;;           [[:adt "Symbol" [[:string-cmp "~@"]]] [:adt "Cons" [[:bind spliced] [:adt "Nil" []]]]]]]}

  ;; [:branches]
  

  ;; (case elems
  ;;   #Nil
  ;;   elems

  ;;   (#Cons (list (' ~) unquoted) tail)
  ;;   (list* unquoted (template tail))

  ;;   (#Cons (list (' ~@) spliced) tail)
  ;;   (list* "++" spliced (template tail))

  ;;   _
  ;;   (#Cons head (template tail))
  ;;   )

  ;; [{:tag "Nil" :data [] :path path-1}
  ;;  {:tag "Cons" :data [[:bind head] [:bind tail]] :path path-2}]

  ;; [[{:tag "Cons" :data [%0 %1] :path nil
  ;;    :sub-cases [[{:tag "Symbol" :data [[:string-cmp "~"]] :path path-1}
  ;;                 {:tag "Symbol" :data [[:string-cmp "~@"]] :path path-2}]
  ;;                [{:tag "Cons" :data [[:bind unquoted] {:tag "Nil" :data []}] :path path-1}
  ;;                 {:tag "Cons" :data [[:bind spliced] {:tag "Nil" :data []}] :path path-2}]]}
  ;;   {:tag ::default :path path-3}]]

  ;; [[["Cons" ["Symbol" "~"] ["Cons" unquoted ["Nil"]]]
  ;;   ["Cons" ["Symbol" "~@"] ["Cons" spliced ["Nil"]]]
  ;;   _]]

  ;; [[["Symbol" "~"]
  ;;   ["Symbol" "~@"]]
  ;;  [["Cons" unquoted ["Nil"]]
  ;;   ["Cons" spliced ["Nil"]]]]

  ;; (if (= "Cons" (:: % tag))
  ;;   (let [%0 (:: % _0)
  ;;         %1 (:: % _1)]
  ;;     (if (= "Symbol" (:: %0 tag))
  ;;       (let [%0|0 (:: %0 _0)]
  ;;         (if (= "~" %0|0)
  ;;           (if (= "Cons" (:: %1 tag))
  ;;             (let [%1|0 (:: %1 _0)
  ;;                   %1|1 (:: %1 _1)]
  ;;               (if (= "Nil" (:: %1|1 tag))
  ;;                 (let [unquoted %1|0]
  ;;                   <path-1>)
  ;;                 <path-3>))
  ;;             <path-3>)
  ;;           (if (= "@~" %0|0)
  ;;             (if (= "Cons" (:: %1 tag))
  ;;               (let [%1|0 (:: %1 _0)
  ;;                     %1|1 (:: %1 _1)]
  ;;                 (if (= "Nil" (:: %1|1 tag))
  ;;                   (let [unquoted %1|0]
  ;;                     <path-2>)
  ;;                   <path-3>))
  ;;               <path-3>)
  ;;             <path-3>)))
  ;;       <path-3>))
  ;;   <path-3>)

  

  ;; (list (list '["Nil" [] ...]
  ;;             '["Cons" [head tail] ...]))

  ;; (list (list '["Cons" [["Symbol" ["~"]] ["Cons" [unquoted ["Nil" []]]]] ...]
  ;;             '["Cons" [["Symbol" ["~@"]] ["Cons" [spliced ["Nil" []]]]] ...]))
  )

