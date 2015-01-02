(ns lang
  (:require (lang [lexer :as &lexer]
                  [parser :as &parser]
                  [type :as &type]
                  [analyser :as &analyser]
                  [compiler :as &compiler])
            :reload))

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
    (with-open [stream (java.io.BufferedOutputStream. (java.io.FileOutputStream. "test2.class"))]
      (.write stream class-data)))

  ;; jar cvf test2.jar *.class test2 && java -cp "test2.jar" test2
  )

(comment
  ;; (let [branches '([::&parser/case-branch [::&parser/variant "Cons" ([::&parser/variant "Symbol" ([::&parser/string "~"])] [::&parser/variant "Cons" ([::&parser/ident "unquoted"] [::&parser/variant "Nil" ()])])] [::&parser/variant "Cons" ([::&parser/ident "unquoted"] [::&parser/fn-call [::&parser/ident "template"] ([::&parser/ident "tail"])])]]
  ;;                    [::&parser/case-branch [::&parser/variant "Cons" ([::&parser/variant "Symbol" ([::&parser/string "~@"])] [::&parser/variant "Cons" ([::&parser/ident "spliced"] [::&parser/variant "Nil" ()])])] [::&parser/variant "Cons" ([::&parser/variant "Symbol" ([::&parser/string "++"])] [::&parser/variant "Cons" ([::&parser/ident "spliced"] [::&parser/fn-call [::&parser/ident "template"] ([::&parser/ident "tail"])])])]]
  ;;                      [::&parser/case-branch [::&parser/ident "_"] [::&parser/variant "Cons" ([::&parser/ident "head"] [::&parser/fn-call [::&parser/ident "template"] ([::&parser/ident "tail"])])]])
  ;;       ;; Step 1: Get all vars
  ;;       get-vars (fn get-vars [pattern]
  ;;                  (clojure.core.match/match pattern
  ;;                    [::&parser/ident ?name]
  ;;                    (list ?name)
                     
  ;;                    [::&parser/variant ?tag ?members]
  ;;                    (mapcat get-vars ?members)

  ;;                    [::&parser/string ?text]
  ;;                    '()))
  ;;       vars+body (for [branch branches]
  ;;                   (clojure.core.match/match branch
  ;;                     [::&parser/case-branch ?pattern ?body]
  ;;                     [(get-vars ?pattern) ?body]))
  ;;       ;; _ (prn 'vars+body vars+body)
  ;;       max-registers (reduce max 0 (map (comp count first) vars+body))
  ;;       ;; _ (prn 'max-registers max-registers)
  ;;       ;; Step 2: Analyse bodies
  ;;       ;; all-analysis (map (fn [[vars body]]
  ;;       ;;                     (reduce #(with-local %2 [::&type/object "java.lang.Object" []] %1)
  ;;       ;;                             (analyse-form* body)
  ;;       ;;                             (reverse vars)))
  ;;       ;;                   vars+body)
  ;;       ;; Step 3: Extract bodies
  ;;       [_ branch-mappings branches*] (reduce (fn [[$link links branches*] branch]
  ;;                                               (clojure.core.match/match branch
  ;;                                                 [::&parser/case-branch ?pattern ?body]
  ;;                                                 [(inc $link) (assoc links $link ?body) (conj branches* [::&parser/case-branch ?pattern $link])]))
  ;;                                             [0 {} []]
  ;;                                             branches)
  ;;       ;; Step 4: Pattens -> Instructions
  ;;       ;; ->instructions (fn ->instructions [locals pattern]
  ;;       ;;                  (clojure.core.match/match pattern
  ;;       ;;                    [::&parser/variant ?tag ?members]
  ;;       ;;                    [::pm-variant ?tag (map (partial ->instructions locals) ?members)]
                           
  ;;       ;;                    [::&parser/ident ?name]
  ;;       ;;                    [::pm-local (get locals ?name)]
                           
  ;;       ;;                    [::&parser/string ?text]
  ;;       ;;                    [::pm-text ?text]
  ;;       ;;                    ))
  ;;       ;; $scope 0 ;; scope-id
  ;;       ;; $local 11 ;; next-local-idx
  ;;       ;; branches** (for [[branch branch-vars] (map vector branches* (map first vars+body))
  ;;       ;;                  :let [[_ locals] (reduce (fn [[$local =locals] $var]
  ;;       ;;                                             [(inc $local) (assoc =locals $var [::local $scope $local])])
  ;;       ;;                                           [$local {}] branch-vars)]]
  ;;       ;;              (clojure.core.match/match branch
  ;;       ;;                [::&parser/case-branch ?pattern ?body]
  ;;       ;;                [(->instructions locals ?pattern) ?body]))
  ;;       ;; _ (prn branches**)
  ;;       ;; Step 5: Re-structure branching
  ;;       ]
  ;;   ;; [branch-mappings branches**]
  ;;   branches*)

  
  

  
  ;; (let [data '([[:lang/pm-variant "Cons" ([:lang/pm-variant "Symbol" ([:lang/pm-text "~"])]  [:lang/pm-variant "Cons" ([:lang/pm-local [:lang/local 0 11]] [:lang/pm-variant "Nil" ()])])] 0]
  ;;                [[:lang/pm-variant "Cons" ([:lang/pm-variant "Symbol" ([:lang/pm-text "~@"])] [:lang/pm-variant "Cons" ([:lang/pm-local [:lang/local 0 11]] [:lang/pm-variant "Nil" ()])])] 1]
  ;;                  [[:lang/pm-local [:lang/local 0 11]] 2])
  ;;       classify-outer (fn [struct [branch $body]]
  ;;                        (clojure.core.match/match branch
  ;;                          [::pm-variant ?tag ?members]
  ;;                          (update-in struct [:cases ?tag] conj {:members ?members
  ;;                                                                :body $body})
                           
  ;;                          [::pm-text ?text]
  ;;                          (update-in struct [:tests] conj {:test [::text ?text]
  ;;                                                           :body $body})

  ;;                          [::pm-local ?binding]
  ;;                          (assoc struct :default {:storage ?binding
  ;;                                                  :body $body})))
  ;;       outer-classification (reduce classify-outer
  ;;                                    {:cases {}
  ;;                                     :tests '()
  ;;                                     :default nil}
  ;;                                    data)
  ;;       full-classifier (fn full-classifier [global]
  ;;                         (prn 'full-classifier global)
  ;;                         (let [subcases (:cases global)]
  ;;                           (if (empty? subcases)
  ;;                             global
  ;;                             (let [crossed (sort (fn [x1 x2] (> (-> x1 second :cases count) (-> x2 second :cases count)))
  ;;                                                 (for [[tag subs] subcases
  ;;                                                       :let [_ (prn 'subcases tag subs)]
  ;;                                                       :let [parts (for [cross (apply map list (map :members subs))
  ;;                                                                         :let [_ (prn 'cross tag cross)]
  ;;                                                                         ;; :let [_ (prn '(map :body subs) (map :body subs))]
  ;;                                                                         ;; :let [_ (prn (class cross)            (count cross)
  ;;                                                                         ;;              (class (map :body subs)) (count (map :body subs)))]
  ;;                                                                         :let [cross+ (map vector cross (map :body subs))]
  ;;                                                                         ;; :let [_ (prn 'cross+ tag (class cross+) (count cross+))]
  ;;                                                                         ;; :let [_ (prn 'cross+ tag cross+)]
  ;;                                                                         :let [cross++ (reduce classify-outer
  ;;                                                                                               {:cases {}
  ;;                                                                                                :tests '()
  ;;                                                                                                :default nil}
  ;;                                                                                               cross+)]
  ;;                                                                         ;; :let [_ (prn 'cross++ tag cross++)]
  ;;                                                                         ]
  ;;                                                                     cross++)]
  ;;                                                       :let [_ (prn 'parts parts)]]
  ;;                                                   [tag parts]))
                                    
  ;;                                   ]
  ;;                               (assoc global :cases (reduce (fn [tree [tag subcases]]
  ;;                                                              (update-in tree [tag] #(conj (or % []) (full-classifier subcases))))
  ;;                                                            {}
  ;;                                                            crossed))))))]
  ;;   (full-classifier outer-classification))
  )

(comment
  [{2 [:lang.parser/variant "Cons" ([:lang.parser/ident "head"] [:lang.parser/fn-call [:lang.parser/ident "template"] ([:lang.parser/ident "tail"])])],
    1 [:lang.parser/variant "Cons" ([:lang.parser/variant "Symbol" ([:lang.parser/string "++"])] [:lang.parser/variant "Cons" ([:lang.parser/ident "spliced"] [:lang.parser/fn-call [:lang.parser/ident "template"] ([:lang.parser/ident "tail"])])])],
    0 [:lang.parser/variant "Cons" ([:lang.parser/ident "unquoted"] [:lang.parser/fn-call [:lang.parser/ident "template"] ([:lang.parser/ident "tail"])])]}
   {:type :lang/adt*,
    :patterns {"Cons" ({:type :lang/adt*,
                        :patterns {"Symbol" ({:type :lang/text-tests,
                                              :patterns {"~@" #{1},
                                                         "~" #{0}},
                                              :defaults [],
                                              :branches #{0 1}})},
                        :default nil,
                        :branches #{0 1}}
                       {:type :lang/adt*,
                        :patterns {"Cons" ({:type :lang/defaults,
                                            :stores {[:lang/local 0 11] #{0 1}},
                                            :branches #{0 1}}
                                           {:type :lang/adt*,
                                            :patterns {"Nil" ()},
                                            :default nil,
                                            :branches #{0 1}})},
                        :default nil,
                        :branches #{0 1}})},
    :default [:lang/default [:lang/local 0 11] 2],
    :branches #{0 1 2}}]
  
  (let [data '([[:lang/pm-variant "Cons" ([:lang/pm-variant "Symbol" ([:lang/pm-text "~"])]  [:lang/pm-variant "Cons" ([:lang/pm-local [:lang/local 0 11]] [:lang/pm-variant "Nil" ()])])] 0]
                 [[:lang/pm-variant "Cons" ([:lang/pm-variant "Symbol" ([:lang/pm-text "~@"])] [:lang/pm-variant "Cons" ([:lang/pm-local [:lang/local 0 11]] [:lang/pm-variant "Nil" ()])])] 1]
                   [[:lang/pm-local [:lang/local 0 11]] 2])
        ]
    (generate-branches data))

  ;; (def (workday? d)
  ;;   (case d
  ;;     (or [#Monday #Tuesday #Wednesday #Thursday #Friday]
  ;;         true)
  ;;     (or [#Saturday #Sunday]
  ;;         false)))
  
  )
