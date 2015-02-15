
(let [+tag-sig+ (->type-signature "java.lang.String")
      variant-class* (->class +variant-class+)
      tuple-class* (->class +tuple-class+)
      +variant-field-sig+ (->type-signature "java.lang.Object")
      oclass (->class "java.lang.Object")
      equals-sig (str "(" (->type-signature "java.lang.Object") ")Z")]
  (defn ^:private compile-decision-tree [writer mappings default-label decision-tree]
    (match decision-tree
      [::test-bool ?pairs]
      (compile-compare-bools writer mappings default-label ?pairs)
      
      [::test-int  ?pairs]
      (compile-compare-ints writer mappings default-label ?pairs)

      [::test-real ?pairs]
      (compile-compare-reals writer mappings default-label ?pairs)

      [::test-char ?pairs]
      (compile-compare-chars writer mappings default-label ?pairs)
      
      [::test-text ?pairs]
      (compile-compare-texts writer mappings default-label ?pairs)

      [::store ?idx $body]
      (doto writer
        (.visitVarInsn Opcodes/ASTORE ?idx)
        (.visitJumpInsn Opcodes/GOTO (get mappings $body)))
      
      [::test-tuple ?branches ?cases]
      (let [[_ ?subcases] (first ?cases)
            arity (-> ?subcases first (nth 2) count)
            tuple-class** (str tuple-class* arity)]
        (doto writer
          ;; object
          (.visitTypeInsn Opcodes/CHECKCAST tuple-class**) ;; tuple
          (do (doseq [subcase ?subcases
                      :let [next-subcase (new Label)]]
                (match subcase
                  [::subcase $body ?subseq]
                  (do (doseq [[?subpart ?subidx] (map vector ?subseq (range (count ?subseq)))
                              :let [sub-next-elem (new Label)]]
                        (doto writer
                          (.visitInsn Opcodes/DUP) ;; tuple, tuple
                          (.visitFieldInsn Opcodes/GETFIELD tuple-class** (str +partial-prefix+ ?subidx) +variant-field-sig+) ;; tuple, object
                          (compile-decision-tree (assoc mappings $body sub-next-elem) next-subcase ?subpart) ;; tuple
                          (.visitLabel sub-next-elem)))
                    (doto writer
                      (.visitInsn Opcodes/POP)
                      (.visitJumpInsn Opcodes/GOTO (get mappings $body))
                      (.visitLabel next-subcase)))
                  )))
          (.visitInsn Opcodes/POP) ;; ->
          (.visitJumpInsn Opcodes/GOTO default-label)))

      [::test-variant ?branches ?cases]
      (doto writer
        ;; object
        (.visitTypeInsn Opcodes/CHECKCAST variant-class*) ;; variant
        (.visitInsn Opcodes/DUP) ;; variant, variant
        (.visitFieldInsn Opcodes/GETFIELD variant-class* "tag" +tag-sig+) ;; variant, tag
        (-> (doto (.visitInsn Opcodes/DUP) ;; variant, tag, tag
              (.visitLdcInsn ?tag) ;; variant, tag, tag, text
              (.visitMethodInsn Opcodes/INVOKEVIRTUAL oclass "equals" equals-sig) ;; variant, tag, B
              (.visitJumpInsn Opcodes/IFEQ tag-else-label) ;; variant, tag
              (.visitInsn Opcodes/POP) ;; variant
              (do (let [arity (-> ?subcases first (nth 2) count)
                        variant-class** (str variant-class* arity)]
                    (.visitTypeInsn writer Opcodes/CHECKCAST variant-class**) ;; variantN
                    (doseq [subcase ?subcases
                            :let [next-subcase (new Label)]]
                      (match subcase
                        [::subcase $body ?subseq]
                        (do (doseq [[?subpart ?subidx] (map vector ?subseq (range (count ?subseq)))
                                    :let [sub-next-elem (new Label)]]
                              (doto writer
                                (.visitInsn Opcodes/DUP) ;; variant, variant
                                (.visitFieldInsn Opcodes/GETFIELD variant-class** (str +partial-prefix+ ?subidx) +variant-field-sig+) ;; variant, object
                                (compile-decision-tree (assoc mappings $body sub-next-elem) next-subcase ?subpart) ;; variant
                                (.visitLabel sub-next-elem)))
                          (doto writer
                            (.visitInsn Opcodes/POP)
                            (.visitJumpInsn Opcodes/GOTO (get mappings $body))
                            (.visitLabel next-subcase)))
                        ))
                    ))
              (.visitInsn Opcodes/POP) ;; ->
              (.visitJumpInsn Opcodes/GOTO default-label)
              ;; variant, tag ->
              (.visitLabel tag-else-label))
            (->> (doseq [[?tag ?subcases] ?cases
                         :let [tag-else-label (new Label)]])))
        ;; variant, tag ->
        (.visitInsn Opcodes/POP) ;; variant ->
        (.visitInsn Opcodes/POP) ;; ->
        (.visitJumpInsn Opcodes/GOTO default-label)))
    ))

(defn ^:private map-branches [idx mappings patterns]
  (reduce (fn [[idx mappings patterns*] [test body]]
            [(inc idx)
             (assoc mappings idx body)
             (cons [test idx] patterns*)])
          [idx mappings (list)]
          patterns))

(defn ^:private map-bodies [pm-struct]
  (match pm-struct
    [::BoolPM ?patterns ?defaults]
    (let [[idx mappings patterns*] (map-branches 0 {} ?patterns)
          [_ mappings* defaults*] (map-branches idx mappings ?defaults)]
      [mappings* [::BoolPM patterns* defaults*]])

    [::IntPM ?patterns ?defaults]
    (let [[idx mappings patterns*] (map-branches 0 {} ?patterns)
          [_ mappings* defaults*] (map-branches idx mappings ?defaults)]
      [mappings* [::IntPM patterns* defaults*]])

    [::RealPM ?patterns ?defaults]
    (let [[idx mappings patterns*] (map-branches 0 {} ?patterns)
          [_ mappings* defaults*] (map-branches idx mappings ?defaults)]
      [mappings* [::RealPM patterns* defaults*]])

    [::CharPM ?patterns ?defaults]
    (let [[idx mappings patterns*] (map-branches 0 {} ?patterns)
          [_ mappings* defaults*] (map-branches idx mappings ?defaults)]
      [mappings* [::CharPM patterns* defaults*]])

    [::TextPM ?patterns ?defaults]
    (let [[idx mappings patterns*] (map-branches 0 {} ?patterns)
          [_ mappings* defaults*] (map-branches idx mappings ?defaults)]
      [mappings* [::TextPM patterns* defaults*]])

    [::TuplePM ?num-elems ?patterns ?defaults]
    (let [[idx mappings patterns*] (map-branches 0 {} ?patterns)
          [_ mappings* defaults*] (map-branches idx mappings ?defaults)]
      [mappings* [::TuplePM ?num-elems patterns* defaults*]])
    
    [::VariantPM ?tags ?patterns ?defaults]
    (let [[idx mappings patterns*] (map-branches 0 {} ?patterns)
          [_ mappings* defaults*] (map-branches idx mappings ?defaults)]
      [mappings* [::VariantPM ?tags patterns* defaults*]])
    
    [::?PM ?defaults]
    (let [[_ mappings defaults*] (map-branches 0 {} ?defaults)]
      [mappings [::?PM defaults*]])))

(defn ^:private get-default [pm-struct]
  (match pm-struct
    [::BoolPM ?patterns ?defaults]
    (first ?defaults)

    [::IntPM ?patterns ?defaults]
    (first ?defaults)

    [::RealPM ?patterns ?defaults]
    (first ?defaults)

    [::CharPM ?patterns ?defaults]
    (first ?defaults)

    [::TextPM ?patterns ?defaults]
    (first ?defaults)

    [::TuplePM ?num-elems ?patterns ?defaults]
    (first ?defaults)
    
    [::VariantPM ?tags ?patterns ?defaults]
    (first ?defaults)
    
    [::?PM ?defaults]
    (first ?defaults)
    ))

(do-template [<name> <wrapper-class> <value-method> <method-sig>]
  (defn <name> [writer mappings $default ?patterns]
    (doseq [[?token $body] ?patterns
            :let [$else (new Label)]]
      (doto writer
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL (->class <wrapper-class>) <value-method> <method-sig>)
        (.visitLdcInsn ?token)
        (.visitJumpInsn Opcodes/IF_ICMPNE $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO (get mappings $body))
        (.visitLabel $else)))
    (doto writer
      (.visitInsn Opcodes/POP)
      (.visitJumpInsn Opcodes/GOTO $default)))

  ^:private compile-bool-pm "java.lang.Boolean"   "booleanValue" "()Z"
  ^:private compile-char-pm "java.lang.Character" "charValue"    "()C"
  )

(do-template [<name> <wrapper-class> <value-method> <method-sig> <cmp-op>]
  (defn <name> [writer mappings $default ?patterns]
    (doseq [[?token $body] ?patterns
            :let [$else (new Label)]]
      (doto writer
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL (->class <wrapper-class>) <value-method> <method-sig>)
        (.visitLdcInsn ?token)
        (.visitInsn <cmp-op>)
        (.visitJumpInsn Opcodes/IFNE $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO (get mappings $body))
        (.visitLabel $else)))
    (doto writer
      (.visitInsn Opcodes/POP)
      (.visitJumpInsn Opcodes/GOTO $default)))

  ^:private compile-int-pm  "java.lang.Long"   "longValue"   "()J" Opcodes/LCMP
  ^:private compile-real-pm "java.lang.Double" "doubleValue" "()D" Opcodes/DCMPL
  )

(defn ^:private compile-text-pm [writer mappings $default ?patterns]
  (doseq [[?token $body] ?patterns
          :let [$else (new Label)]]
    (doto writer
      (.visitInsn Opcodes/DUP)
      (.visitLdcInsn ?token)
      (.visitMethodInsn Opcodes/INVOKEVIRTUAL (->class "java.lang.Object") "equals" (str "(" (->type-signature "java.lang.Object") ")Z"))
      (.visitJumpInsn Opcodes/IFEQ $else)
      (.visitInsn Opcodes/POP)
      (.visitJumpInsn Opcodes/GOTO (get mappings $body))
      (.visitLabel $else)))
  (doto writer
    (.visitInsn Opcodes/POP)
    (.visitJumpInsn Opcodes/GOTO $default)))

(defn ^:private compile-tuple-pm [writer mapping $default ?num-elems ?patterns]
  (let [sub-patterns (map (fn [idx]
                            (map (fn [tup body]
                                   [(nth tup idx) body])
                                 ?patterns))
                          (range ?num-elems))
        subpm-structs (map group-patterns sub-patterns)
        [pat-h & pat-t] subpm-structs
        (for [(get-branches pat-h)
              (cull pat-t)]
          )
        (reduce (fn [branches pattern]
                  ( (group-patterns pattern)))
                (get-branches pat-h)
                pat-t)
        (sequence-tests sub-patterns)]
    ))

(defn ^:private compile-pm [writer mapping pm-struct]
  (match pm-struct
    [::BoolPM ?patterns ?defaults]
    (compile-bool-pm writer mapping $default ?patterns)

    [::IntPM ?patterns ?defaults]
    (compile-int-pm writer mapping $default ?patterns)

    [::RealPM ?patterns ?defaults]
    (compile-real-pm writer mapping $default ?patterns)

    [::CharPM ?patterns ?defaults]
    (compile-char-pm writer mapping $default ?patterns)

    [::TextPM ?patterns ?defaults]
    (compile-text-pm writer mapping $default ?patterns)

    [::TuplePM ?num-elems ?patterns ?defaults]
    (compile-tuple-pm writer mapping $default ?num-elems ?patterns)
    
    [::VariantPM ?tags ?patterns ?defaults]
    (first ?defaults)
    
    [::?PM ?defaults]
    (first ?defaults)
    ))

(do-template [<name> <pm-tag>]
  (defn <name> [pm value body]
    (match pm
      [<pm-tag> ?branches ?defaults]
      (return [<pm-tag> (cons [value body] ?branches) ?defaults])

      [::?PM ?defaults]
      (return [<pm-tag> (list [value body])           ?defaults])

      _
      (fail "Can't match pattern!")))

  ^:private group-bool-pm ::BoolPM
  ^:private group-int-pm  ::IntPM
  ^:private group-real-pm ::RealPM
  ^:private group-char-pm ::CharPM
  ^:private group-text-pm ::textPM
  )

(defn ^:private group-branch [pm [pattern body]]
  (match pattern
    [::&parser/Bool ?value]
    (group-bool-pm pm ?value body)

    [::&parser/Int ?value]
    (group-int-pm pm ?value body)

    [::&parser/Real ?value]
    (group-real-pm pm ?value body)

    [::&parser/Char ?value]
    (group-char-pm pm ?value body)

    [::&parser/Text ?value]
    (group-text-pm pm ?value body)
    
    [::&parser/Tuple ?members]
    (match pm
      [::TuplePM ?num-elems ?branches ?defaults]
      (exec [_ (assert! (= ?num-elems (count ?members))
                        (str "[Analyser Error] Mismatch in tuple size: " ?num-elems " =/= " (count ?members)))]
        (return [::TuplePM ?num-elems (cons [?members body] ?branches) ?defaults]))

      [::?PM ?defaults]
      (return [::TuplePM (count ?members) (list [?members body]) ?defaults])

      _
      (fail "Can't match pattern!"))

    [::&parser/Tag ?tag]
    (let [members (list)
          num-members (count members)]
      (match pm
        [::VariantPM ?variants ?branches ?defaults]
        (exec [variants* (if-let [?num-elems (get ?variants ?tag)]
                           (exec [_ (assert! (= ?num-elems num-members)
                                             (str "[Analyser Error] Mismatch in tuple size: " ?num-elems " =/= " num-members))]
                             (return ?variants))
                           (return (assoc ?variants ?tag num-members)))]
          (return [::VariantPM variants* (conj ?branches [[?tag members] body]) ?defaults]))

        [::?PM ?defaults]
        (return [::VariantPM {?tag num-members} (list [[?tag members] body]) ?defaults])

        _
        (fail "Can't match pattern!")))
    
    [::&parser/Form ([[::&parser/Tag ?tag] & ?members] :seq)]
    (let [members ?members
          num-members (count members)]
      (match pm
        [::VariantPM ?variants ?branches ?defaults]
        (exec [variants* (if-let [?num-elems (get ?variants ?tag)]
                           (exec [_ (assert! (= ?num-elems num-members)
                                             (str "[Analyser Error] Mismatch in tuple size: " ?num-elems " =/= " num-members))]
                             (return ?variants))
                           (return (assoc ?variants ?tag num-members)))]
          (return [::VariantPM variants* (conj ?branches [[?tag members] body]) ?defaults]))

        [::?PM ?defaults]
        (return [::VariantPM {?tag num-members} (list [[?tag members] body]) ?defaults])

        _
        (fail "Can't match pattern!")))

    [::&parser/Ident ?name]
    (match pm
      [::BoolPM ?patterns ?defaults]
      (return [::BoolPM ?patterns (conj ?defaults [?name body])])

      [::IntPM ?patterns ?defaults]
      (return [::IntPM ?patterns (conj ?defaults [?name body])])

      [::RealPM ?patterns ?defaults]
      (return [::RealPM ?patterns (conj ?defaults [?name body])])

      [::CharPM ?patterns ?defaults]
      (return [::CharPM ?patterns (conj ?defaults [?name body])])

      [::TextPM ?patterns ?defaults]
      (return [::TextPM ?patterns (conj ?defaults [?name body])])

      [::TuplePM ?num-elems ?patterns ?defaults]
      (return [::TuplePM ?num-elems ?patterns (conj ?defaults [?name body])])
      
      [::VariantPM ?tags ?patterns ?defaults]
      (return [::VariantPM ?tags ?patterns (conj ?defaults [?name body])])
      
      [::?PM ?defaults]
      (return [::?PM (conj ?defaults [?name body])]))
    ))

(defn ^:private valid-paths [group]
  (set (match group
         [::BoolPM ?patterns ?defaults]
         (concat (map second ?patterns) (map second ?defaults))

         [::IntPM ?patterns ?defaults]
         (concat (map second ?patterns) (map second ?defaults))

         [::RealPM ?patterns ?defaults]
         (concat (map second ?patterns) (map second ?defaults))

         [::CharPM ?patterns ?defaults]
         (concat (map second ?patterns) (map second ?defaults))

         [::TextPM ?patterns ?defaults]
         (concat (map second ?patterns) (map second ?defaults))

         [::TuplePM ?num-elems ?patterns ?defaults]
         (concat (map second ?patterns) (map second ?defaults))

         [::VariantPM ?tags ?patterns ?defaults]
         (concat (map second ?patterns) (map second ?defaults))

         [::?PM ?defaults]
         (map second ?defaults))))

(defn ^:private sequence-multi-pm [sequence-pm prev-paths groups]
  (match groups
    ([head & tail] :seq)
    (for [:let [curr-paths (set/intersection prev-paths (valid-paths head))]
          [head-paths head-test] (sequence-pm curr-paths head)]
      [:multi-test head-test head-paths (sequence-multi-pm sequence-pm head-paths tail)])
    
    _
    (list (list))))

(do-template [<name> <pm> <test>]
  (defn <name> [prev-paths group]
    (match group
      [<pm> ?patterns ?defaults]
      (return (concat (for [[value $body] ?patterns
                            :when (contains? prev-paths $body)]
                        [<test> value #{$body}])
                      (match ?defaults
                        ([[default-register $body] & _] :seq)
                        (list [<test> default-register #{$body}])
                        
                        :else
                        (list))))

      :else
      (fail "")))

  ^:private sequence-bool ::BoolPM ::test-bool
  ^:private sequence-int  ::IntPM  ::test-int
  ^:private sequence-real ::RealPM ::test-real
  ^:private sequence-char ::CharPM ::test-char
  ^:private sequence-text ::TextPM ::test-text
  )

(defn ^:private sequence-? [group]
  [::?PM ([[default-register $body] & _] :seq)]
  (return (list [<test> default-register #{$body}]))

  :else
  (fail ""))

(defn ^:private sequence-pm [group]
  (match group
    [::BoolPM _ _]
    (sequence-bool group)

    [::IntPM _ _]
    (sequence-int group)

    [::RealPM _ _]
    (sequence-real group)

    [::CharPM _ _]
    (sequence-char group)

    [::TextPM _ _]
    (sequence-text group)

    [::?PM _]
    (sequence-? group)

    [::TuplePM ?num-elems ?patterns ?defaults]
    (exec [:let [sub-patterns (map (fn [idx]
                                     (map (fn [[tup body]]
                                            [(nth tup idx) body])
                                          ?patterns))
                                   (range ?num-elems))]
           groups (map-m #(reduce-m group-branch [::?PM (list)] %) sub-patterns)
           tuple-paths (valid-paths group)
           sub-seqs (sequence-multi-pm sequence-pm tuple-paths groups)]
      (return (cons [::test-tuple ?num-elems sub-seqs]
                    (match ?defaults
                      ([[default-register $body] & _] :seq)
                      (list [<test> default-register #{$body}])
                      
                      :else
                      (list)))))

    [::VariantPM ?tags ?patterns ?defaults]
    (map-m (fn [tag]
             (exec [:let [members+bodies (mapcat (fn [[ptag pmembers pbody]]
                                                   (if (= ptag tag)
                                                     (list [pmembers pbody])
                                                     (list)))
                                                 ?patterns)
                          sub-patterns (map (fn [idx]
                                              (map (fn [[tup body]]
                                                     [(nth tup idx) body])
                                                   members+bodies))
                                            (range ?num-elems))]
                    groups (map-m #(reduce-m group-branch [::?PM (list)] %) sub-patterns)
                    tag-paths (set (map second members+bodies))
                    sub-seqs (sequence-multi-pm sequence-pm tag-paths groups)]
               (cons [::test-variant tag ?num-elems sub-seqs]
                     (match ?defaults
                       ([[default-register $body] & _] :seq)
                       (list [<test> default-register #{$body}])
                       
                       :else
                       (list)))))
           (keys ?tags))
    ))

(defn ^:private decision-tree [branches]
  (exec [group (reduce-m group-branch [::?PM (list)] branches)
         :let [[mappings group*] (map-bodies group)
               paths (valid-paths group*)]]
    (sequence-pm paths group*)))

(let [ex-class (->class "java.lang.IllegalStateException")]
  (defn ^:private compile-case [compile *type* ?variant ?base-register ?num-registers ?branches]
    (exec [*writer* &util/get-writer
           :let [$start (new Label)
                 $end (new Label)
                 _ (dotimes [offset ?num-registers]
                     (let [idx (+ ?base-register offset)]
                       (.visitLocalVariable *writer* (str +local-prefix+ idx) (->java-sig [::&type/Any]) nil $start $end idx)))]
           _ (compile ?variant)
           :let [_ (doto *writer*
                     (.visitInsn Opcodes/DUP)
                     (.visitLabel $start))]
           :let [[mapping tree] (decision-tree ?branches)]
           
           :let [[mappings pm-struct*] (map-bodies pm-struct)
                 entries (for [[?branch ?body] mappings
                               :let [label (new Label)]]
                           [[?branch label]
                            [label ?body]])
                 mappings* (into {} (map first entries))
                 ]
           :let [$default (new Label)
                 _ (do (doseq [decision-tree (let [pieces (map first (sequence-parts ?pm-struct))]
                                               (if (get-default pm-struct)
                                                 (butlast pieces)
                                                 pieces))]
                         (compile-decision-tree *writer* mappings* $default decision-tree))
                     (.visitLabel *writer* $default)
                     (if-let [[?idx ?body] (get-default pm-struct)]
                       (doto *writer*
                         (.visitInsn Opcodes/DUP)
                         (.visitVarInsn Opcodes/ASTORE ?idx)
                         (.visitJumpInsn Opcodes/GOTO (get mappings* ?body)))
                       (doto *writer*
                         (.visitInsn Opcodes/POP)
                         (.visitTypeInsn Opcodes/NEW ex-class)
                         (.visitInsn Opcodes/DUP)
                         (.visitMethodInsn Opcodes/INVOKESPECIAL ex-class "<init>" "()V")
                         (.visitInsn Opcodes/ATHROW))))]
           _ (map-m (fn [[?label ?body]]
                      (exec [:let [_ (do (.visitLabel *writer* ?label)
                                       (.visitInsn *writer* Opcodes/POP))]
                             ret (compile ?body)
                             :let [_ (.visitJumpInsn *writer* Opcodes/GOTO $end)]]
                        (return ret)))
                    (map second entries))
           :let [_ (.visitLabel *writer* $end)]]
      (return nil))))
