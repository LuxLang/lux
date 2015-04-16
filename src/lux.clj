(ns lux
  (:require [lux.base :as &]
            [lux.compiler :as &compiler]
            :reload-all))

(comment
  ;; TODO: Finish type system.
  ;; TODO: Re-implement compiler in language.
  ;; TODO: Add source-file information to .class files for easier debugging.
  ;; TODO: Finish implementing class & interface definition
  ;; TODO: All optimizations
  ;; TODO: Anonymous classes
  ;; TODO: 

  ;; Finish total-locals

  ;; TODO: Change &type/check to it returns a tuple with the new expected & actual types
  ;; TODO: Stop passing-along the exo-types and instead pass-along endo-types where possible
  ;; TODO: Optimize analyser to avoid redundant checks when dealing with type-checking (making sure check* is being handed a type)
  
  (time (&compiler/compile-all (&/|list "lux")))
  (System/gc)
  (time (&compiler/compile-all (&/|list "lux" "test2")))

  ;; jar cvf test2.jar *.class test2 && java -cp "test2.jar" test2
  ;; cd output && jar cvf test2.jar * && java -cp "test2.jar" test2 && cd ..
  )
