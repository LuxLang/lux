(ns lux
  (:require [lux.base :as &]
            [lux.compiler :as &compiler]
            :reload-all))

(comment
  ;; TODO: Finish total-locals

  (time (&compiler/compile-all (&/|list "lux")))
  (System/gc)
  (time (&compiler/compile-all (&/|list "lux" "test2")))

  ;; jar cvf test2.jar *.class test2 && java -cp "test2.jar" test2
  ;; cd output && jar cvf test2.jar * && java -cp "test2.jar" test2 && cd ..
  )
