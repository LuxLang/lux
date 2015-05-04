(ns lux
  (:gen-class)
  (:require [lux.base :as &]
            [lux.compiler :as &compiler]
            :reload-all))

(defn -main [& _]
  (time (&compiler/compile-all (&/|list "program")))
  (System/exit 0))

(comment
  ;; TODO: Finish total-locals

  (time (&compiler/compile-all (&/|list "program")))
  
  (time (&compiler/compile-all (&/|list "lux")))
  (System/gc)
  (time (&compiler/compile-all (&/|list "lux" "test2")))

  ;; jar cvf test2.jar *.class test2 && java -cp "test2.jar" test2
  ;; jar cvf program.jar output/*.class output/program && java -cp "program.jar" program
  ;; cd output && jar cvf test2.jar * && java -cp "test2.jar" test2 && cd ..

  ;; cd output && jar cvf program.jar * && java -cp "program.jar" program && cd ..
  )
