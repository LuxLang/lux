(ns lux
  (:gen-class)
  (:require [lux.base :as &]
            [lux.compiler :as &compiler]
            :reload-all))

(defn -main [& _]
  (time (&compiler/compile-all (&/|list "program")))
  (System/exit 0))

(comment
  ;; cd output && jar cvf program.jar * && java -cp "program.jar" program && cd ..
  )
