(ns lux
  (:gen-class)
  (:require [lux.base :as &]
            [lux.compiler :as &compiler]
            [lux.type :as &type]
            :reload-all))

(defn -main [& _]
  (do (time (&compiler/compile-all (&/|list "program")))
    ;; (prn @&type/counter)
    )
  (System/exit 0))

(comment
  ;; cd output && jar cvf program.jar * && java -cp "program.jar" program && cd ..
  )
