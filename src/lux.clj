(ns lux
  (:require [lux.compiler :as &compiler]
            :reload-all))

(comment
  ;; TODO: Finish type system.
  ;; TODO: Re-implement compiler in language.
  ;; TODO: Adding metadata to global vars.
  ;; TODO: Add column & line numbers for syntactic elements.
  ;; TODO: Add text-dispenser to regulate column & line numbering.
  ;; TODO: Add source-file information to .class files for easier debugging.
  ;; TODO: Finish implementing class & interface definition
  ;; TODO: All optimizations
  ;; TODO: Take module-name aliasing into account.
  ;; TODO: 
  
  (time (&compiler/compile-all ["lux"]))
  (time (&compiler/compile-all ["lux" "test2"]))

  ;; jar cvf test2.jar *.class test2 && java -cp "test2.jar" test2
  ;; cd output && jar cvf test2.jar * && java -cp "test2.jar" test2 && cd ..
  )
