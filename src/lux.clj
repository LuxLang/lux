(ns lux
  (:require [lux.compiler :as &compiler]
            :reload-all))

(comment
  ;; TODO: Finish type system.
  ;; TODO: Re-implement compiler in language.
  ;; TODO: Adding metadata to global vars.
  ;; TODO: Allow setting fields.
  ;; TODO: Add column & line numbers for syntactic elements.
  ;; TODO: Add source-file information to .class files for easier debugging.
  ;; TODO: invokespecial & invokeinterface
  ;; TODO: 
  
  (time (&compiler/compile-all ["lux"]))
  (time (&compiler/compile-all ["lux" "test2"]))

  ;; jar cvf test2.jar *.class test2 && java -cp "test2.jar" test2
  ;; cd output && jar cvf test2.jar * && java -cp "test2.jar" test2 && cd ..
  )




