(ns lux
  (:require [lux.compiler :as &compiler]
            :reload-all))

(comment
  ;; TODO: Finish type system.
  ;; TODO: Re-implement compiler in language.
  ;; TODO: Adding metadata to global vars.
  ;; TODO: throw, try, catch, finally
  ;; TODO: Allow setting fields.
  ;; TODO: monitor enter & monitor exit.
  ;; TODO: Add column & line numbers for syntactic elements.
  ;; TODO: Add source-file information to .class files for easier debugging.
  ;; TODO: Add conversions between primitives
  ;; TODO: Being able to ask if a value is null or not. (null?)
  ;; TODO: invokespecial & invokeinterface
  ;; TODO: bitwise operators
  ;; TODO: multianewarray
  ;; TODO: 
  
  (time (&compiler/compile-all ["lux"]))
  (time (&compiler/compile-all ["lux" "test2"]))

  ;; jar cvf test2.jar *.class test2 && java -cp "test2.jar" test2
  ;; cd output && jar cvf test2.jar * && java -cp "test2.jar" test2 && cd ..
  )




