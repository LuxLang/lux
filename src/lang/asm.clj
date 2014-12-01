(ns lang.asm
  (:import (org.objectweb.asm Opcodes
                              ClassWriter
                              MethodVisitor)))

(defn write-file [file data]
  (with-open [stream (java.io.BufferedOutputStream. (java.io.FileOutputStream. file))]
    (.write stream data)))

(comment
  (let [class-data (let [cw (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                              (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                                      "hello_world" nil "java/lang/Object" nil))]
                     (doto (.visitMethod cw Opcodes/ACC_PUBLIC "<init>" "()V" nil nil)
                       (.visitCode)
                       (.visitVarInsn Opcodes/ALOAD 0)
                       (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V")
                       (.visitInsn Opcodes/RETURN)
                       (.visitMaxs 0 0)
                       (.visitEnd))
                     (doto (.visitMethod cw (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "main" "([Ljava/lang/String;)V" nil nil)
                       (.visitCode)
                       (.visitFieldInsn Opcodes/GETSTATIC "java/lang/System" "out" "Ljava/io/PrintStream;")
                       (.visitLdcInsn "Hello, World!")
                       (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/io/PrintStream" "println" "(Ljava/lang/String;)V")
                       (.visitInsn Opcodes/RETURN)
                       (.visitMaxs 0 0)
                       (.visitEnd))
                     (.visitEnd cw)
                     (.toByteArray cw))]
    (write-file "hello_world.class" class-data))

  
  )

;; package asm;
;; public class HelloWorld {
;;     public static void main(String[] args) {
;;         System.out.println("Hello, World!");
;;     }
;; }







