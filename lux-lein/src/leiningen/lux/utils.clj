;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns leiningen.lux.utils
  (:require [leiningen.core.classpath :as classpath])
  (:import (java.io File
                    InputStreamReader
                    BufferedReader)))

(def ^:const ^String output-dir "target/jvm")
(def ^:const ^String output-package "program.jar")

(def ^:private unit-separator (str (char 31)))

(def ^:private vm-options "-server -Xms2048m -Xmx2048m -XX:+OptimizeStringConcat")

(defn compile-path [project module source-paths]
  (let [output-dir (get-in project [:lux :target] output-dir)
        jar-paths (->> ^java.net.URLClassLoader (ClassLoader/getSystemClassLoader)
                       (.getURLs)
                       (map #(.getFile ^java.net.URL %))
                       (filter #(.endsWith ^String % ".jar")))
        compiler-path (some (fn [^:private path]
                              (if (.contains path "com/github/luxlang/luxc-jvm")
                                path
                                nil))
                            jar-paths)
        stdlib-path (some (fn [^:private path]
                            (if (.contains path "com/github/luxlang/lux-stdlib")
                              path
                              nil))
                          jar-paths)
        deps-paths (filter (fn [^:private path]
                             (or (.contains path "org/ow2/asm/asm-all")
                                 (.contains path "org/clojure/core.match")
                                 (.contains path "org/clojure/clojure")))
                           jar-paths)
        sdk-path (get-in project [:lux :android :sdk])
        android-path (str sdk-path "/platforms/android-" (get-in project [:lux :android :version]) "/android.jar")
        deps-paths (if (.exists (new File android-path))
                     (cons android-path deps-paths)
                     deps-paths)]
    (let [class-path (->> (classpath/get-classpath project)
                          (filter #(.endsWith % ".jar"))
                          (concat deps-paths)
                          (list* stdlib-path)
                          (interpose java.io.File/pathSeparator)
                          (reduce str ""))
          java-cmd (get project :java-cmd "java")
          jvm-opts (->> (get project :jvm-opts) (interpose " ") (reduce str ""))]
      (str java-cmd " " jvm-opts " " vm-options " -cp " (str compiler-path ":" class-path)
           " lux release " module
           " " (->> (get project :resource-paths (list)) (interpose unit-separator) (apply str))
           " " (->> source-paths (interpose unit-separator) (apply str))
           " " output-dir))))

(defn repl-path [project source-paths]
  (let [jar-paths (->> ^java.net.URLClassLoader (ClassLoader/getSystemClassLoader)
                       (.getURLs)
                       (map #(.getFile ^java.net.URL %))
                       (filter #(.endsWith ^String % ".jar")))
        compiler-path (some (fn [^:private path]
                              (if (.contains path "com/github/luxlang/luxc-jvm")
                                path
                                nil))
                            jar-paths)
        deps-paths (filter (fn [^:private path]
                             (or (.contains path "org/ow2/asm/asm-all")
                                 (.contains path "org/clojure/core.match")
                                 (.contains path "org/clojure/clojure")))
                           jar-paths)]
    (let [class-path (->> (classpath/get-classpath project) (filter #(.endsWith % ".jar")) (concat deps-paths) (interpose ":") (reduce str ""))
          java-cmd (get project :java-cmd "java")
          jvm-opts (->> (get project :jvm-opts) (interpose " ") (reduce str ""))]
      (str java-cmd " " jvm-opts " " vm-options " -cp " (str compiler-path ":" class-path)
           " lux repl " (->> source-paths (interpose unit-separator) (apply str))))))

(defn run-process [command working-directory pre post]
  (let [process (.exec (Runtime/getRuntime) command nil working-directory)]
    (with-open [std-out (->> process .getInputStream (new InputStreamReader) (new BufferedReader))
                std-err (->> process .getErrorStream (new InputStreamReader) (new BufferedReader))]
      (println pre)
      (loop [line (.readLine std-out)]
        (when line
          (println line)
          (recur (.readLine std-out))))
      (loop [line (.readLine std-err)]
        (when line
          (println line)
          (recur (.readLine std-err))))
      (println post))))
