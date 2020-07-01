(ns leiningen.lux.utils
  (:require (clojure
             [template :refer [do-template]]
             [string :as string])
            [leiningen.core.classpath :as classpath])
  (:import (java.io File
                    InputStreamReader
                    BufferedReader)))

(def ^:const ^String default-target-dir "target")
(def ^:const ^String output-package "program.jar")

(def ^:private unit-separator (str (char 31)))

(def ^:private vm-options
  ""
  ;; "-server -Xms2048m -Xmx2048m -XX:+OptimizeStringConcat"
  )

(defn sanitize-path [^String path]
  (.replace path "/" java.io.File/separator))

(defn prepare-path [path]
  (let [is-windows? (and (.startsWith path "/")
                         (= "\\" java.io.File/separator))]
    (sanitize-path (if is-windows?
                     (.substring path 1)
                     path))))

(defn ^:private project-id [project]
  [(get project :group) (get project :name)])

(def ^:private lux-group "com.github.luxlang")
(def ^:private compiler-id [lux-group "luxc-jvm"])
(def ^:private jvm-compiler-id [lux-group "lux-jvm"])
(def ^:private stdlib-id [lux-group "stdlib"])

(defn ^:private id-path
  "(-> Project-ID Text)"
  [[group name]]
  (str (.replace group "." "/") "/" name))

(def ^:private compiler-path (id-path compiler-id))
(def ^:private jvm-compiler-path (id-path jvm-compiler-id))
(def ^:private stdlib-path (id-path stdlib-id))

(defn ^:private project-jars [project]
  (->> project
       classpath/get-classpath
       (filter #(.endsWith % ".jar"))))

(do-template [<name> <path>]
  (defn <name> [jar-paths]
    (some (fn [^:private path]
            (if (.contains path <path>)
              path
              nil))
          jar-paths))

  ^:private find-compiler-path (sanitize-path compiler-path)
  ^:private find-jvm-compiler-path (sanitize-path jvm-compiler-path)
  ^:private find-stdlib-path   (sanitize-path stdlib-path)
  )

(defn ^:private compiler-dependency? [path]
  (or (.contains path (sanitize-path "org/ow2/asm/asm-all"))
      (.contains path (sanitize-path "org/clojure/core.match"))
      (.contains path (sanitize-path "org/clojure/clojure"))
      (.contains path (sanitize-path compiler-path))
      (.contains path (sanitize-path jvm-compiler-path))))

(defn ^:private filter-compiler-dependencies [jar-paths]
  (filter compiler-dependency? jar-paths))

(defn ^:private java-command [project]
  (str (get project :java-cmd "java")
       ;; " " (->> (get project :jvm-opts) (interpose " ") (reduce str ""))
       " " vm-options))

(defn ^:private join-paths [paths]
  (->> paths
       (interpose unit-separator)
       (apply str)
       (str unit-separator)))

(defn ^:private lux-command [project mode program-dependencies source-paths]
  (str "lux " mode
       " " (join-paths program-dependencies)
       " " (join-paths source-paths)
       " " (get project :target-path default-target-dir)))

(do-template [<name> <mode>]
  (defn <name> [project module source-paths]
    (let [is-stdlib? (= stdlib-id
                        (project-id project))
          raw-paths (project-jars project)
          stdlib-path (when (not is-stdlib?)
                        (prepare-path (find-stdlib-path raw-paths)))
          sdk-path (get-in project [:lux :android :sdk])
          android-path (str sdk-path
                            java.io.File/separator "platforms"
                            java.io.File/separator "android-"
                            (get-in project [:lux :android :version])
                            java.io.File/separator "android.jar")
          compiler-dependencies (let [compiler-dependencies (->> raw-paths
                                                                 (filter compiler-dependency?)
                                                                 (map prepare-path))
                                      with-android (if (.exists (new File android-path))
                                                     (cons android-path compiler-dependencies)
                                                     compiler-dependencies)]
                                  with-android)
          program-dependencies (let [deps (->> raw-paths
                                               (filter (complement compiler-dependency?))
                                               (map prepare-path))]
                                 (if is-stdlib?
                                   deps
                                   (list* stdlib-path deps)))
          compiler-path (prepare-path (find-compiler-path raw-paths))
          class-path (->> compiler-dependencies
                          (list* compiler-path)
                          (interpose java.io.File/pathSeparator)
                          (reduce str "")
                          sanitize-path)]
      (str (java-command project) " -cp " class-path
           " " (lux-command project <mode> program-dependencies source-paths))))

  compile-path (str "release " module)
  repl-path    "repl"
  )

(defn build-jvm [project module]
  (let [raw-paths (project-jars project)]
    (when-let [compiler-path (find-jvm-compiler-path raw-paths)]
      (let [compiler (prepare-path compiler-path)
            sources (->> (get project :source-paths (list))
                         (map #(str " --source " %))
                         (string/join ""))
            target (get project :target-path default-target-dir)]
        (str (java-command project)
             " -jar " compiler " build"
             " --library " "~/lux/stdlib/target/library.tar"
             sources
             " --target " target
             " --module " module)))))

(def ^:private normal-exit 0)

(defn run-process [command working-directory pre post]
  (let [process (.exec (Runtime/getRuntime) command nil working-directory)]
    (with-open [std-out (->> process .getInputStream (new InputStreamReader) (new BufferedReader))
                std-err (->> process .getErrorStream (new InputStreamReader) (new BufferedReader))]
      (println pre)
      (loop []
        (when-let [line (.readLine std-out)]
          (println line)
          (recur)))
      (loop []
        (when-let [line (.readLine std-err)]
          (println line)
          (recur)))
      (println post)
      (= normal-exit (.waitFor process)))))
