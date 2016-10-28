;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.packager.program
  (:require [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return fail fail*]]
                 [host :as &host])
            (lux.compiler [base :as &&]))
  (:import (java.io InputStream
                    File
                    FileInputStream
                    FileOutputStream
                    BufferedInputStream
                    ByteArrayOutputStream)
           (java.util.jar Manifest
                          Attributes$Name
                          JarEntry
                          JarInputStream
                          JarOutputStream
                          )))

;; [Utils]
(def ^:private kilobyte 1024)
(def ^:private buffer-size (* 10 kilobyte))

(def ^:private jar-resources "__LUX_PROGRAM_RESOURCES__/")

(defn ^:private manifest
  "(-> Text Manifest)"
  [^String module]
  (doto (new Manifest)
    (-> .getMainAttributes (doto (.put Attributes$Name/MAIN_CLASS (str (&host/->module-class module) "._"))
                             (.put Attributes$Name/MANIFEST_VERSION "1.0")
                             (.put Attributes$Name/CLASS_PATH jar-resources)))))

(defn ^:private write-class!
  "(-> Text File JarOutputStream Null)"
  [^String path ^File file ^JarOutputStream out]
  (with-open [in (new BufferedInputStream (new FileInputStream file))]
    (let [buffer (byte-array buffer-size)]
      (doto out
        (.putNextEntry (new JarEntry (str path "/" (.getName file))))
        (-> (.write buffer 0 bytes-read)
            (->> (when (not= -1 bytes-read))
                 (loop [bytes-read (.read in buffer)])))
        (.flush)
        (.closeEntry)
        ))
    ))

(let [output-dir-size (inc (.length &&/output-dir))]
  (defn ^:private write-module!
    "(-> File JarOutputStream Null)"
    [^File file ^JarOutputStream out]
    (let [module-name (.substring (.getPath file) output-dir-size)
          inner-files (.listFiles file)
          inner-modules (filter #(.isDirectory ^File %) inner-files)
          inner-classes (filter #(not (.isDirectory ^File %)) inner-files)]
      (doseq [$class inner-classes]
        (write-class! module-name $class out))
      (doseq [$module inner-modules]
        (write-module! $module out)))))

(defn ^:private write-resources!
  "(-> JarOutputStream (List Text) Null)"
  [^JarOutputStream out resources-dirs]
  (doseq [^String resources-dir (&/->seq resources-dirs)
          :let [resources-dir (new File resources-dir)]
          :when (.exists resources-dir)
          ^File res (.listFiles resources-dir)
          :let [buffer (byte-array buffer-size)]]
    (with-open [in (->> res (new FileInputStream) (new BufferedInputStream))]
      (doto out
        (.putNextEntry (new JarEntry (str jar-resources (.getName res))))
        (-> (.write buffer 0 bytes-read)
            (->> (when (not= -1 bytes-read))
                 (loop [bytes-read (.read in buffer)])))
        (.flush)
        (.closeEntry))
      )))

(defn ^:private fetch-available-jars []
  (->> ^java.net.URLClassLoader (ClassLoader/getSystemClassLoader)
       (.getURLs)
       (map #(.getFile ^java.net.URL %))
       (filter #(.endsWith ^String % ".jar"))
       (filter #(not (or (.contains ^String % "org/ow2/asm/asm-all")
                         (.contains ^String % "org/clojure/core.match")
                         (.contains ^String % "org/clojure/clojure")
                         )))))

(let [init-capacity (* 100 1024)
      buffer-size 1024]
  (defn ^:private ^"[B" read-stream [^InputStream is]
    (let [buffer (byte-array buffer-size)]
      (with-open [os (new ByteArrayOutputStream init-capacity)]
        (loop [bytes-read (.read is buffer 0 buffer-size)]
          (when (not= -1 bytes-read)
            (do (.write os buffer 0 bytes-read)
              (recur (.read is buffer 0 buffer-size)))))
        (.toByteArray os)))))

(defn ^:private add-jar! [^File jar-file seen ^JarOutputStream out]
  (with-open [is (->> jar-file (new FileInputStream) (new JarInputStream))]
    (loop [^JarEntry entry (.getNextJarEntry is)
           seen seen]
      (if entry
        (let [entry-name (.getName entry)]
          (if (and (not (.isDirectory entry))
                   (not (.startsWith entry-name "META-INF/maven/"))
                   (not (contains? seen entry-name)))
            (let [entry-data (read-stream is)]
              (doto out
                (.putNextEntry (doto entry (.setCompressedSize -1)))
                (.write entry-data 0 (alength entry-data))
                (.flush)
                (.closeEntry))
              (recur (.getNextJarEntry is)
                     (conj seen entry-name)))
            (recur (.getNextJarEntry is)
                   seen)))
        seen
        ))))

;; [Resources]
(defn package
  "(-> Text (List Text) Null)"
  [module resources-dirs]
  (with-open [out (new JarOutputStream (->> &&/output-package (new File) (new FileOutputStream)) (manifest module))]
    (do (doseq [$group (.listFiles (new File &&/output-dir))]
          (write-module! $group out))
      (write-resources! out resources-dirs)
      (->> (fetch-available-jars)
           (filter #(and (not (.endsWith ^String % "luxc.jar"))
                         (not (.endsWith ^String % "tools.nrepl-0.2.3.jar"))
                         (not (.endsWith ^String % "clojure-complete-0.2.3.jar"))
                         (not (.endsWith ^String % "clojure-1.6.0.jar"))
                         (not (.endsWith ^String % "core.match-0.2.1.jar"))))
           (reduce (fn [s ^String j] (add-jar! (new File ^String j) s out))
                   #{"META-INF/MANIFEST.MF"}))
      nil)
    ))
