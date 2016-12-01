;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns leiningen.luxc.packager
  (:require [clojure.string :as string]
            [leiningen.core.classpath :as classpath]
            [leiningen.uberjar]
            [leiningen.luxc.utils :as &utils])
  (:import (java.io InputStream
                    File
                    FileInputStream
                    FileOutputStream
                    BufferedInputStream
                    ByteArrayInputStream
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

(defn ^:private manifest
  "(-> Project Text Bool Manifest)"
  [project module includes-android?]
  (doto (new Manifest)
    (-> .getMainAttributes
        (doto (-> (.put Attributes$Name/MAIN_CLASS (str module "._"))
                  (->> (when (not includes-android?))))
          (.put Attributes$Name/MANIFEST_VERSION "1.0")
          (.put (new Attributes$Name "LUX_JAR") "true")
          (-> (.put (new Attributes$Name name) real-v)
              (->> (doseq [[name v] (get project :manifest)
                           :let [real-v (if (string? v) v (v project))]])))))))

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

(defn ^:private write-module!
  "(-> File JarOutputStream Null)"
  [^File file ^JarOutputStream out output-dir]
  (let [output-dir-size (inc (.length output-dir))
        module-name (.substring (.getPath file) output-dir-size)
        inner-files (.listFiles file)
        inner-modules (filter #(.isDirectory ^File %) inner-files)
        inner-classes (filter #(not (.isDirectory ^File %)) inner-files)]
    (doseq [$class inner-classes]
      (write-class! module-name $class out))
    (doseq [$module inner-modules]
      (write-module! $module out output-dir))))

(defn ^:private write-resources!
  "(-> JarOutputStream (List Text) Null)"
  [^JarOutputStream out resources-dirs]
  (doseq [resources-dir resources-dirs
          :let [resources-dir (new File resources-dir)]
          :when (.exists resources-dir)
          ^File res (.listFiles resources-dir)
          :let [buffer (byte-array buffer-size)]]
    (with-open [in (->> res (new FileInputStream) (new BufferedInputStream))]
      (doto out
        (.putNextEntry (new JarEntry (.getName res)))
        (-> (.write buffer 0 bytes-read)
            (->> (when (not= -1 bytes-read))
                 (loop [bytes-read (.read in buffer)])))
        (.flush)
        (.closeEntry))
      )))

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

(defn ^:private add-jar! [^File jar-file project !all-jar-files]
  (with-open [is (->> jar-file (new FileInputStream) (new JarInputStream))]
    (loop [^JarEntry entry (.getNextJarEntry is)]
      (when entry
        (let [entry-name (.getName entry)]
          (if (and (not (.isDirectory entry))
                   (not (.startsWith entry-name "META-INF/maven/"))
                   (not (some (fn [exclusion]
                                (re-find exclusion entry-name))
                              (get project :uberjar-exclusions))))
            (let [entry-data (read-stream is)
                  entry-data (or (some (fn [[pattern [read fuse write]]]
                                         (let [matches? (if (string? pattern)
                                                          (= pattern entry-name)
                                                          (re-find pattern entry-name))]
                                           (when matches?
                                             (let [os (new ByteArrayOutputStream 1024)
                                                   [_data _entry] (get @!all-jar-files entry-name [(byte-array 0) nil])
                                                   _ (write os (fuse (read (new ByteArrayInputStream _data))
                                                                     (read (new ByteArrayInputStream entry-data))))]
                                               (.toByteArray os)))))
                                       (eval (get project :uberjar-merge-with)))
                                 entry-data)]
              (swap! !all-jar-files assoc entry-name [entry-data entry])
              (recur (.getNextJarEntry is)))
            (recur (.getNextJarEntry is))))
        ))))

(def default-manifest-file "./AndroidManifest.xml")

;; [Resources]
(defn package
  "(-> Text (List Text) Null)"
  [project module resources-dirs]
  (let [output-dir (get-in project [:lux :target] &utils/output-dir)
        output-package (str (get-in project [:lux :target] &utils/output-dir) "/"
                            (get project :jar-name &utils/output-package))
        !all-jar-files (atom {})
        includes-android? (boolean (some #(-> % first (= 'com.google.android/android))
                                         (get project :dependencies)))
        project* (-> project
                     (update-in [:dependencies] (fn [_deps]
                                                  ;; Skip the last two,
                                                  ;; because they are:
                                                  ;; tools.nrepl-0.2.12.jar and
                                                  ;; clojure-complete-0.2.4.jar
                                                  ;; and they belong to Leiningen.
                                                  (take (- (count _deps) 2) _deps))))
        deps (->> project*
                  (classpath/resolve-managed-dependencies :dependencies :managed-dependencies)
                  (map #(.getAbsolutePath ^File %)))]
    (do (.delete (new File output-package))
      (with-open [out (new JarOutputStream
                           (->> output-package (new File) (new FileOutputStream))
                           (manifest project module includes-android?))]
        (do (doseq [$group (.listFiles (new File output-dir))]
              (write-module! $group out output-dir))
          (when (not (get-in project [:lux :android]))
            (write-resources! out resources-dirs))
          (doseq [^String file-path deps]
            (add-jar! (new File file-path) project !all-jar-files))
          (doseq [[_ [entry-data entry]] @!all-jar-files]
            (doto out
              (.putNextEntry (doto entry (.setCompressedSize -1)))
              (.write entry-data 0 (alength entry-data))
              (.flush)
              (.closeEntry)))
          nil))
      (when (get-in project [:lux :android])
        (let [output-dex "classes.dex"
              _ (do (.delete (new File output-dex))
                  (&utils/run-process (str "dx --dex --output=" output-dex " " output-package)
                                      (new File (get-in project [:lux :target] &utils/output-dir))
                                      "[DX BEGIN]"
                                      "[DX END]"))
              manifest-path (get-in project [:lux :android :manifest] default-manifest-file)
              sdk-path (get-in project [:lux :android :sdk])
              android-path (str sdk-path "/platforms/android-" (get-in project [:lux :android :version]) "/android.jar")
              _ (assert (.exists (new File android-path))
                        (str "Can't find Android JAR: " android-path))
              output-apk-unaligned (string/replace output-package #"\.jar$" ".apk.unaligned")
              output-apk (string/replace output-package #"\.jar$" ".apk")
              current-working-dir (.getCanonicalPath (new File "."))
              _ (do (&utils/run-process (str "aapt package -f -M " manifest-path " -I " android-path " -F " output-apk-unaligned
                                             (apply str " " (interleave (repeat (count resources-dirs)
                                                                                "-A ")
                                                                        (filter #(.exists (new File %))
                                                                                resources-dirs)))
                                             (apply str " " (interleave (repeat (count resources-dirs)
                                                                                "-S ")
                                                                        (->> (get-in project [:lux :android :resources] ["android-resources"])
                                                                             (map (partial str current-working-dir "/"))
                                                                             (filter #(.exists (new File %)))))))
                                        nil
                                        "[AAPT PACKAGE BEGIN]"
                                        "[AAPT PACKAGE END]")
                  (&utils/run-process (str "aapt add -f " output-apk-unaligned " " output-dex)
                                      (new File (get-in project [:lux :target] &utils/output-dir))
                                      "[AAPT ADD BEGIN]"
                                      "[AAPT ADD END]")
                  (when-let [path (get-in project [:lux :android :keystore :path])]
                    (when-let [alias (get-in project [:lux :android :keystore :alias])]
                      (when-let [password (get-in project [:lux :android :keystore :password])]
                        (&utils/run-process (str "jarsigner -storepass " password " -keystore " path " " output-apk-unaligned " " alias)
                                            nil
                                            "[JARSIGNER BEGIN]"
                                            "[JARSIGNER END]"))))
                  (&utils/run-process (str "zipalign 4 " output-apk-unaligned " " output-apk)
                                      nil
                                      "[ZIPALIGN BEGIN]"
                                      "[ZIPALIGN END]")
                  )
              ]
          nil)))))
