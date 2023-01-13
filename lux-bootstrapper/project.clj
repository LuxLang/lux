;; This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;; If a copy of the MPL was not distributed with this file, You can obtain one at https://mozilla.org/MPL/2.0/.

(def version "0.8.0-SNAPSHOT")

(defproject com.github.luxlang/lux-bootstrapper #=(identity version)
  :min-lein-version  "2.1.0" ;; 2.1.0 introduced jar classifiers
  :description "The JVM (bootstrapping) compiler for the Lux programming language."
  :url "https://github.com/LuxLang/lux"
  :license {:name "Lux License v0.1.2"
            :url "https://github.com/LuxLang/lux/blob/master/license.txt"}
  :deploy-repositories [["releases" {:url "https://oss.sonatype.org/service/local/staging/deploy/maven2/"
                                     :creds :gpg}]
                        ["snapshots" {:url "https://oss.sonatype.org/content/repositories/snapshots/"
                                      :creds :gpg}]]
  :pom-addition [:developers [:developer
                              [:name "Eduardo Julian"]
                              [:url "https://github.com/eduardoejp"]]]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.match "0.2.1"]

                 [com.github.luxlang/lux-jvm-function "0.6.5"]
                 
                 ;; Prefer when building JS compiler.
                 [org.ow2.asm/asm "7.3.1"]
                 [org.ow2.asm/asm-commons "7.3.1"]
                 [org.ow2.asm/asm-analysis "7.3.1"]
                 [org.ow2.asm/asm-tree "7.3.1"]
                 [org.ow2.asm/asm-util "7.3.1"]]
  :warn-on-reflection true
  :repositories [["snapshots" "https://oss.sonatype.org/content/repositories/snapshots/"]]
  :source-paths ["src"]

  :scm {:name "git"
        :url "https://github.com/LuxLang/lux.git"}

  :main lux
  :profiles {:uberjar {:classifiers {:sources {:resource-paths ["src"]}
                                     :javadoc {:resource-paths ["src"]}}
                       :aot [lux]}}

  :jvm-opts ^:replace ["-server" "-Xms2048m" "-Xmx2048m"
                       "-Xss16m"
                       "-XX:+OptimizeStringConcat"]
  )
