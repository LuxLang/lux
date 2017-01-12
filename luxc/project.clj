(defproject com.github.luxlang/luxc-jvm "0.6.0-SNAPSHOT"
  :min-lein-version  "2.1.0" ;; 2.1.0 introduced jar classifiers
  :description "The JVM compiler for the Lux programming language."
  :url "https://github.com/LuxLang/lux"
  :license {:name "Mozilla Public License (Version 2.0)"
            :url "https://www.mozilla.org/en-US/MPL/2.0/"}
  :deploy-repositories [["releases" {:url "https://oss.sonatype.org/service/local/staging/deploy/maven2/"
                                     :creds :gpg}]
                        ["snapshots" {:url "https://oss.sonatype.org/content/repositories/snapshots/"
                                      :creds :gpg}]]
  :pom-addition [:developers [:developer
                              [:name "Eduardo Julian"]
                              [:url "https://github.com/eduardoejp"]]]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.match "0.2.1"]
                 [org.ow2.asm/asm-all "5.0.3"]]
  :warn-on-reflection true
  :main lux
  :repositories [["snapshots" "https://oss.sonatype.org/content/repositories/snapshots/"]
                 ["releases" "https://oss.sonatype.org/service/local/staging/deploy/maven2/"]]
  :source-paths ["src"]

  :scm {:name "git"
        :url "https://github.com/LuxLang/lux.git"}

  :classifiers {:sources {:resource-paths ["src"]}
                :javadoc {:resource-paths ["src"]}}
  
  :aot [lux]
  
  :jvm-opts ^:replace ["-server" "-Xms2048m" "-Xmx2048m"
                       "-XX:+OptimizeStringConcat"]
  )
