(defproject com.github.luxlang/luxc-jvm "0.3.3"
  :description "The JVM compiler for the Lux programming language."
  :url "https://github.com/LuxLang/lux"
  :license {:name "Mozilla Public License (Version 2.0)"
            :url "https://www.mozilla.org/en-US/MPL/2.0/"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.match "0.2.1"]
                 [org.ow2.asm/asm-all "5.0.3"]]
  :warn-on-reflection true
  :main lux
  :repositories [["snapshots" "https://oss.sonatype.org/content/repositories/snapshots/"]
                 ["releases" "https://oss.sonatype.org/service/local/staging/deploy/maven2/"]])
