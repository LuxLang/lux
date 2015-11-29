(defproject lux-jvm "0.3.1"
  :description "The JVM compiler for the Lux programming language."
  :url "https://github.com/LuxLang/lux"
  :license {:name "Mozilla Public License (Version 2.0)"
            :url "https://www.mozilla.org/en-US/MPL/2.0/"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.match "0.2.1"]
                 [org.ow2.asm/asm-all "5.0.3"]]
  :warn-on-reflection true
  :main lux)
