(defproject lux-jvm "0.3.0"
  :description "The JVM compiler for the Lux programming language."
  :url "https://github.com/LuxLang/lux"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.match "0.2.1"]
                 [org.ow2.asm/asm-all "5.0.3"]]
  :warn-on-reflection true
  :main lux)
