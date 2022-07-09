(def version "0.6.6-SNAPSHOT")

(defproject com.github.luxlang/lein-luxc #=(identity version)
  :description "The Leiningen plugin for the Lux programming language."
  :url "https://github.com/LuxLang/lux"
  :license {:name "Lux License v0.1.2"
            :url "https://github.com/LuxLang/lux/blob/master/license.txt"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :deploy-repositories [["releases" {:url "https://oss.sonatype.org/service/local/staging/deploy/maven2/"
                                     :creds :gpg}]
                        ["snapshots" {:url "https://oss.sonatype.org/content/repositories/snapshots/"
                                      :creds :gpg}]]
  :pom-addition [:developers [:developer
                              [:name "Eduardo Julian"]
                              [:url "https://github.com/eduardoejp"]]]
  :scm {:name "git"
        :url "https://github.com/LuxLang/lux.git"}
  
  :eval-in :leiningen)
