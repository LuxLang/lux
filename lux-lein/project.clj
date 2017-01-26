(defproject com.github.luxlang/lein-luxc "0.6.0-SNAPSHOT"
  :description "The Leiningen plugin for the Lux programming language."
  :url "https://github.com/LuxLang/lein-luxc"
  :license {:name "Mozilla Public License"
            :url "https://www.mozilla.org/en-US/MPL/2.0/"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.github.luxlang/luxc-jvm "0.6.0-SNAPSHOT"]
                 [com.github.luxlang/stdlib "0.6.0-SNAPSHOT"]]
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
