(def version "0.6.0-SNAPSHOT")
(def repo "https://github.com/LuxLang/lux")
(def sonatype-releases "https://oss.sonatype.org/service/local/staging/deploy/maven2/")
(def sonatype-snapshots "https://oss.sonatype.org/content/repositories/snapshots/")

(defproject com.github.luxlang/stdlib #=(identity version)
  :url ~repo
  :license {:name "Lux License v0.1"
            :url ~(str repo "/blob/master/license.txt")}
  :plugins [[com.github.luxlang/lein-luxc ~version]]
  :deploy-repositories [["releases" {:url ~sonatype-releases :creds :gpg}]
                        ["snapshots" {:url ~sonatype-snapshots :creds :gpg}]]
  :pom-addition [:developers [:developer
                              [:name "Eduardo Julian"]
                              [:url "https://github.com/eduardoejp"]]]
  :repositories [["releases" ~sonatype-releases]
                 ["snapshots" ~sonatype-snapshots]]
  :scm {:name "git"
        :url ~(str repo ".git")}

  :manifest {"lux" ~version}
  :source-paths ["source"]
  :dependencies [[com.github.luxlang/luxc-jvm ~version]]
  :profiles {:bibliotheca {:description "Standard library for the Lux programming language."
                           :dependencies []
                           :lux {:test "test/lux"}}
             :aedifex {:description "A build system/tool made exclusively for Lux."
                       :dependencies []
                       :lux {:program "program/aedifex"}}
             :scriptum {:description "A documentation generator for Lux code."
                        :dependencies []
                        :lux {:program "program/scriptum"}}
             :licentia {:description "A program for producing free/open-source/reciprocal licenses."
                        :dependencies []
                        :lux {:program "program/licentia"
                              :test "test/licentia"}}}
  )
