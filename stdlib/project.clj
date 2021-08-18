(def version "0.6.0-SNAPSHOT")
(def repo "https://github.com/LuxLang/lux")
(def sonatype-releases "https://oss.sonatype.org/service/local/staging/deploy/maven2/")
(def sonatype-snapshots "https://oss.sonatype.org/content/repositories/snapshots/")

(defproject com.github.luxlang/stdlib #=(identity version)
  :url ~repo
  :license {:name "Lux License v0.1.1"
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
  :dependencies [[com.github.luxlang/lux-bootstrapper ~version]]
  :profiles {:bibliotheca {:description "Standard library for the Lux programming language."
                           :lux {:test "test/lux"}}
             :aedifex {:description "A build system/tool made exclusively for Lux."
                       :lux {:program "program/aedifex"
                             :test "test/aedifex"}}
             :scriptum {:description "A documentation generator for Lux code."
                        :lux {:program "program/scriptum"}}}
  )
