(def version "0.6.0-SNAPSHOT")
(def repo "https://github.com/LuxLang/lux")
(def sonatype-releases "https://oss.sonatype.org/service/local/staging/deploy/maven2/")
(def sonatype-snapshots "https://oss.sonatype.org/content/repositories/snapshots/")

(defproject com.github.luxlang/licentia #=(identity version)
  :description "A program for producing free/open-source/reciprocal licenses."

  :url ~(str repo "/licentia")
  :license {:name "Lux License v0.1.1"
            :url ~(str repo "/blob/master/license.txt")}

  :repositories [["releases" ~sonatype-releases]
                 ["snapshots" ~sonatype-snapshots]]
  :deploy-repositories [["releases" {:url ~sonatype-releases :creds :gpg}]
                        ["snapshots" {:url ~sonatype-snapshots :creds :gpg}]]
  
  :plugins [[com.github.luxlang/lein-luxc ~version]]
  
  :dependencies [[com.github.luxlang/lux-jvm ~version]
                 [com.github.luxlang/stdlib ~version]]
  
  :pom-addition [:developers [:developer
                              [:name "Eduardo Julian"]
                              [:url "https://github.com/eduardoejp"]]]
  :scm {:name "git"
        :url ~(str repo ".git")}
  :manifest {"lux" ~version}
  
  :source-paths ["source"]
  :lux {:program "program/licentia"
        :test "test/licentia"}
  )
