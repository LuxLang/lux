(def version "0.6.0-SNAPSHOT")
(def repo "https://github.com/LuxLang/lux")
(def sonatype-releases "https://oss.sonatype.org/service/local/staging/deploy/maven2/")
(def sonatype-snapshots "https://oss.sonatype.org/content/repositories/snapshots/")

(defproject com.github.luxlang/lux-php #=(identity version)
  :description "A Ruby compiler for Lux."
  :url ~repo
  :license {:name "Lux License v0.1"
            :url ~(str repo "/blob/master/license.txt")}
  :scm {:name "git"
        :url ~(str repo ".git")}
  :pom-addition [:developers [:developer
                              [:name "Eduardo Julian"]
                              [:url "https://github.com/eduardoejp"]]]
  
  :repositories [["releases" ~sonatype-releases]
                 ["snapshots" ~sonatype-snapshots]
                 ["jitpack" "https://jitpack.io"]]
  :deploy-repositories [["releases" {:url ~sonatype-releases :creds :gpg}]
                        ["snapshots" {:url ~sonatype-snapshots :creds :gpg}]]
  
  :plugins [[com.github.luxlang/lein-luxc ~version]]
  :dependencies [[com.github.luxlang/luxc-jvm ~version]
                 [com.github.luxlang/stdlib ~version]
                 ;; PHP 5
                 [org.develnext.jphp/jphp-core "0.9.2"]
                 [org.develnext.jphp/jphp-scripting "0.9.2"]]

  :manifest {"lux" ~version}
  :source-paths ["source"]
  :lux {:program "program"}
  )
