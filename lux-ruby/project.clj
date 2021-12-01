(def version "0.6.5")
(def repo "https://github.com/LuxLang/lux")
(def sonatype-releases "https://oss.sonatype.org/service/local/staging/deploy/maven2/")
(def sonatype-snapshots "https://oss.sonatype.org/content/repositories/snapshots/")

(defproject com.github.luxlang/lux-ruby #=(identity version)
  :description "A Ruby compiler for Lux."
  :url ~repo
  :license {:name "Lux License v0.1.2"
            :url ~(str repo "/blob/master/license.txt")}
  :scm {:name "git"
        :url ~(str repo ".git")}
  :pom-addition [:developers [:developer
                              [:name "Eduardo Julian"]
                              [:url "https://github.com/eduardoejp"]]]
  
  :repositories [["snapshots" ~sonatype-snapshots]]
  :deploy-repositories [["releases" {:url ~sonatype-releases :creds :gpg}]
                        ["snapshots" {:url ~sonatype-snapshots :creds :gpg}]]
  
  :plugins [[com.github.luxlang/lein-luxc ~version]]
  :dependencies [[com.github.luxlang/lux-bootstrapper ~version]
                 ;; [com.github.luxlang/stdlib ~version]
                 
                 [org.jruby/jruby-complete "9.2.15.0"]]

  :manifest {"lux" ~version}
  :source-paths ["source"]
  :lux {:program "program"}
  )
