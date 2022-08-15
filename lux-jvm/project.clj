(def version "0.7.0")
(def repo "https://github.com/LuxLang/lux")
(def sonatype "https://oss.sonatype.org")
(def sonatype-releases (str sonatype "/service/local/staging/deploy/maven2/"))
(def sonatype-snapshots (str sonatype "/content/repositories/snapshots/"))

(defproject com.github.luxlang/lux-jvm #=(identity version)
  :description "A JVM compiler for Lux."
  :url ~repo
  :license {:name "Lux License v0.1.2"
            :url ~(str repo "/blob/master/license.txt")}
  :plugins [[com.github.luxlang/lein-luxc ~version]]
  :deploy-repositories [["releases" {:url ~sonatype-releases :creds :gpg}]
                        ["snapshots" {:url ~sonatype-snapshots :creds :gpg}]]
  :pom-addition [:developers [:developer
                              [:name "Eduardo Julian"]
                              [:url "https://github.com/eduardoejp"]]]
  :repositories [["snapshots" ~sonatype-snapshots]]
  :scm {:name "git"
        :url ~(str repo ".git")}

  :dependencies [[com.github.luxlang/lux-bootstrapper ~version]
                 [com.github.luxlang/lux-jvm-function "0.6.5"]
                 ;; [com.github.luxlang/stdlib ~version]
                 ]
  
  :manifest {"lux" ~version}
  :source-paths ["source"]
  :lux {:program ["program" "_"]
        :test ["test/program" "_"]}
  )
