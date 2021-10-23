(def version "0.6.3")
(def repo "https://github.com/LuxLang/lux")
(def sonatype-releases "https://oss.sonatype.org/service/local/staging/deploy/maven2/")
(def sonatype-snapshots "https://oss.sonatype.org/content/repositories/snapshots/")

(def asm_version "5.0.4")
(def rembulan_version "0.1")

(defproject com.github.luxlang/lux-lua #=(identity version)
  :description "A Lua compiler for Lux."
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
                 
                 [org.ow2.asm/asm ~asm_version]
                 [org.ow2.asm/asm-commons ~asm_version]
                 [org.ow2.asm/asm-analysis ~asm_version]
                 [org.ow2.asm/asm-tree ~asm_version]
                 [org.ow2.asm/asm-util ~asm_version]
                 
                 [com.github.luxlang/rembulan-runtime ~rembulan_version]
                 [com.github.luxlang/rembulan-stdlib ~rembulan_version]
                 [com.github.luxlang/rembulan-compiler ~rembulan_version]]

  :manifest {"lux" ~version}
  :source-paths ["source"]
  :lux {:program "program"}
  )
