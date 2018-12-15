(defproject luxdoc "0.5.0"
  :description "Documentation generator for the Lux programming language's ."
  :url "https://github.com/LuxLang/luxdoc"
  :license {:name "Lux License v0.1"
            :url "https://github.com/LuxLang/lux/blob/master/license.txt"}
  :plugins [[com.github.luxlang/lein-luxc "0.5.0"]]
  :dependencies []
  :repositories [["snapshots" "https://oss.sonatype.org/content/repositories/snapshots/"]
                 ["releases" "https://oss.sonatype.org/service/local/staging/deploy/maven2/"]]
  :source-paths ["source"]
  :lux {:program "program"})
