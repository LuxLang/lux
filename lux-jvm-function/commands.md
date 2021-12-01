# Deploy

```
cd ~/lux/lux-jvm-function/ \
&& mvn install:install-file -Dfile=dependency.jar -DgroupId=com.github.luxlang -DartifactId=lux-jvm-function -Dversion=0.6.5 -Dpackaging=jar

cd ~/lux/lux-jvm/ && mvn deploy:deploy-file \
-Durl=https://USERNAME:PASSWORD@oss.sonatype.org/content/repositories/snapshots/ \
-Dfile=target/program.jar \
-DgroupId=com.github.luxlang \
-DartifactId=lux-jvm \
-Dversion=0.6.1-SNAPSHOT \
-Dpackaging=jar
```

