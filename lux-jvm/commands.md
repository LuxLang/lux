# Build

```
cd ~/lux/lux-jvm/ \
&& lux clean \
&& lux auto build
```

# REPL

```
cd ~/lux/lux-jvm/ && java -jar target/program.jar repl --source ~/lux/stdlib/source --target ~/lux/stdlib/target
```

# Try

```
cd ~/lux/lux-jvm/ && time java -jar target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux --program _
cd ~/lux/lux-jvm/ && java -jar target/program.jar export --source ~/lux/stdlib/source --target ~/lux/stdlib/target

cd ~/lux/stdlib/ \
&& cd ~/lux/lux-jvm/ \
&& time java -jar target/program.jar build --source ~/lux/stdlib/source --library ~/lux/stdlib/target/library.tar --target ~/lux/stdlib/target --module test/lux --program _

## Use new JVM compiler to compile tests for the Standard Library
cd ~/lux/stdlib/ \
&& lux clean \
&& java -jar ~/lux/lux-jvm/target/program.jar build --host_dependency ~/.m2/repository/com/github/luxlang/lux-jvm-function/0.6.5/lux-jvm-function-0.6.5.jar --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux --program _ \
&& java -jar ~/lux/stdlib/target/program.jar
```

# Deploy

```
cd ~/lux/lux-jvm/ \
&& mvn install:install-file -Dfile=target/program.jar -DgroupId=com.github.luxlang -DartifactId=lux-jvm -Dversion=0.9.0-SNAPSHOT -Dpackaging=jar

cd ~/lux/lux-jvm/ && mvn deploy:deploy-file \
-Durl=https://$NEXUS_USERNAME:$NEXUS_PASSWORD@oss.sonatype.org/content/repositories/snapshots/ \
-Dfile=target/program.jar \
-DgroupId=com.github.luxlang \
-DartifactId=lux-jvm \
-Dversion=0.9.0-SNAPSHOT \
-Dpackaging=jar
```

# Release

```
LUX_VERSION=0.8.0 && \
cd ~/lux/lux-jvm/ && \
lein pom && \
mv pom.xml RELEASE/lux-jvm-$LUX_VERSION.pom && \
mv target/program.jar RELEASE/lux-jvm-$LUX_VERSION.jar && \
cd RELEASE && \
touch README.md && \
zip lux-jvm-$LUX_VERSION-sources.jar README.md && \
zip lux-jvm-$LUX_VERSION-javadoc.jar README.md && \
rm README.md && \
for file in *.*; do gpg -ab $file; done
```

