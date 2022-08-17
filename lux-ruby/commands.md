# Test

```
cd ~/lux/lux-ruby/ && lein lux auto test
cd ~/lux/lux-ruby/ && lein clean && lein lux auto test
```

# Build

```
## Develop
cd ~/lux/lux-ruby/ \
&& lux clean \
&& lux auto build
```

# Try

```
## Compile Lux's Standard Library's tests using a JVM-based compiler.
cd ~/lux/stdlib/ \
&& lein clean \
&& java -jar ~/lux/lux-ruby/target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux --program _ \
&& RUBY_THREAD_VM_STACK_SIZE=15700000 ruby ~/lux/stdlib/target/program/main.rb
```

# Deploy

```
cd ~/lux/lux-ruby/ \
&& mvn install:install-file -Dfile=target/program.jar -DgroupId=com.github.luxlang -DartifactId=lux-ruby -Dversion=0.8.0-SNAPSHOT -Dpackaging=jar

cd ~/lux/lux-ruby/ && mvn deploy:deploy-file \
-Durl=https://$NEXUS_USERNAME:$NEXUS_PASSWORD@oss.sonatype.org/content/repositories/snapshots/ \
-Dfile=target/program.jar \
-DgroupId=com.github.luxlang \
-DartifactId=lux-ruby \
-Dversion=0.8.0-SNAPSHOT \
-Dpackaging=jar
```

# Release

```
LUX_PROJECT=lux-ruby && \
LUX_VERSION=0.7.0 && \
cd ~/lux/$LUX_PROJECT/ && \
lux pom && \
mv pom.xml RELEASE/$LUX_PROJECT-$LUX_VERSION.pom && \
mv target/program.jar RELEASE/$LUX_PROJECT-$LUX_VERSION.jar && \
cd RELEASE && \
touch README.md && \
zip $LUX_PROJECT-$LUX_VERSION-sources.jar README.md && \
zip $LUX_PROJECT-$LUX_VERSION-javadoc.jar README.md && \
rm README.md && \
for file in *.*; do gpg -ab $file; done
```

