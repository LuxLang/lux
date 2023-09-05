# Develop
```
cd ~/lux/lux-js/ \
&& lux clean \
&& lux with js auto build

cd ~/lux/lux-js/ \
&& lux clean \
&& lux with jvm auto build
```

# Build

```
## Build JVM-based compiler
cd ~/lux/lux-js/ \
&& lux clean \
&& lux with jvm build \
&& mv target/program.jar jvm_based_compiler.jar

## Use JVM-based compiler to produce a JS/Node-based compiler.
cd ~/lux/lux-js/ \
&& lux clean \
&& time java -Xss2m -jar jvm_based_compiler.jar build --source ~/lux/lux-js/source --target ~/lux/lux-js/target --module program --program _ \
&& mv target/program.js node_based_compiler.js

## Use JS/Node-based compiler to produce another JS/Node-based compiler.
cd ~/lux/lux-js/ \
&& lux clean \
&& node --stack_size=8192 node_based_compiler.js build --source ~/lux/lux-js/source --target ~/lux/lux-js/target --module program --program _
```

# Try

```
## Compile Lux's Standard Library's tests using a JS/Node-based compiler.
cd ~/lux/stdlib/ \
&& lux clean \
&& node --stack_size=8192 ~/lux/lux-js/target/program.js build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux --program _ \
&& node ~/lux/stdlib/target/program.js
```

# Deploy

```
cd ~/lux/lux-js/ \
&& mvn install:install-file -Dfile=target/program.js -DgroupId=com.github.luxlang -DartifactId=lux-js -Dversion=0.8.0-SNAPSHOT -Dpackaging=js

cd ~/lux/lux-js/ && mvn deploy:deploy-file \
-Durl=https://$NEXUS_USERNAME:$NEXUS_PASSWORD@oss.sonatype.org/content/repositories/snapshots/ \
-Dfile=target/program.js \
-DgroupId=com.github.luxlang \
-DartifactId=lux-js \
-Dversion=0.8.0-SNAPSHOT \
-Dpackaging=js
```

# Release

```
LUX_PROJECT=lux-js && \
LUX_VERSION=0.7.0 && \
cd ~/lux/$LUX_PROJECT/ && \
lux pom && \
mv pom.xml RELEASE/$LUX_PROJECT-$LUX_VERSION.pom && \
mv target/program.js RELEASE/$LUX_PROJECT-$LUX_VERSION.js && \
cd RELEASE && \
touch README.md && \
zip $LUX_PROJECT-$LUX_VERSION-sources.jar README.md && \
zip $LUX_PROJECT-$LUX_VERSION-javadoc.jar README.md && \
zip $LUX_PROJECT-$LUX_VERSION.jar README.md && \
rm README.md && \
for file in *.*; do gpg -ab $file; done
```

