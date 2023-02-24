# Develop
```
cd ~/lux/lux-c++/ \
&& lux clean \
&& lux with python auto build
```

# Build

```
cd ~/lux/lux-c++/ \
&& lux clean \
&& lux with c++ auto build

## Build JVM-based compiler
cd ~/lux/lux-c++/ \
&& lux clean \
&& lux with jvm build \
&& mv target/program.jar jvm_based_compiler.jar

## Use JVM-based compiler to produce a c++/Node-based compiler.
cd ~/lux/lux-c++/ \
&& lux clean \
&& time java -jar jvm_based_compiler.jar build --source ~/lux/lux-c++/source --target ~/lux/lux-c++/target --module program --program _ \
&& mv target/program.c++ node_based_compiler.c++

## Use C++/Node-based compiler to produce another c++/Node-based compiler.
cd ~/lux/lux-c++/ \
&& lux clean \
&& node --stack_size=8192 node_based_compiler.c++ build --source ~/lux/lux-c++/source --target ~/lux/lux-c++/target --module program --program _
```

# Try

```
cd ~/lux/stdlib/ \
&& lux clean \
&& cd ~/lux/lux-c++/ \
&& conda activate WORK \
&& python3 target/program.py build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux --program _

## Compile Lux's Standard Library's tests using a c++/Node-based compiler.
cd ~/lux/stdlib/ \
&& lux clean \
&& node --stack_size=8192 ~/lux/lux-c++/target/program.c++ build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux --program _ \
&& node ~/lux/stdlib/target/program.c++
```

# Deploy

```
cd ~/lux/lux-c++/ \
&& mvn install:install-file -Dfile=target/program.c++ -DgroupId=com.github.luxlang -DartifactId=lux-c++ -Dversion=0.8.0-SNAPSHOT -Dpackaging=c++

cd ~/lux/lux-c++/ && mvn deploy:deploy-file \
-Durl=https://$NEXUS_USERNAME:$NEXUS_PASSWORD@oss.sonatype.org/content/repositories/snapshots/ \
-Dfile=target/program.c++ \
-DgroupId=com.github.luxlang \
-DartifactId=lux-c++ \
-Dversion=0.8.0-SNAPSHOT \
-Dpackaging=c++
```

# Release

```
LUX_PROJECT=lux-c++ && \
LUX_VERSION=0.7.0 && \
cd ~/lux/$LUX_PROJECT/ && \
lux pom && \
mv pom.xml RELEASE/$LUX_PROJECT-$LUX_VERSION.pom && \
mv target/program.c++ RELEASE/$LUX_PROJECT-$LUX_VERSION.c++ && \
cd RELEASE && \
touch README.md && \
zip $LUX_PROJECT-$LUX_VERSION-sources.jar README.md && \
zip $LUX_PROJECT-$LUX_VERSION-javadoc.jar README.md && \
zip $LUX_PROJECT-$LUX_VERSION.jar README.md && \
rm README.md && \
for file in *.*; do gpg -ab $file; done
```

