# Test

```
cd ~/lux/lux-jvm/ && lein clean && lein lux auto test
```

# Build

```
## Use bootstrapping compiler to build new JVM compiler
cd ~/lux/lux-jvm/ \
&& lein clean \
&& lein lux auto build
```

# REPL

```
cd ~/lux/lux-jvm/ && java -jar target/program.jar repl --source ~/lux/stdlib/source --target ~/lux/stdlib/target
```

# Try

```
cd ~/lux/lux-jvm/ && time java -jar target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux
cd ~/lux/lux-jvm/ && java -jar target/program.jar export --source ~/lux/stdlib/source --target ~/lux/stdlib/target

cd ~/lux/stdlib/ \
&& cd ~/lux/lux-jvm/ \
&& time java -jar target/program.jar build --source ~/lux/stdlib/source --library ~/lux/stdlib/target/library.tar --target ~/lux/stdlib/target --module test/lux

## Use new JVM compiler to compile tests for the Standard Library
cd ~/lux/stdlib/ \
&& lein clean \
&& java -jar ~/lux/lux-jvm/target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux \
&& java -jar ~/lux/stdlib/target/program.jar
```

# Deploy

```
cd ~/lux/lux-jvm/ \
&& mvn install:install-file -Dfile=target/program.jar -DgroupId=com.github.luxlang -DartifactId=lux-jvm -Dversion=0.6.5 -Dpackaging=jar

cd ~/lux/lux-jvm/ && mvn deploy:deploy-file \
-Durl=https://USERNAME:PASSWORD@oss.sonatype.org/content/repositories/snapshots/ \
-Dfile=target/program.jar \
-DgroupId=com.github.luxlang \
-DartifactId=lux-jvm \
-Dversion=0.6.1-SNAPSHOT \
-Dpackaging=jar
```

