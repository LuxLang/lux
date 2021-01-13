# Clean all

```
cd ~/lux/lux-jvm/ && lein clean && \
cd ~/lux/lux-js/ && lein clean && \
cd ~/lux/lux-python/ && lein clean && \
cd ~/lux/lux-lua/ && lein clean && \
cd ~/lux/lux-ruby/ && lein clean && \
cd ~/lux/lux-php/ && lein clean && \
cd ~/lux/lux-cl/ && lein clean && \
cd ~/lux/lux-scheme/ && lein clean && \
cd ~/lux/lux-r/ && lein clean
```

---

# JVM compiler

## Test

```
cd ~/lux/lux-jvm/ && lein lux auto test
cd ~/lux/lux-jvm/ && lein clean && lein lux auto test
```

## Build

```
cd ~/lux/lux-jvm/ && lein lux auto build
cd ~/lux/lux-jvm/ && lein clean && lein lux auto build
```

## REPL

```
cd ~/lux/lux-jvm/ && java -jar target/program.jar repl --source ~/lux/stdlib/source --target ~/lux/stdlib/target
```

## Try

```
cd ~/lux/lux-jvm/ && time java -jar target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux
cd ~/lux/stdlib/ && lein clean && cd ~/lux/lux-jvm/ && time java -jar target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux
cd ~/lux/stdlib/ && cd ~/lux/lux-jvm/ && time java -jar target/program.jar build --source ~/lux/stdlib/source --library ~/lux/stdlib/target/library.tar --target ~/lux/stdlib/target --module test/lux
cd ~/lux/lux-jvm/ && java -jar target/program.jar export --source ~/lux/stdlib/source --target ~/lux/stdlib/target

cd ~/lux/stdlib/target/ && java -jar program.jar
```

## Deploy

```
cd ~/lux/lux-jvm/ && mvn install:install-file -Dfile=target/program.jar -DgroupId=com.github.luxlang -DartifactId=lux-jvm -Dversion=0.6.0-SNAPSHOT -Dpackaging=jar

cd ~/lux/lux-jvm/ && mvn deploy:deploy-file \
-Durl=https://<username>:<password>@oss.sonatype.org/content/repositories/snapshots/ \
-Dfile=target/program.jar \
-DgroupId=com.github.luxlang \
-DartifactId=lux-jvm \
-Dversion=0.6.0-SNAPSHOT \
-Dpackaging=jar
```

---

# JavaScript compiler

## Test

```
cd ~/lux/lux-js/ && lein lux auto test
cd ~/lux/lux-js/ && lein clean && lein lux auto test
```

## Build

```
cd ~/lux/lux-js/ && lein lux auto build
cd ~/lux/lux-js/ && lein clean && lein lux auto build

## Build JVM-based compiler
cd ~/lux/lux-js/ \
&& lein clean \
&& lein lux build \
&& mv target/program.jar program.jar

## Use JVM-based compiler to produce a JS/Node-based compiler.
cd ~/lux/lux-js/ \
&& lein clean \
&& time java -jar program.jar build --source ~/lux/lux-js/source --target ~/lux/lux-js/target --module program \
&& mv target/program.js program.js

## Use JS/Node-based compiler to produce another JS/Node-based compiler.
cd ~/lux/lux-js/ \
&& lein clean \
&& cd ~/lux/lux-js/ \
&& time node --stack_size=8192 _program.js build --source ~/lux/lux-js/source --target ~/lux/lux-js/target --module program \
&& mv target/program.js program.js
```

## Try

```
cd ~/lux/stdlib/ && lein clean && cd ~/lux/lux-js/ && time java -jar program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux
cd ~/lux/stdlib/target/ && node program.js

## Compile Lux's Standard Library's tests using a JS/Node-based compiler.
cd ~/lux/stdlib/ \
&& lein clean \
&& cd ~/lux/lux-js/ \
&& time node --stack_size=8192 program.js build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux
```

---

# Python compiler

## Test

```
cd ~/lux/lux-python/ && lein lux auto test
cd ~/lux/lux-python/ && lein clean && lein lux auto test
```

## Build

```
## Develop
cd ~/lux/lux-python/ \
&& lein clean \
&& lein lux auto build

## Build JVM-based compiler
cd ~/lux/lux-python/ \
&& lein clean \
&& lein lux build \
&& mv target/program.jar program.jar

## Use JVM-based compiler to produce a Python-based compiler.
cd ~/lux/lux-python/ \
&& lein clean \
&& time java -jar program.jar build --source ~/lux/lux-python/source --target ~/lux/lux-python/target --module program \
&& mv target/program.py program.py
```

## Try

```
cd ~/lux/lux-python/ && java -jar target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux
```

---

# Lua compiler

## Test

```
cd ~/lux/lux-lua/ && lein lux auto test
cd ~/lux/lux-lua/ && lein clean && lein lux auto test
```

## Build

```
cd ~/lux/lux-lua/ && lein lux auto build
cd ~/lux/lux-lua/ && lein clean && lein lux auto build
```

## Try

```
cd ~/lux/lux-lua/ && java -jar target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux
```

---

# Ruby compiler

## Test

```
cd ~/lux/lux-ruby/ && lein lux auto test
cd ~/lux/lux-ruby/ && lein clean && lein lux auto test
```

## Build

```
cd ~/lux/lux-ruby/ && lein lux auto build
cd ~/lux/lux-ruby/ && lein clean && lein lux auto build
```

## Try

```
cd ~/lux/lux-ruby/ && java -jar target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux
```

---

# PHP compiler

## Test

```
cd ~/lux/lux-php/ && lein lux auto test
cd ~/lux/lux-php/ && lein clean && lein lux auto test
```

## Build

```
cd ~/lux/lux-php/ && lein lux auto build
cd ~/lux/lux-php/ && lein clean && lein lux auto build
```

## Try

```
cd ~/lux/lux-php/ && java -jar target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux
```

---

# Common Lisp compiler

## Test

```
cd ~/lux/lux-cl/ && lein lux auto test
cd ~/lux/lux-cl/ && lein clean && lein lux auto test
```

## Build

```
cd ~/lux/lux-cl/ && lein lux auto build
cd ~/lux/lux-cl/ && lein clean && lein lux auto build
```

## Try

```
cd ~/lux/lux-cl/ && java -jar target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux
```

---

# Scheme compiler

## Test

```
cd ~/lux/lux-scheme/ && lein lux auto test
cd ~/lux/lux-scheme/ && lein clean && lein lux auto test
```

## Build

```
cd ~/lux/lux-scheme/ && lein lux auto build
cd ~/lux/lux-scheme/ && lein clean && lein lux auto build
```

## Try

```
cd ~/lux/lux-scheme/ && java -jar target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux
```

---

# R compiler

## Test

```
cd ~/lux/lux-r/ && lein lux auto test
cd ~/lux/lux-r/ && lein clean && lein lux auto test
```

## Build

```
cd ~/lux/lux-r/ && lein lux auto build
cd ~/lux/lux-r/ && lein clean && lein lux auto build
```

## REPL

```
cd ~/lux/lux-r/ && java -jar target/program.jar repl --source ~/lux/stdlib/source --target ~/lux/stdlib/target
```

## Try

```
cd ~/lux/lux-r/ && time java -jar target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux
cd ~/lux/stdlib/ && lein clean && cd ~/lux/lux-r/ && time java -jar target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux
cd ~/lux/stdlib/ && lein clean && cd ~/lux/lux-r/ && time java -jar target/program.jar build --source ~/lux/stdlib/source --library ~/lux/stdlib/target/library.tar --target ~/lux/stdlib/target --module test/lux
cd ~/lux/lux-r/ && java -jar target/program.jar export --source ~/lux/stdlib/source --target ~/lux/stdlib/target

cd ~/lux/stdlib/target/ && java -jar program.jar
```

---

# Compiler trial

## Build

```
cd ~/lux/lux-trial/ && lein clean && lein lux build
cd ~/lux/lux-trial/target/ && java -jar program.jar
```

