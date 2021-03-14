# PHP compiler

## Test

```
cd ~/lux/lux-php/ && lein lux auto test
cd ~/lux/lux-php/ && lein clean && lein lux auto test
```

## Build

```
## Develop
## NOTE: Must set lux/control/concurrency/thread.parallelism = 1 before compiling to make sure JPHP doesn't cause trouble.
cd ~/lux/lux-php/ \
&& lein clean \
&& lein lux auto build

## Build JVM-based compiler
## NOTE: Must set lux/control/concurrency/thread.parallelism = 1 before compiling to make sure JPHP doesn't cause trouble.
cd ~/lux/lux-php/ \
&& lein clean \
&& lein lux build \
&& mv target/program.jar jvm_based_compiler.jar
```

## Try

```
## Compile Lux's Standard Library's tests using a JVM-based compiler.
cd ~/lux/stdlib/ \
&& lein clean \
&& time java -jar ~/lux/lux-php/target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux

php -f ~/lux/stdlib/target/program.php
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

