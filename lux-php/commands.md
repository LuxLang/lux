# PHP compiler

## Test

```
cd ~/lux/lux-php/ && lein lux auto test
cd ~/lux/lux-php/ && lein clean && lein lux auto test
```

## Build

```
## Develop
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
&& time java -jar ~/lux/lux-php/jvm_based_compiler.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux

php -f ~/lux/stdlib/target/program.php
```

