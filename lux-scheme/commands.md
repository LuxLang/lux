# Scheme compiler

## Test

```
cd ~/lux/lux-scheme/ && lein lux auto test
cd ~/lux/lux-scheme/ && lein clean && lein lux auto test
```

## Build

```
## Develop
## NOTE: Must set lux/control/concurrency/thread.parallelism = 1 before compiling to make sure Kawa doesn't cause trouble.
cd ~/lux/lux-scheme/ \
&& lein clean \
&& lein lux auto build
```

## Try

```
## Compile Lux's Standard Library's tests using a JVM-based compiler.
cd ~/lux/stdlib/ \
&& lein clean \
&& time java -jar ~/lux/lux-scheme/target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux

clear && time kawa ~/lux/stdlib/target/program.scm
```

