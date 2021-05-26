# Common Lisp compiler

## Test

```
cd ~/lux/lux-cl/ && lein lux auto test
cd ~/lux/lux-cl/ && lein clean && lein lux auto test
```

## Build

```
## Develop
cd ~/lux/lux-cl/ \
&& lein clean \
&& lein lux auto build
```

## Try

```
## Compile Lux's Standard Library's tests using a JVM-based compiler.
cd ~/lux/stdlib/ \
&& lein clean \
&& time java -jar ~/lux/lux-cl/target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux
```
