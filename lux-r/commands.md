# R compiler

## Test

```
cd ~/lux/lux-r/ && lein lux auto test
cd ~/lux/lux-r/ && lein clean && lein lux auto test
```

## Build

```
## Develop
cd ~/lux/lux-r/ \
&& lein clean \
&& lein lux auto build
```

## REPL

```
cd ~/lux/lux-r/ && java -jar target/program.jar repl --source ~/lux/stdlib/source --target ~/lux/stdlib/target
```

## Try

```
## Compile Lux's Standard Library's tests using a JVM-based compiler.
cd ~/lux/stdlib/ \
&& lein clean \
&& time java -jar ~/lux/lux-r/target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux
```

