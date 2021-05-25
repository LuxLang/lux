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
cd ~/lux/lux-cl/ && java -jar target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux
```
