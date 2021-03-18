# Scheme compiler

## Test

```
cd ~/lux/lux-scheme/ && lein lux auto test
cd ~/lux/lux-scheme/ && lein clean && lein lux auto test
```

## Build

```
## Develop
cd ~/lux/lux-scheme/ \
&& lein clean \
&& lein lux auto build
```

## Try

```
cd ~/lux/lux-scheme/ && java -jar target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux
```

