# Lua compiler

## Test

```
cd ~/lux/lux-lua/ && lein lux auto test
cd ~/lux/lux-lua/ && lein clean && lein lux auto test
```

## Build

```
## Develop
cd ~/lux/lux-lua/ \
&& lein clean \
&& lein lux auto build

## Build JVM-based compiler
## NOTE: Must set lux/control/concurrency/thread.parallelism = 1 before compiling to make sure Rembulan doesn't cause trouble.
cd ~/lux/lux-lua/ \
&& lein clean \
&& lein lux build \
&& mv target/program.jar jvm_based_compiler.jar
```

## Try

```
## Compile Lux's Standard Library's tests using a JVM-based compiler.
cd ~/lux/stdlib/ \
&& lein clean \
&& java -jar ~/lux/lux-lua/jvm_based_compiler.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux \
&& ~/lua-5.4.2/install/bin/lua ~/lux/stdlib/target/program.lua
```

