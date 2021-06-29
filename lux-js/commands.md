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
&& mv target/program.jar jvm_based_compiler.jar

## Use JVM-based compiler to produce a JS/Node-based compiler.
cd ~/lux/lux-js/ \
&& lein clean \
&& java -jar jvm_based_compiler.jar build --source ~/lux/lux-js/source --target ~/lux/lux-js/target --module program \
&& mv target/program.js node_based_compiler.js

## Use JS/Node-based compiler to produce another JS/Node-based compiler.
cd ~/lux/lux-js/ \
&& lein clean \
&& node --stack_size=8192 node_based_compiler.js build --source ~/lux/lux-js/source --target ~/lux/lux-js/target --module program \
&& mv target/program.js lux.js
```

## Try

```
## Compile Lux's Standard Library's tests using a JS/Node-based compiler.
cd ~/lux/stdlib/ \
&& lein clean \
&& node --stack_size=8192 ~/lux/lux-js/lux.js build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux \
&& node ~/lux/stdlib/target/program.js
```

