# Test

```
cd ~/lux/lux-js/ && lein lux auto test
cd ~/lux/lux-js/ && lein clean && lein lux auto test
```

# Build

```
cd ~/lux/lux-js/ \
&& lux clean \
&& lux with js auto build

cd ~/lux/lux-js/ \
&& lux clean \
&& lux with js build \
&& mv target/program.js lux.js

## Develop
cd ~/lux/lux-js/ \
&& lein clean \
&& lein lux auto build

## Build JVM-based compiler
cd ~/lux/lux-js/ \
&& lein clean \
&& lein lux build \
&& mv target/program.jar jvm_based_compiler.jar

## Use JVM-based compiler to produce a JS/Node-based compiler.
## @ library/lux/data/text TODO: Comment/turn-off when generating a JS compiler using a JVM-based compiler because Nashorn's implementation of "replaceAll" is incorrect. 
cd ~/lux/lux-js/ \
&& lein clean \
&& time java -jar jvm_based_compiler.jar build --source ~/lux/lux-js/source --target ~/lux/lux-js/target --module program \
&& mv target/program.js node_based_compiler.js

## Use JS/Node-based compiler to produce another JS/Node-based compiler.
cd ~/lux/lux-js/ \
&& lein clean \
&& node --stack_size=8192 node_based_compiler.js build --source ~/lux/lux-js/source --target ~/lux/lux-js/target --module program \
&& mv target/program.js lux.js
```

# Try

```
## Compile Lux's Standard Library's tests using a JS/Node-based compiler.
cd ~/lux/stdlib/ \
&& lein clean \
&& node --stack_size=8192 ~/lux/lux-js/target/program.js build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux \
&& node ~/lux/stdlib/target/program.js
```

# Deploy

```
cd ~/lux/lux-js/ \
&& mvn install:install-file -Dfile=target/program.js -DgroupId=com.github.luxlang -DartifactId=lux-js -Dversion=0.6.5-SNAPSHOT -Dpackaging=js
```

