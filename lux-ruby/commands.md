# Test

```
cd ~/lux/lux-ruby/ && lein lux auto test
cd ~/lux/lux-ruby/ && lein clean && lein lux auto test
```

# Build

```
## Develop
cd ~/lux/lux-ruby/ \
&& lein clean \
&& lein lux auto build

## Build JVM-based compiler
cd ~/lux/lux-ruby/ \
&& lein clean \
&& lein lux build \
&& mv target/program.jar jvm_based_compiler.jar
```

# Try

```
## Compile Lux's Standard Library's tests using a JVM-based compiler.
cd ~/lux/stdlib/ \
&& lein clean \
&& java -jar ~/lux/lux-ruby/jvm_based_compiler.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux \
&& RUBY_THREAD_VM_STACK_SIZE=15700000 ruby ~/lux/stdlib/target/program.rb
```

# Deploy

```
cd ~/lux/lux-ruby/ \
&& mvn install:install-file -Dfile=jvm_based_compiler.jar -DgroupId=com.github.luxlang -DartifactId=lux-ruby -Dversion=0.6.0-SNAPSHOT -Dpackaging=jar
```

