# Test

```
cd ~/lux/lux-python/ && lein lux auto test
cd ~/lux/lux-python/ && lein clean && lein lux auto test
```

# Build

```
## Develop
cd ~/lux/lux-python/ \
&& lux clean \
&& lux auto build

## Build JVM-based compiler
cd ~/lux/lux-python/ \
&& lux clean \
&& lux build \
&& mv target/program.jar jvm_based_compiler.jar
```

# Try

```
## Compile Lux's Standard Library's tests using a JVM-based compiler.
cd ~/lux/stdlib/ \
&& lein clean \
&& java -jar ~/lux/lux-python/target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux \
&& python3 ~/lux/stdlib/target/program.py
```

# Deploy

```
cd ~/lux/lux-python/ \
&& mvn install:install-file -Dfile=target/program.jar -DgroupId=com.github.luxlang -DartifactId=lux-python -Dversion=0.6.0-SNAPSHOT -Dpackaging=jar
```

