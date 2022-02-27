# Test

```
cd ~/lux/lux-lua/ && lein lux auto test
cd ~/lux/lux-lua/ && lein clean && lein lux auto test
```

# Build

```
## Develop
## NOTE: Must set lux/control/concurrency/thread.parallelism = 1 before compiling to make sure Rembulan doesn't cause trouble.
cd ~/lux/lux-lua/ \
&& lux clean \
&& lux auto build
```

# Try

```
## Compile Lux's Standard Library's tests using a JVM-based compiler.
cd ~/lux/stdlib/ \
&& lein clean \
&& java -jar ~/lux/lux-lua/target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux \
&& ~/lua-5.4.2/install/bin/lua ~/lux/stdlib/target/program.lua
```

# Deploy

```
cd ~/lux/lux-lua/ \
&& mvn install:install-file -Dfile=target/program.jar -DgroupId=com.github.luxlang -DartifactId=lux-lua -Dversion=0.6.6-SNAPSHOT -Dpackaging=jar
```

