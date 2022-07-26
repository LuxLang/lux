# Standard Library

## Test

```
cd ~/lux/stdlib/ \
&& lein clean \
&& lein with-profile bibliotheca lux auto test

cd ~/lux/stdlib/ \
&& lux clean \
&& lux with jvm with bibliotheca auto test

cd ~/lux/stdlib/ \
&& lux clean \
&& lux with js with bibliotheca auto test

cd ~/lux/stdlib/ \
&& lux clean \
&& lux with lua with bibliotheca auto test

cd ~/lux/stdlib/ \
&& lux clean \
&& lux with python with bibliotheca auto test

cd ~/lux/stdlib/ \
&& lux clean \
&& lux with ruby with bibliotheca auto test
```

## Deploy

```
cd ~/lux/stdlib/ && lein install

cd ~/lux/stdlib/ && mvn install:install-file -Dfile=target/library.tar -DgroupId=com.github.luxlang -DartifactId=stdlib -Dversion=0.6.0-SNAPSHOT -Dpackaging=tar

cd ~/lux/stdlib/ && mvn deploy:deploy-file \
-Durl=https://USERNAME:PASSWORD@oss.sonatype.org/content/repositories/snapshots/ \
-Dfile=library.tar \
-DgroupId=com.github.luxlang \
-DartifactId=stdlib \
-Dversion=0.7.0-SNAPSHOT \
-Dpackaging=tar

cd ~/lux/stdlib/ \
&& lux install

cd ~/lux/stdlib/ \
&& lux deploy snapshots $NEXUS_USERNAME $NEXUS_PASSWORD

cd ~/lux/stdlib/ \
&& lux deploy releases $NEXUS_USERNAME $NEXUS_PASSWORD
```

## Generate documentation

```
cd ~/lux/stdlib/ \
&& lein clean \
&& lein with-profile scriptum lux auto build

cd ~/lux/stdlib/ \
&& lux clean \
&& lux with jvm with scriptum auto test

### JVM
cd ~/lux/stdlib/ \
&& lux clean \
&& lux with jvm with scriptum build \
&& java -jar target/program.jar > ~/lux/documentation/library/standard/jvm.md

### JS
cd ~/lux/stdlib/ \
&& lux clean \
&& lux with js with scriptum build \
&& node ~/lux/stdlib/target/program.js > ~/lux/documentation/library/standard/js.md

### Lua
cd ~/lux/stdlib/ \
&& lux clean \
&& lux with lua with scriptum build \
&& ~/lua-5.4.2/install/bin/lua ~/lux/stdlib/target/program.lua > ~/lux/documentation/library/standard/lua.md

### Python
cd ~/lux/stdlib/ \
&& lux clean \
&& lux with python with scriptum build \
&& python3 ~/lux/stdlib/target/program.py > ~/lux/documentation/library/standard/python.md

### Ruby
cd ~/lux/stdlib/ \
&& lux clean \
&& lux with ruby with scriptum build \
&& RUBY_THREAD_VM_STACK_SIZE=15700000 ruby ~/lux/stdlib/target/program/main.rb | tee ~/lux/documentation/library/standard/ruby.md
```

---

# Aedifex: Build system

## Build

```
cd ~/lux/stdlib/ \
&& lux clean \
&& lux with aedifex auto build

cd ~/lux/stdlib/ \
&& lux clean \
&& lux with aedifex build \
&& mv target/program.jar aedifex.jar

cd ~/lux/stdlib/ \
&& lein clean \
&& lein with-profile aedifex lux auto build

cd ~/lux/stdlib/ \
&& lein clean \
&& lein with-profile aedifex lux build \
&& mv target/program.jar aedifex.jar
```

## Test

```
cd ~/lux/stdlib/ \
&& lux clean \
&& lux with aedifex auto test
```

