```
cd ~/lux && grep -r "" --include *.lux | sort
cd ~/lux && grep -RiIl 'BEFORE' | xargs sed -i 's/BEFORE/AFTER/g'
```

# Test

```
cd ~/lux/stdlib/ \
&& lux with jvm clean \
&& lux with jvm with bibliotheca auto test

cd ~/lux/stdlib/ \
&& lux with js clean \
&& lux with js with bibliotheca auto test

cd ~/lux/stdlib/ \
&& lux with lua clean \
&& lux with lua with bibliotheca auto test

cd ~/lux/stdlib/ \
&& lux with python clean \
&& lux with python with bibliotheca auto test

cd ~/lux/stdlib/ \
&& lux with ruby clean \
&& lux with ruby with bibliotheca auto test
```

# Deploy

```
cd ~/lux/stdlib/ \
&& lux clean \
&& lux install

cd ~/lux/stdlib/ \
&& lux deploy snapshots $NEXUS_USERNAME $NEXUS_PASSWORD

cd ~/lux/stdlib/ \
&& lux deploy releases $NEXUS_USERNAME $NEXUS_PASSWORD

LUX_PROJECT=stdlib && \
LUX_VERSION=0.8.0 && \
cd ~/lux/$LUX_PROJECT/ && \
lux pom && \
mv pom.xml RELEASE/$LUX_PROJECT-$LUX_VERSION.pom && \
cd RELEASE && \
touch README.md && \
zip $LUX_PROJECT-$LUX_VERSION-sources.jar README.md && \
zip $LUX_PROJECT-$LUX_VERSION-javadoc.jar README.md && \
zip $LUX_PROJECT-$LUX_VERSION.jar README.md && \
rm README.md && \
for file in *.*; do gpg -ab $file; done
```

# Generate documentation

```
cd ~/lux/stdlib/ \
&& lux with jvm clean \
&& lux with jvm with scriptum auto test

## JVM
cd ~/lux/stdlib/ \
&& lux with jvm clean \
&& lux with jvm with scriptum build \
&& java -jar target/program.jar > ~/lux/documentation/library/standard/jvm.md

## JS
cd ~/lux/stdlib/ \
&& lux with js clean \
&& lux with js with scriptum build \
&& node ~/lux/stdlib/target/program.js > ~/lux/documentation/library/standard/js.md

## Lua
cd ~/lux/stdlib/ \
&& lux with lua clean \
&& lux with lua with scriptum build \
&& ~/lua-5.4.2/install/bin/lua ~/lux/stdlib/target/program.lua > ~/lux/documentation/library/standard/lua.md

## Python
cd ~/lux/stdlib/ \
&& lux with python clean \
&& lux with python with scriptum build \
&& python3 ~/lux/stdlib/target/program.py > ~/lux/documentation/library/standard/python.md

## Ruby
cd ~/lux/stdlib/ \
&& lux with ruby clean \
&& lux with ruby with scriptum build \
&& RUBY_THREAD_VM_STACK_SIZE=15700000 ruby ~/lux/stdlib/target/program/main.rb | tee ~/lux/documentation/library/standard/ruby.md
```

