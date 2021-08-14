# Standard Library

## Test

```
cd ~/lux/stdlib/ \
&& lein clean \
&& lein with-profile bibliotheca lux auto test

cd ~/lux/stdlib/ \
&& lux clean \
&& lux auto test
```

## Deploy

```
cd ~/lux/stdlib/ && lein install

cd ~/lux/stdlib/ && mvn install:install-file -Dfile=target/library.tar -DgroupId=com.github.luxlang -DartifactId=stdlib -Dversion=0.6.0-SNAPSHOT -Dpackaging=tar

cd ~/lux/stdlib/ && mvn deploy:deploy-file \
-Durl=https://<username>:<password>@oss.sonatype.org/content/repositories/snapshots/ \
-Dfile=target/library.tar \
-DgroupId=com.github.luxlang \
-DartifactId=stdlib \
-Dversion=0.6.0-SNAPSHOT \
-Dpackaging=tar
```

## Generate documentation

```
cd ~/lux/stdlib/ \
&& lein clean \
&& lein with-profile scriptum lux auto build

cd ~/lux/stdlib/ \
&& lux clean \
&& lux with documentation auto test

cd ~/lux/stdlib/ \
&& lux clean \
&& lux with documentation build \
&& java -jar target/program.jar > documentation.md
```

---

# Aedifex: Build system

## Build

```
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
&& lein clean \
&& lein with-profile aedifex lux auto test
```

