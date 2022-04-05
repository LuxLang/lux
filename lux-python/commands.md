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
```

# Try

```
## Compile Lux's Standard Library's tests using a JVM-based compiler.
cd ~/lux/stdlib/ \
&& lux clean \
&& java -jar ~/lux/lux-python/target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux \
&& python3 ~/lux/stdlib/target/program.py
```

# Deploy

```
cd ~/lux/lux-python/ \
&& mvn install:install-file -Dfile=target/program.jar -DgroupId=com.github.luxlang -DartifactId=lux-python -Dversion=0.7.0-SNAPSHOT -Dpackaging=jar

cd ~/lux/lux-python/ && mvn deploy:deploy-file \
-Durl=https://$NEXUS_USERNAME:$NEXUS_PASSWORD@oss.sonatype.org/content/repositories/snapshots/ \
-Dfile=target/program.jar \
-DgroupId=com.github.luxlang \
-DartifactId=lux-python \
-Dversion=0.7.0-SNAPSHOT \
-Dpackaging=jar
```

