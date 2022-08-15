# Install

```
cd ~/lux/lux-lein/ \
&& lein install
```

# Release

```
LUX_VERSION=0.7.0 && \
cd ~/lux/lux-lein/ && \
lein pom && \
mv pom.xml RELEASE/lein-luxc-$LUX_VERSION.pom && \
mv target/lein-luxc-$LUX_VERSION.jar RELEASE/lein-luxc-$LUX_VERSION.jar && \
cd RELEASE && \
touch README.md && \
zip lein-luxc-$LUX_VERSION-sources.jar README.md && \
zip lein-luxc-$LUX_VERSION-javadoc.jar README.md && \
rm README.md && \
for file in *.*; do gpg -ab $file; done
```

