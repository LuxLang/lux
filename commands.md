# Count lines of code

```
cd ~/lux/ && find . -name '*.lux' | xargs wc -l

## Sorted by number of lines per file.
cd ~/lux/ && find . -name '*.lux' | xargs wc -l | sort -rn
```

---

# Find & replace

```
cd ~/lux && grep -r "" --include *.lux | sort
```

---

# Clean everything

```
cd ~/lux/lux-bootstrapper/ && lein clean && \
cd ~/lux/lux-jvm/ && lein clean && \
cd ~/lux/stdlib/ && lux clean && \
cd ~/lux/lux-js/ && lux clean && \
cd ~/lux/lux-python/ && lux clean && \
cd ~/lux/lux-lua/ && lux clean && \
cd ~/lux/lux-ruby/ && lux clean

cd ~/lux/lux-php/ && lux clean && \
cd ~/lux/lux-scheme/ && lux clean && \
cd ~/lux/lux-cl/ && lein lux && \
cd ~/lux/lux-r/ && lux clean

```

---

[Deploy Clojure Projects to Maven Central with Leiningen](https://kpow.io/how-to/deploy-clojure-projects-to-maven-central/)
[How to Generate PGP Signatures with Maven](https://blog.sonatype.com/2010/01/how-to-generate-pgp-signatures-with-maven/)
https://central.sonatype.org/publish/requirements/gpg/
https://github.com/technomancy/leiningen/blob/master/doc/DEPLOY.md

