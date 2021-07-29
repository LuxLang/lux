# Count lines of code

```
cd ~/lux/ && find . -name '*.lux' | xargs wc -l

## Sorted by number of lines per file.
cd ~/lux/ && find . -name '*.lux' | xargs wc -l | sort -rn
```

---

# Clean everything

```
cd ~/lux/lux-bootstrapper/ && lein clean && \
cd ~/lux/stdlib/ && lein clean && \
cd ~/lux/lux-jvm/ && lein clean && \
cd ~/lux/lux-js/ && lein clean && \
cd ~/lux/lux-python/ && lein clean && \
cd ~/lux/lux-lua/ && lein clean && \
cd ~/lux/lux-ruby/ && lein clean && \
cd ~/lux/lux-php/ && lein clean && \
cd ~/lux/lux-scheme/ && lein clean && \
cd ~/lux/lux-cl/ && lein clean && \
cd ~/lux/lux-r/ && lein clean

```

---

# Leiningen plugin

## Install

```
cd ~/lux/lux-lein/ \
&& lein install
```

