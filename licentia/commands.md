# Build

```
## Develop
cd ~/lux/licentia/ \
&& lux clean \
&& lux auto build
```

# Test

```
## Develop
cd ~/lux/licentia/ \
&& lux clean \
&& lux auto test
```

# Run

```
## Re-generate Lux's license.
cd ~/lux/licentia/ \
&& java -jar target/program.jar --input ../license.json --output ../license.txt
```

