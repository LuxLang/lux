## Build

```
## Develop
cd ~/lux/licentia/ \
&& lein clean \
&& lein lux auto build

cd ~/lux/licentia/ \
&& lux clean \
&& time lux build

cd ~/lux/licentia/ \
&& time lux test
```

## Test

```
## Develop
cd ~/lux/licentia/ \
&& lein clean \
&& lein lux auto test
```

## Run

```
## Re-generate Lux's license.
cd ~/lux/licentia/ \
&& java -jar target/program.jar --input ../license.json --output ../license.txt
```

