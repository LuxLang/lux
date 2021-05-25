# R compiler

## Test

```
cd ~/lux/lux-r/ && lein lux auto test
cd ~/lux/lux-r/ && lein clean && lein lux auto test
```

## Build

```
cd ~/lux/lux-r/ && lein lux auto build
cd ~/lux/lux-r/ && lein clean && lein lux auto build
```

## REPL

```
cd ~/lux/lux-r/ && java -jar target/program.jar repl --source ~/lux/stdlib/source --target ~/lux/stdlib/target
```

## Try

```
cd ~/lux/lux-r/ && time java -jar target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux
cd ~/lux/stdlib/ && lein clean && cd ~/lux/lux-r/ && time java -jar target/program.jar build --source ~/lux/stdlib/source --target ~/lux/stdlib/target --module test/lux
cd ~/lux/stdlib/ && lein clean && cd ~/lux/lux-r/ && time java -jar target/program.jar build --source ~/lux/stdlib/source --library ~/lux/stdlib/target/library.tar --target ~/lux/stdlib/target --module test/lux
cd ~/lux/lux-r/ && java -jar target/program.jar export --source ~/lux/stdlib/source --target ~/lux/stdlib/target

cd ~/lux/stdlib/target/ && java -jar program.jar
```

---

# Compiler trial

## Build

```
cd ~/lux/lux-trial/ && lein clean && lein lux build
cd ~/lux/lux-trial/target/ && java -jar program.jar
```

