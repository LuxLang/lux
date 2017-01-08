# How to use it

You'll need a project.clj that imports the lein-luxc plugin.

Here's an example:

```
(defproject lux/tutorial1 "0.1.0-SNAPSHOT"
  :plugins [[com.github.luxlang/lein-luxc "0.5.0"]]
  :dependencies [[io.vertx/vertx-web "3.0.0"]]
  :source-paths ["source"]
  :test-paths ["test"]
  :lux {:program "tutorial1
        tests "tests"}
  )
```

Now, all you need to do is run the plugin like this:

	lein lux build

There is also the _auto-build_ feature, which will re-build your project every time a file changes.

	lein lux auto build

And, if you want to run your tests, you can do:

	lein lux test
	lein lux auto test

Those tests must be in the `test` directory on your project root.
