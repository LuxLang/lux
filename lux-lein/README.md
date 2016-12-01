# How to use it

You'll need a project.clj that imports the lein-luxc plugin.

Here's an example:

```
(defproject com.github.luxlang/lux-stdlib "0.4.0"
  :description "Standard library for the Lux programming language."
  :url "https://github.com/LuxLang/stdlib"
  :license {:name "Mozilla Public License (Version 2.0)"
            :url "https://www.mozilla.org/en-US/MPL/2.0/"}
  :plugins [[com.github.luxlang/lein-luxc "0.3.0"]]
  :source-paths ["source"]
  )

```

Now, all you need to do is run the plugin like this:

	lein luxc compile
	
And, if you want to run unit-tests, you can do:

	lein luxc test

Those unit tests must be in the `test` directory on your project root.
