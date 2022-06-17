# Chapter 16: Testing

_Where you will learn how to avoid annoying bug reports._

---

Automated testing is a fundamental aspect of modern software development.

Long gone are the days of manual, ad-hoc testing.

With modern testing tools and frameworks, it's somewhat easy to increase the quality of programs by implementing comprehensive test suites that can cover large percentages of a program's functionality and behavior.

Lux doesn't stay behind and includes a testing module as part of its standard library.

The `library/lux/test` module contains the machinery you need to write unit-testing suites for your programs.

Not only that, but the _Aedifex_ build tool for Lux also includes a command for testing: `lux test`

How do you set that up?

Let's take a look at the `project.lux` file for the Lux standard library itself.

```clojure
[""
 ["identity" ["com.github.luxlang" "stdlib" "0.6.5"]

  "deploy_repositories" ["snapshots" "https://oss.sonatype.org/content/repositories/snapshots/"
                         "releases" "https://oss.sonatype.org/service/local/staging/deploy/maven2/"]

  "repositories" ["https://oss.sonatype.org/content/repositories/snapshots/"
                  "https://oss.sonatype.org/service/local/staging/deploy/maven2/"]]

 "jvm"
 ["compiler" ["com.github.luxlang" "lux-jvm" "0.6.5" "jar"]]

 "bibliotheca"
 ["info" ["description" "Standard Library for the Lux programming language."]
  "test" "test/lux"]
 ]
```

The `"test"` parameter specifies the name of a Lux module that serves as the entry point for testing.

Here is a summary of the file:

```clojure
(.require
 [library
  ["/" lux "*"
   [program {"+" program:}]
   ["_" test {"+" Test}]
   [control
    ["[0]" io]]
   ...
   ]])

(program: args
  (io.io (_.run! (_.times 100 ..test))))

```

A test suit consists of a `Test` (or a composite of as many `Test`s as you want), which is then `run!`.

The `times` combinator allows you to execute `Test`s several times.

This can be very useful when using random data generation within your tests, as each run of the tests will lead to the generation of different sorts of data.

This will help you cover many possible scenarios within the same test run, and perhaps uncover tricky corner cases you wouldn't have thought of.

But where do those tests come from?

Nothing is being defined here.

Let's take a look at the tests defined in a simpler module.

Well, the `run` macro, from `library/lux/test` pulls in all the tests from the imported modules to run them later once the program starts.

To know how tests work, let's take a look at one of those modules.

	From `test/lux/data/collection/stack`.

```clojure
(.require
  [library
   [lux "*"
    ["_" test {"+" Test}]
    [abstract
     [monad {"+" do}]
     [\\specification
      ["$[0]" equivalence]
      ["$[0]" functor {"+" Injection}]]]
    [control
     ["[0]" maybe]]
    [data
     ["[0]" bit ("[1]#[0]" equivalence)]]
    [math
     ["[0]" random]
     [number
      ["n" nat]]]]]
  [\\library
   ["[0]" /]])

(def (injection value)
  (Injection /.Stack)
  (/.top value /.empty))

(def .public test
  Test
  (<| (_.covering /._)
      (_.for [/.Stack])
      (do random.monad
        [size (# random.monad map (n.% 100) random.nat)
         sample (random.stack size random.nat)
         expected_top random.nat]
        ($_ _.and
            (_.for [/.equivalence]
                   ($equivalence.spec (/.equivalence n.equivalence) (random.stack size random.nat)))
            (_.for [/.functor]
                   ($functor.spec ..injection /.equivalence /.functor))
            
            (_.cover [/.size]
                     (n.= size (/.size sample)))
            (_.cover [/.empty?]
                     (bit#= (n.= 0 (/.size sample))
                            (/.empty? sample)))
            (_.cover [/.empty]
                     (/.empty? /.empty))
            (_.cover [/.value]
                     (case (/.value sample)
                       {.#None}
                       (/.empty? sample)
                       
                       {.#Some _}
                       (not (/.empty? sample))))
            (_.cover [/.next]
                     (case (/.next sample)
                       {.#None}
                       (/.empty? sample)
                       
                       {.#Some [top remaining]}
                       (# (/.equivalence n.equivalence) =
                          sample
                          (/.top top remaining))))
            (_.cover [/.top]
                     (case (/.next (/.top expected_top sample))
                       {.#Some [actual_top actual_sample]}
                       (and (same? expected_top actual_top)
                            (same? sample actual_sample))
                       
                       {.#None}
                       false))
            ))))

```

There's a lot going on here.

First of all, by using the `covering` macro, you can tell the test suit to track the coverage that your test suite has of a given module.

That way, if your tests miss some _exported/public_ definitions, the report you'll get after running the tests will tell you, so you can judiciously choose to either expand your coverage, or skip covering them.

The `for` and `cover` macros then signal whenever one or more definitions are being covered by a given test.

Lux also defines some _specifications_, which are basically parameterizable tests, which implement consistent testing for various interfaces in the standard library.

That way, when testing an implementation of those interfaces, instead of having to copy-paste, or re-invent the testing every time, the specification is imported.

This enables consistent testing of implementations.

`and` allows you to _sequentially_ compose `Test`s into a larger `Test`.

You can also see an example of how to use randomness to generate sample data for testing.

---

If you want to learn more about how to write tests, feel free to check out the test-suite for the Lux standard library.

It's very comprehensive and filled with good examples.

---

Without tests, the reliability of programs becomes a matter of faith, not engineering.

Automated tests can be integrated into processes of continuous delivery and integration to increase the confidence of individuals and teams that real value is being delivered, and that the customer won't be dissatisfied by buggy software.

Now that you know how to test your programs, you know everything you need to know to be a Lux programmer... on the JVM.

However, Lux has been expanded with support for other platforms, and it's time for you to learn about some of its capabilities.

See you in [the next chapter](chapter_17.md)!

