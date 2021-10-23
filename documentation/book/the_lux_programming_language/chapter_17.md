# Chapter 17: Cross-platform Lux

_Where you will sail to exotic foreign platforms aboard the S.S. Lux._

It was always my desire for Lux to be a language that could be used to write software for multiple platforms.

I've always found it annoying to write a piece of software in one language, and then if it became necessary to run the software under different circumstances, a rewrite had to be done because the language in which the software was written was not capable of adapting to the new requirements.

In theory, programming languages are universal.

* Logic is logic, no matter the language in which it is expressed.
* Instructions are instructions, no matter who executes them.

And yet, in practice, you need JavaScript for the browser, and Swift for the IPhone (to give you some examples).

Granted, with the advent of WebAssembly, it has now become possible to have any options you want for the browser, instead of being locked to JavaScript and any languages that transpile to it.

But there is still another type of constraint that is probably not going away any time soon.

If a programmer or a company write a piece of software in some language, they have made a very significant investment.

If tomorrow, they realize that another language might be a better fit for what they need, it might be too costly for them to rewrite their software in a new language.

Cross-platform support for Lux is not just about accessing as many devices as possible, but also about being able to inter-operate with as many languages as possible.

Being able to interact with and extend a Python server, or a Lua script.

Ideally, I would like Lux to be able to inter-operate with every programming language that exists, thereby giving Lux programmers the capacity to write any program they want for any device; and the capacity to extend any codebase without being forcefully tied to any legacy language or implementation.

Sadly, the ideal world can only exist within our dreams, but (as of v0.6) steps have been taken to bring the dream closer to reality.

Currently, it is possible to write Lux programs that compile to the following target platforms:

* Java Virtual Machines
* JavaScript interpreters (such as browsers, and Node JS)
* Python interpreters
* Lua interpreters
* Ruby interpreters

Not only is that possible, but great care has been taken to make sure Lux works consistently across each of those platforms.

That means 2 things:

1. If your Lux program behaves one way in one platform, you can expect it to behave the same way in any other, _minus performance considerations_.
1. There is only one language for programming in any of those platforms: Lux. There is no _LuxScript_ or _PyLux_. Lux is a **single** programming language, not a family of languages.

The 2nd point is important, because another possible design could have been to have slightly different variations of Lux as different programming languages (albeit, sharing most of their syntax and semantics), each one targeting one of those platforms.

This could have been a very nice approach, as each of those platforms works slightly differently, so it's not crazy to think that each variation of Lux could adapt itself to those differences to offer a programmign experience closer to the platform.

The problem with this approach is that libraries written for one platform might not work for the others (or work _correctly_) because they would make certain assumptions about how things work that would be true for their platform of origin, but not in general.

This approach would allow Lux programmers to target different platforms, but it would make sharing code between them impossible at worst, and risky at best.

Instead, I've designed the semantics and the feature set of Lux to be independent of any host platform.

When the feature-set of a platform fits well with the feature-set of Lux, I use the platform's resources to implement Lux's functionality.

And when Lux needs something the platform does not offer, but does not disallow either, I emulate the feature in other to ensure a consistent experience.

This means all of Lux's naturals and integers are 64-bit, on the JVM (which supports them), and even on JavaScript (where they are emulated).

And this also means that Lux's concurrency mechanisms work as expected on the JVM (which allows multi-core processing), and on JavaScript/Python/Lua/Ruby (which don't).

---

In order to compile a Lux program to any of these alternative platforms, you must use a different compiler for each.

An Aedifex `project.lux` file allows for a `"compiler"` option to specify (as a dependency) the compiler you wish to use.

This option can be omitted, in which case it will pick, as a default value: `["com.github.luxlang" "lux-jvm" "0.6.3" "jar"]`.

Here are the compilers for the alternative platforms:

* For JavaScript: `["com.github.luxlang" "lux-js" "0.6.3" "js"]`
* For Python: `["com.github.luxlang" "lux-python" "0.6.3" "jar"]`
* For Lua: `["com.github.luxlang" "lux-lua" "0.6.3" "jar"]`
* For Ruby: `["com.github.luxlang" "lux-ruby" "0.6.3" "jar"]`

You don't need to use any special command on Aedifex in order to compile Lux to any alternative platform.

Just set the compiler, and build/test your program as usual.

	For a thorough specification of what Aedifex can do, please refer to [Appendix H](appendix_h.md).

In the same way that the JVM compiler produces a _single_ executable JAR file, each of these compilers will produce a _single_ executable `.js`/`.py`/`.lua`/`.rb` file that can directly be executed with the usual interpreters for those languages.

---

You might be wondering, though, how is it possible to share Lux code that is meant to work on these different platforms, given that they have different features, and different libraries.

_How is it possible to write Lux code that works everywhere, instead of being tied to the minutiae of each platform?_

Lux offers 2 different mechanisms to write cross-platform code.

One of them is meant to be used when most of the code you're writing is the same regardless of platform, and you just need to add some snippets here and there to access some specific things in each platform.

And the other is meant for when the changes are so massive, you might as well just write different files for different platforms.

First, let's go with the smaller mechanism:

```clojure
(def: js "JavaScript")

(for ["JVM" (do jvm stuff)
      ..js (do js stuff)]
     (do default stuff))
```

The `for` macro allows you to specify the code to use for each platform.

Each Lux compiler has a name (as a `Text`) for the platform it compiles to, and this information is made available to macros through the state of the compiler.

The `for` macro just compares this name to whatever options you give it in order to select what code to use when compiling to a given platform.

Additionally, it is possible to provide an (_optional_) snippet of code, to use in case there isn't a more specific snippet available.

Also, as you can see, the names for the platforms don't need to be provided as `Text` literals, as `for` knows how to resolve the names of definitions to get the options that way.

This is specially useful to avoid typos causing trouble when using `for`.

The module `library/lux/target` contains constants with the names of currently supported platforms, so it's a good idea to rely on that instead of manually specifying the names.

	If you check out `library/lux/target`, you might notice that there are more constants than there are currently supported platforms.
	This is because I made the effort to add additional platforms to Lux, but due to complications, I had to postpone them for later.
	The constants were left there anyway, since there is some code that depends on them on some of the more platform-specific parts of the standard library.
	As I finish the remaining back-ends, these constants will gain relevance again.

To give you an example of `for` in action, here is a definition from the `library/lux/data/text` module:

```clojure
(def: .public (replaced pattern replacement template)
  (-> Text Text Text Text)
  (for [@.old
        (:as Text
             ("jvm invokevirtual:java.lang.String:replace:java.lang.CharSequence,java.lang.CharSequence"
              (:as (Primitive "java.lang.String") template)
              (:as (Primitive "java.lang.CharSequence") pattern)
              (:as (Primitive "java.lang.CharSequence") replacement)))
        @.jvm
        (:as Text
             ("jvm member invoke virtual" [] "java.lang.String" "replace" []
              (:as (Primitive "java.lang.String") template)
              ["Ljava/lang/CharSequence;" (:as (Primitive "java.lang.CharSequence") pattern)]
              ["Ljava/lang/CharSequence;" (:as (Primitive "java.lang.CharSequence") replacement)]))
        ... TODO: Comment/turn-off when generating a JS compiler using a JVM-based compiler because Nashorn's implementation of "replaceAll" is incorrect. 
        @.js
        (:as Text
             ("js object do" "replaceAll" template [pattern replacement]))
        @.python
        (:as Text
             ("python object do" "replace" template pattern replacement))
        ... TODO @.lua
        @.ruby
        (:as Text
             ("ruby object do" "gsub" template pattern replacement))
        @.php
        (:as Text
             ("php apply" (:expected ("php constant" "str_replace"))
              pattern replacement template))
        ... TODO @.scheme
        ... TODO @.common_lisp
        ... TODO @.r
        ]
       ... Inefficient default
       (loop [left ""
              right template]
         (case (..split_by pattern right)
           {.#Some [pre post]}
           (again ($_ "lux text concat" left pre replacement) post)

           {.#None}
           ("lux text concat" left right)))))
```

This function implements text-replacement in a _generic_ way, while also taking advantage of platform-specific functionality where available.

---

The 2nd mechanism for writing cross-platform code is to specify platform-specific Lux files.

The way this works is by adding a secondary extension to your Lux files.

A normal Lux file looks like this: `foo.lux`.

When a Lux compiler sees that, it assumes the file contains code which is expected to work on any platform Lux can compile to.

However, it is possible to specify that a file contains code that is only meant for a specific platform, like this:

* For the JVM: `foo.jvm.lux`
* For JavaScript: `foo.js.lux`
* For Python: `foo.py.lux`
* For Lua: `foo.lua.lux`
* For Ruby: `foo.rb.lux`

If you're using, let's say, the JavaScript compiler for Lux (i.e. `["com.github.luxlang" "lux-js" "0.6.3" "js"]`), whenever you import a module as a dependency, the compiler will first look for a file with the `.js.lux` extension, and if it fails to find one, it will look for a file with the plain `.lux` extension.

_What happens if I do not have a `.js.lux` file, but I do have files with the other special extensions?_

A Lux compiler will always ignore files with extensions for platforms other than its own.

It will only ever take into account its own custom extension, and the general extension, so you don't need to worry about the wrong code being compiled.

A good example of this mechanism in action is the `library/lux/ffi` module.

This module provides several macros for making host-platform interoperation very easy to do.

Since the whole point of `library/lux/ffi` is platform interoperation, there is no point in there ever being a generic `library/lux/ffi.lux` file.

Instead, there is a `library/lux/ffi.jvm.lux` file, a `library/lux/ffi.js.lux` file, a `library/lux/ffi.py.lux` file, a `library/lux/ffi.lua.lux` file, and a `library/lux/ffi.rb.lux` file.

It makes the most sense to provide this module as a set of platform-specific files, instead of a single file that uses `for`, because everything in those files is platform-specific and there can be no meaningful re-use, so a version of that module which is a single file using `for`, would be an incomprehensible monstrosity.

But, by splitting the code into platform specific files, everything can be kept nice and tidy.

	You might also want to take a close look at the documentation for `library/lux/ffi` to see what macros are available.
	I wouldn't be surprised if you looked at the previous example of the `replaced` function and thought: _YUCK!_.
	Don't worry about it. Such ugly code is un-characteristic of _the Lux experience_.
	The reason why that code looks that way is because the `library/lux/data/text` gets implemented before `library/lux/ffi`, and so it cannot use any of the machinery specified therein.
	_Your code_ will have access to `library/lux/ffi`, so you can write much nicer code that doesn't have to concern itself with the low-level nitty-gritty details.

---

You might be puzzled by what you saw in that `replaced` example.

_You're calling text as if it was a function?_

Not quite.

You see, not all functionality a programming language provides can be implemented entirely within the programming language.

Sometimes, there are primitive bits of functionality that have to be baked into the language from the get-go.

Lux's mechanism for exposing those bits is as _extensions_ to the compiler.

Some of these extensions are _common_ to each compiler, and can be expected to be around regardless of whether you're compiling to the JVM, to JavaScript, or anywhere else.

Other extensions are _host_-specific, and are only meant to be around for a specific platform.

Either way, Lux uses the same mechanism for all of them: the humble _extension_.

You want to know what's the coolest thing about extensions?

_**You can write your own**_, and by doing so you can teach the compiler how to type-check, optimize and even generate code for your own new types of expressions.

Sounds cool?

See you in [the next chapter](chapter_18.md)!

