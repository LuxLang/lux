# Chapter 1: Getting started

_Where you will learn how to set up a development environment for Lux._

---

Before any coding can happen, it is necessary to set-up everything you need to become a productive Lux programmer.

## Question #1: How do I write Lux code?

Text editor support is a fundamental thing for any language, and Lux already covers some of that.
The catch is that there's only support for Emacs at the moment.

The plugin is called [lux-mode](https://github.com/LuxLang/lux/tree/master/lux-mode).

The instructions for how to install it are at the link and it won't take much time.

**Note**: If you've already installed _lux-mode_ before while using a previous version of Lux, you should install it again, as the language has changed a lot between previous versions and v0.6.

## Question #2: How do I build Lux programs?

Lux uses a custom-made build tool named _Aedifex_ which is configured using a declarative Lux-based syntax.

To install Aedifex, go to https://github.com/LuxLang/lux/tree/master/shell and download either `lux.bat` or `lux.sh` depending on whether you're on Windows or Linux/Mac.

Also [download the aedifex.jar file](https://github.com/LuxLang/lux/releases/download/0.6.0/aedifex.jar), and place it (along with either of the scripts you downloaded) somewhere in your `PATH`.

Now, you'll have access to the `lux` command, which allows you to run Aedifex to build and test Lux projects.

## Question #3: How do I use Aedifex?

To find out, let's create a sample project that will have everything we need.

These are the steps:

1. Create a directory called `my_project`.
2. Create a new project file at `my_project/project.lux`.
3. Add this to the project file:

```clojure
[""
 ["identity" ["my.group" "my_project" "0.1.0-SNAPSHOT"]
  "repositories" ["https://oss.sonatype.org/content/repositories/snapshots/"
                  "https://oss.sonatype.org/service/local/staging/deploy/maven2/"]
  
  "dependencies" [["com.github.luxlang" "stdlib" "0.6.0" "tar"]]
  "compiler" ["com.github.luxlang" "lux-jvm" "0.6.0" "jar"]

  "program" "main"]]

... By default, Aedifex uses the "source" directory for finding your source-code.
... The file containing our program will be my_project/source/main.lux.
```

4. Create `my_project/source/main.lux` and add this code to it:

```clojure
(.using
 [library
  [lux "*"
   [program {"+" program:}]
   ["[0]" debug]
   [control
    ["[0]" io]]]])

(program: args
  (io.io (debug.log! "Hello, world!")))

... As you can see, this is nothing more than a very simple "Hello, world!" program to test things out.
... Everything will be explained later in the rest of the book.
```

5. In your terminal, go to `my_project`, and execute `lux build`.

When it's done, you should see a message like this:

```
...
Compilation complete!
Duration: +15s26ms
[BUILD ENDED]
```

A directory named `target` will have been created, containing everything that was compiled, alongside an executable JAR file.

6. Run the program with this command: `java -jar target/jvm/program.jar`
7. Smile :)

	For a thorough specification of what Aedifex can do, please refer to [Appendix H](appendix_h.md).

## Question #4: Where can I find documentation for Lux?

A specially useful source of information is [the documentation for the standard library](https://github.com/LuxLang/lux/tree/master/documentation/library/standard).

You can also explore [the Lux repository on GitHub](https://github.com/LuxLang/lux) for more information.

## Question #5: Where do I talk about Lux?

The place to talk about Lux is at [the Lux forum](http://luxlang.freeforums.net/).

---

Now, we can proceed to the actual teaching of the language!

See you in [the next chapter](chapter_2.md)!

