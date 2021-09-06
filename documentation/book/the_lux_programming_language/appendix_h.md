# Appendix H: Aedifex

Aedifex is a fairly basic build tool, but it offers the necessary commands to handle a normal Lux project.

It offers a small set of commands, and a few convenience features.

```
lux version

=>

00.06.00
```

This command tells you the version of Aedifex you're using.

This version is also the same as the version of the Lux compiler Aedifex has been designed to work with.

So, you can also think of this as being the version of the language that is best supported by Aedifex (although not necessarily the only version that is supported).

```
lux clean

=>

Successfully cleaned target directory: target
```

This command cleans up any compilation artifacts and caching artifacts left over by previous compilations/builds of your Lux project.

```
lux pom

=>

Successfully created POM file: pom.xml
```

This command generates a Maven POM file that reflects the contents of the `project.lux` file made for Aedifex.

```
lux deps

=>

[?] Fetching com.github.luxlang:lux-jvm-0.6.0 from "~/.m2/repository"
[O] Found com.github.luxlang:lux-jvm-0.6.0 at "~/.m2/repository"
 Local successes: 0: "com.github.luxlang:lux-jvm-0.6.0"
  Local failures: 
Remote successes: 
 Remote failures:
```

This commands fetches all dependencies from the available repositories (plus the local Maven repository/cache), and installs any dependencies that had to be fetched remotely into the local Maven repository/cache.

```
lux install

=>

Successfully installed the project locally.
```

This command packages your project into a TAR archive and installs it into the local Maven repository/cache, to be used as a dependency.

```
lux deploy <deploy_repository> <user_name> <password>

## For example:

lux deploy snapshots foo bar_baz_quux

=>

Successfully deployed the project.
```

This command packages your project into a TAR archive and deploys it to a remote Maven repository, to be used as a dependency.

```
lux build
```

This command build your project into an executable program, named `program.`(`jar`|`js`|`lua`|`py`|`rb`) depending on the chosen compilation target, and located under the target directory (`target` by default).

```
lux test
```

This command build your project into an executable program, and then executes it and display any STDOUT/STDERR logging output on the terminal/console.

This is the main mechanism to run automated tests for your Lux project.

```
lux auto build

## OR

lux auto test
```

This works the same as the normal versions of the command, but Aedifex also watches over the files in your source directories and every time a file changes, it automatically builds/tests the project for you.

This is extremely useful when fixing typing errors iteratively, or when debugging errors raised during your tests, as it saves you having to constantly re-build/re-run the project every time you want to try the latest changes.

```
lux with <profile> <command>

## For example:

lux with jvm with bibliotheca auto test
```

This command composes non-default profiles with the default one to generate the profile to use when executing a command.

This allows you to segregate useful configuration into different profiles and then combine them based on what you need at a given time.

---

Now that we have seen the available commands, it would be useful to see an annotated example `project.lux` file to see what bits of configuration it can contain.

```clojure
["" ... The empty text ("") is used to specify the default profile.
 [... An optional identity for the project.
  ... It can also be specified or overriden in a non-default profile.
  ... This will be the name given to the project when installed/deployed as a dependency.
  "identity" ["com.github.luxlang" "stdlib" "0.6.0-SNAPSHOT"]

  ... Every piece of information, and the whole "info" bundle, are optional.
  "info" ["url" "https://github.com/LuxLang/lux"
          "scm" "https://github.com/LuxLang/lux.git"
          "licenses" [["name" "Lux License v0.1.1"
                       "url" "https://github.com/LuxLang/lux/blob/master/license.txt"
                       "type" "repo"]]
          ... "organization" [["name" "Lux Foundation"
          ...                  "url" "http://example.com/lux_foundation"]]
          "developers" [["name" "Eduardo Julian"
                         "url" "https://github.com/eduardoejp"
                         ... "organization" ["name" "Lux Foundation"
                         ...                 "url" "http://example.com/lux_foundation"]
                         ]]
          ... #contributors [["name" "Eduardo Julian"
          ...                 "url" "https://github.com/eduardoejp"
          ...                 "organization" ["name" "Lux Foundation"
          ...                                 "url" "http://example.com/lux_foundation"]]]
          ]

  ... An optional list of repositories you can deploy to, given aliases so they're easy to refer to with the "deploy" command.
  "deploy_repositories" ["snapshots" "https://oss.sonatype.org/content/repositories/snapshots/"
                         "releases" "https://oss.sonatype.org/service/local/staging/deploy/maven2/"]

  ... An optional list of repositories to use for fetching remote dependencies.
  ... Additionally, there is an implicit repository being used, which is https://repo1.maven.org/maven2/
  ... So, even if the "repositories" list were to be empty, you'd still have access to the default repository.
  "repositories" ["https://oss.sonatype.org/content/repositories/snapshots/"
                  "https://oss.sonatype.org/service/local/staging/deploy/maven2/"]
  ... The different directories to look for source code. The default is described below.
  ... "sources" ["source"]
  ... The directory for storing the build artifacts. The default is described below.
  ... "target" "target"
  ]

 ... The following are alternative profiles to use in various situations.
 "jvm"
 [... "compiler" specifies the dependency to fetch and use as the compiler.
  "compiler" ["com.github.luxlang" "lux-jvm" "0.6.0-SNAPSHOT" "jar"]
  ... "dependencies" is an optional list of dependencies to fetch.
  ... The dependencies have the same shape as when specifying the compiler.
  ... When omitting the packaging format of the dependency, "tar" will be assumed.
  ... "dependencies" [["org.ow2.asm" "asm-all" "5.0.3" "jar"]
  ...                 ["com.github.luxlang" "stdlib" "0.6.0"]]
  ... The OS command to use when running JVM tests. The default is described below.
  ... "java" ["java" "-jar"]
  ]

 "js"
 ["compiler" ["com.github.luxlang" "lux-js" "0.6.0-SNAPSHOT" "js"]
  ... The OS command to use when running JS tests. The default is described below.
  ... "js" ["node" "--stack_size=8192"]
  ]

 "python"
 ["compiler" ["com.github.luxlang" "lux-python" "0.6.0-SNAPSHOT" "jar"]
  ... The OS command to use when running Python tests. The default is described below.
  ... "python" ["python3"]
  ]

 "lua"
 ["compiler" ["com.github.luxlang" "lux-lua" "0.6.0-SNAPSHOT" "jar"]
  ... The OS command to use when running Lua tests. The default is described below.
  ... "lua" ["lua"]
  ]

 "ruby"
 ["compiler" ["com.github.luxlang" "lux-ruby" "0.6.0-SNAPSHOT" "jar"]
  ... The OS command to use when running Ruby tests. The default is described below.
  ... "ruby" ["ruby"]
  ]

 "bibliotheca"
 ["info" ["description" "Standard library for the Lux programming language."]
  "test" "test/lux"]

 "documentation"
 ["program" "documentation/lux"
  "test" "documentation/lux"]

 "aedifex"
 ["info" ["description" "A build system/tool made exclusively for Lux."]
  ... Parent profiles to this one.
  ... Specifying them here is like automatically using Aedifex's "with" command.
  "parents" ["jvm"]
  ... The name of the main module of the program.
  "program" "program/aedifex"
  ... The name of the main module where the tests are located.
  "test" "test/aedifex"]]
```

