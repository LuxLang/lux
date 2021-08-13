# Chapter 13: JVM inter-operation

_Where you will cross the great divide._

---

No language is an island, and even _compiled-to-native_ languages need to have some _FFI_ (or _foreign function interface_) to interact with C, Fortran or some other language.

There's a ton of awesome infrastructure out there that was implemented in other technologies, and there is no reason for us not to take a piece of that pie.

The beautiful thing about the inter-operation mechanism offered by Lux is that it can be used to interact with _any_ language running on the JVM (not only Java).

	Although, due to its simplicity, it's better suited for interacting with programs originally written in Java, as other languages tend to implement some tricks that make the classes they generate a little bit... _funky_.

_So, what do I **need** to work with the JVM?_

Basically, just 3 things:

1. The means to consume the resources it provides (i.e. classes, methods, fields, and objects).
2. The means to create your own resources (i.e. class definitions).
3. The means to access its special features (such as synchronization).

Let's explore them.

	By the way, the only module relevant to this chapter is `library/lux/ffi`.

## Importing classes, methods, and fields

It's all done with the help of the `import:` macro:

```
... Allows importing JVM classes, and using them as types.
... Their methods, fields and enum options can also be imported.
(import: java/lang/Object
  ["#::."
   (new [])
   (equals [java/lang/Object] boolean)
   (wait [int] #io #try void)])

... Special options can also be given for the return values.
... #? means that the values will be returned inside a Maybe type. That way, null becomes #.None.
... #try means that the computation might throw an exception, and the return value will be wrapped by the Try type.
... #io means the computation has side effects, and will be wrapped by the IO type.
... These options must show up in the following order [#io #try #?] (although, each option can be used independently).
(import: java/lang/String
  ["#::."
   (new [[byte]])
   (#static valueOf [char] java/lang/String)
   (#static valueOf #as int_valueOf [int] java/lang/String)])

(import: (java/util/List e)
  ["#::."
   (size [] int)
   (get [int] e)])

(import: (java/util/ArrayList a)
  ["#::."
   ([T] toArray [[T]] [T])])

... The class-type that is generated is of the fully-qualified name.
... This avoids a clash between the java.util.List type, and Lux's own List type.
... All enum options to be imported must be specified.
(import: java/lang/Character$UnicodeScript
  ["#::."
   (#enum ARABIC CYRILLIC LATIN)])

... It should also be noted, the only types that may show up in method arguments or return values may be Java classes, arrays, primitives, void or type-vars.
... Lux types, such as Maybe cannot be named (otherwise, they'd be confused for Java classes).
(import: (lux/concurrency/async/JvmAsync A)
  ["#::."
   (resolve [A] boolean)
   (poll [] A)
   (wasResolved [] boolean)
   (waitOn [lux/Function] void)
   (#static [A] make [A] (lux/concurrency/async/JvmAsync A))])

... Also, the names of the imported members will look like Class::member
(java/lang/Object::new [])

(java/lang/Object::equals [other_object] my_object)

(java/util/List::size [] my_list)

java/lang/Character$UnicodeScript::LATIN
```

This will be the tool you use the most when working with the JVM.

As you have noticed, it works by creating functions (and constant values) for all the class members you need. It also creates Lux type definitions matching the classes you import, so that you may easily refer to them when you write your own types later in regular Lux code.

	It must be noted that `import:` requires that you only import methods and fields from their original declaring classes/interfaces.

	What that means is that if class `A` declares/defines method `foo`, and class `B` extends `A`; to import `foo`, you must do it by importing it from `A`, instead of `B`.

## Writing classes

Normally, you'd use the `class:` macro:

```
... Allows defining JVM classes in Lux code.
... For example:
(class: #final (TestClass A) [Runnable]
  ... Fields
  (#private foo boolean)
  (#private bar A)
  (#private baz java/lang/Object)
  ... Methods
  (#public [] (new [value A]) []
           (exec
             (:= ::foo #1)
             (:= ::bar value)
             (:= ::baz "")
             []))
  (#public (virtual) java/lang/Object
           "")
  (#public #static (static) java/lang/Object
           "")
  (Runnable [] (run) void
            [])
  )

... The tuple corresponds to parent interfaces.
... An optional super-class can be specified before the tuple. If not specified, java.lang.Object will be assumed.
... Fields and methods defined in the class can be used with special syntax.
... For example:
... ::resolved, for accessing the 'resolved' field.
... (:= ::resolved #1) for modifying it.
... (::new! []) for calling the class's constructor.
... (::resolve! container [value]) for calling the 'resolve' method.
```

And, for anonymous classes, you'd use `object`:

```
... Allows defining anonymous classes.
... The 1st tuple corresponds to class-level type-variables.
... The 2nd tuple corresponds to parent interfaces.
... The 3rd tuple corresponds to arguments to the super class constructor.
... An optional super-class can be specified before the 1st tuple. If not specified, java.lang.Object will be assumed.
(object [] [Runnable]
  []
  (Runnable [] (run self) void
            (exec
              (do_something some_value)
              [])))
```

## Special features

* Accessing class objects.

```
... Loads the class as a java.lang.Class object.
(class_for java/lang/String)
```

* Test instances.

```
... Checks whether an object is an instance of a particular class.
... Caveat emptor: Can't check for polymorphism, so avoid using parameterized classes.
(case (check java/lang/String "YOLO")
  (#.Some value_as_string)
  #.None)
```

* Synchronizing threads.

```
... Evaluates body, while holding a lock on a given object.
(synchronized object-to-be-locked
(exec
  (do something)
  (do something else)
  (finish the computation)))
```

Calling multiple methods consecutively

```
... Call a variety of methods on an object. Then, return the object.
(do_to object
  (ClassName::method1 arg0 arg1 arg2)
  (ClassName::method2 arg3 arg4 arg5))
```

	`do_to` is inspired by Clojure's own doto macro.
	The difference is that, whereas Clojure's version pipes the object as the first argument to the method, Lux's pipes it at the end (which is where method functions take their object values).

The `library/lux/ffi` module offers much more, but you'll have to discover it yourself by heading over to the documentation for the Standard Library.

---

Host platform inter-operation is a feature whose value can never be understated, for there are many **important** features that could never be implemented without the means provided by it.

We're actually going to explore one such feature in the next chapter, when we abandon our old notions of sequential program execution and explore the curious world of concurrency.

See you in the next chapter!

