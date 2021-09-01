# Chapter 9: Meta-programming

_Where we go meta. For real._

---

Metaprogramming is the art of making programs... that make programs.

There are many techniques and tools for achieving this, but one that is very familiar to _Lisp_ fans is to use macros to generate code at compile-time.

However, we're not going to talk about macros in this chapter.

Instead, I'll reveal the infrastructure that makes macros possible, and we'll discuss macros in the next chapter.

## The `Lux` type

	Yeah, I'm aware that it's weird there's a type with the same name as the language, but I couldn't figure out a better name.

The Lux compiler was designed to integrate very well with the language itself.

Most compilers are just programs that take source code and emit some binary executable or some byte-code. But the Lux compiler opens itself for usage within Lux programs and provides Lux programmers with a wealth of information.

The `Lux` type enters the stage.

```
(type: #export Lux
  {#info            Info
   #source          Source
   #location        Location
   #current_module  (Maybe Text)
   #modules         (List [Text Module])
   #scopes          (List Scope)
   #type_context    Type_Context
   #expected        (Maybe Type)
   #seed            Nat
   #scope_type_vars (List Nat)
   #extensions      Any
   #host            Any})
```

	By the way, the `Lux` type and other weird types you may not recognize there are all defined in the `library/lux` module. Check the documentation for the Standard Library for more details.

The `Lux` type represents the state of the Lux compiler at any given point.

It is not a reflection of that state, or a subset of it. It is `the` state of the Lux compiler; and, as you can see, it contains quite a lot of information about compiled modules, the state of the type-checker, the lexical and global environments, and more.

Heck, you can even access the yet-unprocessed source code of a module at any given time.

That's pretty neat.

You can actually write computations that can read and even modify (_careful with that one_) the state of the compiler. This turns out to be massively useful when implementing a variety of powerful macros.

For example, remember the `open:` and `\` macros from [chapter 7](chapter_7.md)?

They actually look up the typing information for the structures you give them to figure out the names of members and generate the code necessary to get that functionality going.

And that is just the tip of the iceberg.

The possibilities are really vast when it comes to using the information provided by the `Lux` compiler state.

## The `Meta` type

_But, how do I use it?_

Well, that is where the `Meta` type and the `library/lux/meta` module come into play.

The `library/lux/meta` module houses many functions for querying the `Lux` compiler state for information, and even to change it a little bit (in safe ways).

I won't go into detail about what's available, but you'll quickly get an idea of what you can do if you read the documentation for it in the Standard Library.

However, one thing I _will_ say is that those functions rely heavily on the `Meta` type, which is defined thusly:

```
(type: #export (Meta a)
  (-> Lux (Either Text [Lux a])))
```

	The `Meta` type is defined in the `library/lux` module, although most functions that deal with it are in the `library/lux/meta` module.

The `Meta` type has a `Functor`, and a `Monad`, but they are a bit rather complicated.

You saw some functor/monad examples in the last chapter, but this is more colorful.

`Meta` instances are functions that given an instance of the `Lux` compiler state, will perform some calculations which may fail (_with an error message_); but if they succeed, they yield a value, plus a (_possibly updated_) instance of the `Lux` compiler state.

Lux metaprogramming is based heavily on the `Meta` type, and macros themselves rely on it for many of their functionalities, as you'll see in the next chapter.

## Where do `Lux` instances come from?

Clearly, `Lux` instances are data, but the compiler is not available at all times.

The compiler is only ever present during... well... compilation.

And that is precisely when all of your `Lux`-dependant code will execute.

Basically, in order for you to get your hands on that _sweet_ compiler information, your code must be run at compile-time. But only macro code can ever do that, so you will have to wait until the next chapter to learn how this story ends.

## Definition annotations

Another important piece of information you should be aware of is that definitions don't just have values and types associated with them, but also arbitrary meta-data which you can customize as much as you want.

The relevant types in the `library/lux` module are:

```
(type: #export Location
  {#module Text
   #line   Nat
   #column Nat})

(type: #export (Ann m v)
  {#meta  m
   #datum v})

(type: #export (Code' w)
  (#Bit Bit)
  (#Nat Nat)
  (#Int Int)
  (#Rev Rev)
  (#Frac Frac)
  (#Text Text)
  (#Identifier Name)
  (#Tag Name)
  (#Form (List (w (Code' w))))
  (#Tuple (List (w (Code' w))))
  (#Record (List [(w (Code' w)) (w (Code' w))])))

(type: #export Code
  (Ann Location (Code' (Ann Location))))
```

You can add annotations to definitions in the many definition macros offered in the standard library.
You can also annotate modules when using the `module:` macro.

All you need to do is pass in some record syntax, with tags signaling the type of annotation you want, and the associated value being either an explicit variant `Code`, or some function or macro call that would produce such a value.

Here's an example from `library/lux`:

```
(def: #export (is? reference sample)
  {#.doc (doc "Tests whether the 2 values are identical (not just 'equal')."
              "This one should succeed:"
              (let [value +5]
                (is? value value))

              "This one should fail:"
              (is? +5 (+ +2 +3)))}
  (All [a] (-> a a Bit))
  ("lux is" reference sample))
```

	The (_optional_) annotations always goes after the declaration or name of the thing being defined.

Note that all tag usage within annotation records should be fully qualified, to avoid potential confusions, as different modules could be using annotation tags with similar names.

The `library/lux/meta/annotation` module contains various functions for reading and exploring annotations, and some modules in the standard library (for example, the `lbrary/lux/ffi` module) make heavy use of annotations to figure out properties of definitions which may be useful during code-generation and parsing in macros.

And also, as you can appreciate from the previous example, some macros may be designed to be used during annotation specification.

---

This chapter feels a little empty because the topic only makes sense within the context of macros. But macros by themselves are a huge subject, and involve more machinery than you've seen so far.

However, I wanted to give you a taste of what's possible in order to whet your appetite, while keeping the chapter focused.

In the next chapter, I'll complete this puzzle, and you'll be given access to a power greater than you've ever known (_unless you've already been a lisper for a while_).

See you in [the next chapter](chapter_10.md)!

