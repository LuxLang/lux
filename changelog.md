# Based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Un-released]
### Added
### Changed
### Removed
### Fixed
### Deprecated
### Security

## [0.8.0]
### Added
* Logic programming.
* Dynamic delimited continuation operators.
* Conditions system.
* Object-oriented programming.
* Dynamic-binding and subject-oriented programming.
* Simple benchmarking machinery.
* Saturation arithmetic.
* Generic logging abstraction/machinery.
* Inline testing.
* Aspect-oriented programming.
* Context-oriented programming.
* Row polymorphism.
* Predicate-based polymorphism.
* Behavioral programming.
* Type-based multiple dispatch.
* Structured concurrency.
* Function trampolines.
* Agent model.
* Support for time-zones.
* Incremental computation.
* Functions with named parameters.
* Communicating Sequential Processes (CSP).
* Variadic functions.
* Event-loop concurrency.
### Changed
* Simplified polytypic programming.
* Re-licensed to MPL 2.0.
* Added source-tracking annotations when compiling for the JVM.
* Now doing implicit definition aliasing, instead of requiring explicit aliasing.
* New syntax for compiler extensions.
* Made labels (tags & slots) into normal definitions.
* Made the compiler's caching system sensitive to the build configuration.
### Removed
* Leiningen plugin.
* Bootstrapping compiler.
* Custom licensing machinery.
### Fixed
### Deprecated
### Security

## [0.7.0]
### Added
* Inlined functions.
* Can pass configuration parameters from the build description to the compiler.
* Code selection based on configuration parameters.
* Code selection based on compiler version.
* (Experimental) extensible meta-compiler architecture.
* Export machinery to consume Lux code from host-language programs.
* Generalized/type-agnostic arithmetic.
* (Optional) faster (but unsafe) array-handling machinery.
* (Optional) faster (but unsafe) text-handling machinery.
* (Optional) faster (but unsafe) binary-handling machinery.
* Can now deploy releases with Aedifex.
* Extensible import syntax.
* Context-aware macros.
* Macro volabularies for more controlled macro-expansion.
### Changed
* JVM compilation no longer relies on the ASM library.
* Friendlier syntax.
* No more automatic conversions of primitive types in JVM FFI.
* Now demanding mandatory loop names, instead of using default "again" name.
* Now taking into account both contravariance and covariance for mutable types in the standard library.
* Improved syntax for JVM interop.
* Programs are now explicit and first-class and the "main" program must be specified in an Aedifex `project.lux` file.
* Macros are first-class values.
* Pattern-matching now supports matching against globally-defined constants.
* All (normal) macros in a pattern are now automatically expanded.
### Removed
### Fixed
* Pattern-matching bug that allowed redundancies for primitives.
* Various bugs related to compiler extensions.
* Various JVM interop bugs.
### Deprecated
### Security

[Un-released]: https://github.com/LuxLang/lux/compare/0.8.0...HEAD
[0.8.0]: https://github.com/LuxLang/lux/releases/tag/0.8.0
[0.7.0]: https://github.com/LuxLang/lux/releases/tag/0.7.0

