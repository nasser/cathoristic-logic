F#/Clojure implementation of Cathoristic Logic
----------------------------------------------

Ported from [the original Haskell implementation](https://github.com/RichardEvans/cathoristic-logic/blob/master/README.md) to F# with a ClojureCLR wrapper. Much thanks to @vivid-synth for the Haskell guidance.

### Building

The F# compiler will issue warnings that you should ignore. It should produce `Cathoristic.dll` and `FSharp.Core.dll` in the same directory.

```
$ fsharpc -a Cathoristic.fs
Microsoft (R) F# Compiler version 4.1
Copyright (c) Microsoft Corporation. All Rights Reserved.

/Users/nasser/Projects/cathoristic/Cathoristic.fs(83,21): warning FS0025: Incomplete pattern matches on this expression. For example, the value '(_,None)' may indicate a case not covered by the pattern(s).

/Users/nasser/Projects/cathoristic/Cathoristic.fs(106,34): warning FS0025: Incomplete pattern matches on this expression. For example, the value '(_,None)' may indicate a case not covered by the pattern(s).
```

### Usage

The `cl` namespace has the following definitions and functions, taken from the original API.

* `top`
* `bot`
* `(entails? p q)`
* `(trans sym prop)`
* `(trans sym)`
* `(conj prop prop)`
* `(bang [sym])`

A 'symbol' here is anything that can be turned into a string. As a convenience if `trans` is called with one argument the second default to `top`. Additionally, some sugar is provided:

* `(∧ p q)` → `(conj p q)`
* `(! & ss)` → `(bang ss)`
* `(?= p q)` → `(entails? p q)`

The Clojure implementation is tested using [nostrand](https://github.com/nasser/nostrand), but should work anywhere ClojureCLR does.

```clj
$ nos repl
user> (use 'cl)
WARNING: conj already refers to: #'clojure.core/conj in namespace: user, being replaced by: #'cl/conj
nil
user> (?= top top)
true
user> (?= top bot)
false
user> (?= (trans :a (! :b)) (trans :a))
true
user> (?= (trans :a (! :b)) (trans :a (! :c)))
false
```
