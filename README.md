
Highlight
=========

[![Build Status](https://secure.travis-ci.org/cdepillabout/highlight.svg)](http://travis-ci.org/cdepillabout/highlight)
[![Hackage](https://img.shields.io/hackage/v/highlight.svg)](https://hackage.haskell.org/package/highlight)
[![Stackage LTS](http://stackage.org/package/highlight/badge/lts)](http://stackage.org/lts/package/highlight)
[![Stackage Nightly](http://stackage.org/package/highlight/badge/nightly)](http://stackage.org/nightly/package/highlight)
![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)

`highlight` is a command line program for highlighting parts of a file matching
a regex.

For example, take a look at the following file:

![non-highlighted file](/img/non-highlighted-file.png?raw=true "non-highlighted file")

`highlight` can be used to highlight the regex `cat`:

![simple highlighted file](/img/highlight-simple-example.png?raw=true "simple highlighted file")

## Uses

This package installs two binaries, `highlight` and `hrep`.  The following
section explains the main uses of `highlight` and `hrep`.

### `highlight`

`highlight` is used to highlight a given regex in a file, while printing out
all lines of a file.

The short example above show how to use `highlight` to `highlight` a regex in a
single file.  It is also possible to use `highlight` on multiple files at once:

![multiple highlighted files](/img/highlight-multi-file-example.png?raw=true "multiple highlighted files")

`highlight` will color and stripe the filenames to make it easier to see which
line came from which file.  This also shows an example of the `--ignore-case`
options, which is similar to `grep`'s `--ignore-case` option.

### `highlight` output from `grep`

`highlight` has a special option for highlighting output from `grep`:

![highlight from grep](/img/highlight-from-grep.png?raw=true "highlight from grep")

This `--from-grep` option will color and stripe filenames, similar to the
previous example.

### `hrep`

With the previous `--from-grep` option to `highlight`, one unfortunate point is
that the regex has to be specified twice, once to `grep` and once to
`highlight`.

The `hrep` command can solve this for us.  It is just like the `grep` command,
but it will color and stripe filenames:

![hrep example](/img/hrep-example.png?raw=true "hrep example")

## Installation


For example, imagine the following Haskell data types and values:

```haskell
data Foo = Foo { foo1 :: Integer , foo2 :: [String] } deriving Show

foo :: Foo
foo = Foo 3 ["hello", "goodbye"]

data Bar = Bar { bar1 :: Double , bar2 :: [Foo] } deriving Show

bar :: Bar
bar = Bar 10.55 [foo, foo]
```

If you run this in `ghci` and type `print bar`, you'll get output like this:

```haskell
> print bar
Bar {bar1 = 10.55, bar2 = [Foo {foo1 = 3, foo2 = ["hello","goodbye"]},Foo {foo1 = 3, foo2 = ["hello","goodbye"]}]}
```

This is pretty hard to read.  Imagine if there were more fields or it were even
more deeply nested.  It would be even more difficult to read.

`pretty-simple` can be used to print `bar` in an easy-to-read format:

![example screenshot](/img/pretty-simple-example-screenshot.png?raw=true "example screenshot")

## Features

## Why not `(some other package)`?

## Contributions

Feel free to open an
[issue](https://github.com/cdepillabout/pretty-simple/issues) or
[PR](https://github.com/cdepillabout/pretty-simple/pulls) for any
bugs/problems/suggestions/improvements.
