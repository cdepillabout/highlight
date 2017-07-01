
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
all lines of the file.

The short example above show how to use `highlight` to highlight a regex in a
single file.  It is also possible to use `highlight` on multiple files at once:

![multiple highlighted files](/img/highlight-multi-file-example.png?raw=true "multiple highlighted files")

`highlight` will color and stripe the filenames to make it easier to see which
line came from which file.  This also shows an example of the `--ignore-case`
option, which is similar to `grep`'s `--ignore-case` option.

### highlight output from `grep`

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

`highlight` and `hrep` can be installed with
[`stack`](https://docs.haskellstack.org/en/stable/README/):

```sh
$ stack install highlight
```

By default, `stack` will install binaries into `~/.local/bin/`.

It should also be possible to use `cabal` to install this package.

## Other ways to highlight parts of files

It is possible to highlight lines matching a given regex with `grep` two
different ways.

1.  Use a special regex that will match any line, but only highlight the part
    desired.  It would look like this:

    ```sh
    $ grep 'about|$' file-cats file-dogs file-goats
    ```

2.  Give a large `--context` flag:

    ```sh
    $ grep --context 9999 'about' file-cats file-dogs file-goats
    ```

However, neither of these will color and stripe filenames.

## Contributions

Feel free to open an
[issue](https://github.com/cdepillabout/pretty-simple/issues) or
[PR](https://github.com/cdepillabout/pretty-simple/pulls) for any
bugs/problems/suggestions/improvements.

### Additional flags

`highlight` and `hrep` do not currently support all flags and options that
`grep` does.  Ideally, `highlight` and `hrep` would be drop-in replacements for
`grep`, supporting all the same flags and options as `grep`.

If there is a flag or option you frequently use and want supported with
`highlight` or `hrep`, please feel free to open an issue or PR.  Some
flags/options will be relatively easy to support, while some may require quite
a large amount of additional code.

## Development

### Build

`highlight` and `hrep` can be built will the following command.  See the
Installation section above for information about `stack`.

```sh
$ stack build
```

### Test

The tests can be run with the following command:

```sh
$ stack test
```
