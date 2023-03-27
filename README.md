# Höfuðtagsreikniritið

This is an implementation of the principal type algorithm,
using Robinsons unification algorithm, for simply typed lambda calculus.
This was made alongside the project *Tagskiptur lambda-reikningur*
for the course STÆ402G Samæfingar í stærðfræði, University of Iceland, in Spring 2023.

## Installation

To install this tool, the build tool Cabal is required, along with a Haskell compiler.
If this is not present, it should suffice to install
[GHCup](https://www.haskell.org/ghcup/install/).

Now, simply run the following command in the project's root directory:

```
$ cabal install
```

This creates an executable `principal-type` and places it
in `$HOME/.cabal/bin/` on UNIX machines and
in `%APPDATA%\cabal\bin` on Windows (this behavior can
be changed with Cabal). 

This directory can be added to the `PATH` environment
variable to enable running the compiler from the command line.

Alternatively, run `cabal run principal-type` in the project's root directory.
This builds the project and runs the resulting executable, without
installing it.

## Uninstallation

To uninstall this tool, simply remove the `.cabal` directory
specified above. If this is not desirable (e.g. if other executables
or libraries in use are found there), simply remove `principal-type`
from `.cabal/bin` and the appropriate directories from `.cabal/store`.

## Usage

Using the command `principal-type` presents the user with a REPL (read-eval-print loop):

```
$ principal-type
λ> 
```

Entering any λ-term outputs a judgement of the term's principal type,
using the principal type algorithm, if the term is typable,
or a message that the term is not typable if not.

```
λ> (λx. x)
⊢ (λx. x):(a → a)
λ> (x x)
(x x) is not typable
```

Abbreviations are available, as well as support for Church-numerals:

```
λ> (h e l l o)
e:b, h:(b → c → c → d → a), l:c, o:d ⊢ (h e l l o):a
λ> (λxy. y)
⊢ 0̅:(a → b → b)
λ> (λxy. (x¹⁰ y))
⊢ 1̅0̅:((a → a) → a → a)
```

Instead of the letter 'λ', one may also use '\\',
and instead of unicode superscript, one may use '^'.

By default, variables are parsed such that each alphabetic letter
is one variable. If a long variable name is desired, one may use
braces:
```
λ> hi
h:(b → a), i:b ⊢ (h i):a
λ> {hi}
hi:a ⊢ hi:a
```
