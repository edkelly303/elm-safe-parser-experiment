# A safer parser package

This package uses phantom types to try to prevent a couple of footguns with
`elm/parser`'s API.

## Terminology

To understand the issues that this package addresses, we first need to
distinguish between two types of parser that `elm/parser` provides:

1. Parsers that always succeed, regardless of the input they are given. Examples
   include `succeed` (which never chomps any characters) and `chompWhile` (which
   can chomp zero or more characters). We'll refer to these as
   **`ZeroOrMore`** parsers.
2. Parsers that may fail on some inputs. If they succeed, they always chomp one
   or more characters. Examples include `chompIf`. We'll call these
   **`OneOrMore`** parsers.

## Problems and solutions

### 1. Unreachable parsers

**The problem**

With `elm-parser`, if you put a `ZeroOrMore` parser into the list that you
pass to `oneOf`, then it's impossible for any subsequent parsers in the list
to run:

```elm
import Parser as P

oops = 
  P.oneOf 
    [ P.chompWhile Char.isDigit 
      -- `chompWhile` always succeeds, even if it 
      -- can't chomp any characters...
    , P.chompIf Char.isAlpha 
      -- ... so we'll never reach this `chompIf`
    ] 
    |> P.getChompedString

P.run oops "a" --> Ok ""
```

The problem isn't just that this feels a bit counterintuitive, it's that the
code can become actively misleading. Especially in a `oneOf` that contains a
long list of parsers, it may be almost impossible to tell at a glance which ones
are reachable or unreachable. You would have to read the implementation of each
parser in the list to understand why the `oneOf` isn't doing what it looks like
it should be doing.

**The solution**

This package solves the problem by augmenting `oneOf` with some funky phantom 
type magic that ensures that it only accepts `OneOrMore` parsers, and adding `or`, 
a new function which ensures that you can only use a `ZeroOrMore` parser as the 
_last_ in a set of alternative parsers. This prevents you from accidentally 
creating unreachable parsers.

Let's say we want to try each of these parsers:

```elm
import SafeParser as SP

zeroOrMoreDigits : SP.Parser SP.ZeroOrMore ()
zeroOrMoreDigits = 
  SP.chompWhile Char.isDigit

oneAlpha : SP.Parser oneOrMore ()
oneAlpha = 
  SP.chompIf Char.isAlpha

-- This is fine, because the ZeroOrMore parser 
-- comes last:

oneAlpha
  |> SP.or zeroOrMoreDigits

--: SP.Parser SP.ZeroOrMore ()
```

But!

```elm
-- This will fail with a compiler error, 
-- because we're trying to add more parsers 
-- after a ZeroOrMore parser:

zeroOrMoreDigits
  |> SP.or oneAlpha
  
--! TYPE ERROR
```

### 2. Infinite loops

**The problem**

With `elm-parser`, if you put a parser that can succeed without chomping into a
`loop`, then your parser can get stuck in an infinite loop. That is never what
you want! Yet it's very easy to do by accident.

```elm
import Parser as P

ohDear = 
  P.loop () 
    (\state -> 
    P.oneOf 
        -- `chompWhile` always succeeds, and if it 
        -- can't chomp anything, we'll fall into...
        [ P.chompWhile Char.isDigit |> map P.Loop
        ]
    )

P.run ohDear "!" --! ... an infinite loop!
```

**The solution**

With this package, only a `OneOrMore` parser can be used to `continue` a loop.
This means it's impossible to fall into an infinite loop (I think...)

So, this won't compile:

```elm
import SafeParser as SP

ohNo = 
  SP.loop 
    ()
    (\state -> 
        -- `chompWhile` is a `ZeroOrMore` parser, 
        -- so it can't be passed to `continue`.
        (SP.chompWhile Char.isDigit |> SP.continue)
          |> SP.or (SP.succeed () |> SP.done)
    )

--! TYPE ERROR
```

But this is ok:

```elm
import SafeParser as SP

ohYeah = 
  SP.loop 
    ()
    (\state -> 
        -- `chompIf` is a `OneOrMore` parser,
        -- so it's fine to pass to `continue`.
        (SP.chompIf Char.isDigit |> SP.continue) 
          |> SP.or (SP.succeed () |> SP.done))

SP.run ohYeah "1234" --> Ok ()
```
