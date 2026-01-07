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

With `elm-parser`, if you put a `ZeroOrMore` parser into the list that you
pass to `oneOf`, then it's impossible for any subsequent parsers in the list
to run:

```elm
import Parser exposing (chompWhile, chompIf, oneOf, getChompedString, run)

oops = 
  oneOf 
    [ chompWhile Char.isDigit 
      -- `chompWhile` always succeeds, even if it 
      -- can't chomp any characters...
    , chompIf Char.isAlpha 
      -- ... so we'll never reach this `chompIf`
    ] 
    |> getChompedString

run oops "a" --> Ok ""
```

The problem isn't just that this feels a bit counterintuitive, it's that the
code can become actively misleading. Especially in a `oneOf` that contains a
long list of parsers, it may be almost impossible to tell at a glance which ones
are reachable or unreachable. You would have to read the implementation of each
parser in the list to understand why the `oneOf` isn't doing what it looks like
it should be doing.

This package solves the problem by replacing `oneOf` with `or` - a new function
which gives us access to some funky phantom type magic. With `or`, we can
ensure at compile time that a `ZeroOrMore` parser can only be used as the _last_
in a set of alternative parsers. This prevents you from accidentally creating
unreachable parsers.

Let's say we want to try each of these parsers:

```elm
import SafeParser exposing (Parser, ZeroOrMore, chompWhile, chompIf, or)

zeroOrMoreDigits : Parser ZeroOrMore ()
zeroOrMoreDigits = 
  chompWhile Char.isDigit

oneAlpha : Parser oneOrMore ()
oneAlpha = 
  chompIf Char.isAlpha

-- This is fine, because the ZeroOrMore parser 
-- comes last:

oneAlpha
  |> or zeroOrMoreDigits

--: Parser ZeroOrMore ()
```

But!

```elm
-- This will fail with a compiler error, 
-- because we're trying to add more parsers 
-- after a ZeroOrMore parser:

zeroOrMoreDigits
  |> or oneAlpha
  
--! TYPE ERROR
```

### 2. Infinite loops

With `elm-parser`, if you put a parser that can succeed without chomping into a
`loop`, then your parser can get stuck in an infinite loop. That is never what
you want! Yet it's very easy to do by accident.

```elm
import Parser exposing (Step(..), loop, oneOf, chompWhile, map)

ohDear = 
  loop () 
    (\state -> 
    oneOf 
        -- `chompWhile` always succeeds, and if it 
        -- can't chomp anything, we'll fall into...
        [ chompWhile Char.isDigit |> map Loop
        ]
    )

run ohDear "!" --! ... an infinite loop!
```

With this package, we are forced to include a `OneOrMore` parser as the first
alternative, and any parser that can `continue` the loop must also be
`OneOrMore`. This means it's impossible to fall into an infinite loop (I
think...)

So, this won't compile:

```elm
import SafeParser exposing (Parser, ZeroOrMore, loop, chompWhile, continue, done, succeed)

ohNo = 
  loop 
    { initialState = ()
    , firstCallback = 
      \state -> 
        -- `chompWhile` is a `ZeroOrMore` parser, 
        -- so it can't be passed to `continue`.
        chompWhile Char.isDigit 
          |> continue
    , restCallbacks = 
      \state -> 
        succeed ()
          |> done
    }

--! TYPE ERROR
```

But this is ok:

```elm
import SafeParser exposing (Parser, ZeroOrMore, loop, chompWhile, chompIf, continue, done, succeed, run)

ohYeah = 
  loop 
    { initialState = ()
    , firstCallback = 
      \state -> 
        -- `chompIf` is an `OneOrMore` parser, 
        -- so we have a guarantee that we will only continue
        -- looping if we've actually chomped something.
        chompIf Char.isDigit 
          |> continue
    , restCallbacks = 
      \state -> 
        succeed ()
          |> done
    }

run ohYeah "1234" --> Ok ()
```