# A safer parser package

This package uses phantom types to try to prevent a couple of footguns with
`elm/parser`'s API.

## Terminology

To understand the issues that this package addresses, we first need to
distinguish between two types of parser that `elm/parser` provides:

1. Parsers that always succeed, regardless of the input they are given. Examples
   include `succeed` (which never chomps any characters) and `chompWhile` (which
   can chomp zero or more characters). We'll refer to these as
   **`AlwaysSucceeds`** parsers.
2. Parsers that may fail on some inputs. If they succeed, they always chomp at
   least one character. Examples include `chompIf`. We'll call these
   **`MightFail`** parsers.

## Problems and solutions

### 1. Unreachable parsers

With `elm-parser`, if you put a `AlwaysSucceeds` parser into the list that you
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

The problem isn't just that this feels a bit counterintuitive, it's that your
code is now bloated and confusing. You might as well delete every item in the
list after the `chompWhile`, because they are all unreachable. But it's very
hard to spot the problem just by looking at the code.

With this package, we replace `oneOf` with `or`, which gives us access to some
funky phantom type magic. As a result, we can ensure at compile time that a
`AlwaysSucceeds` parser can only be used as the _last_ in a set of alternative
parsers. This prevents you from accidentally creating unreachable parsers.

Let's say we want to try each of these parsers:

```elm
import SafeParser exposing (Parser, AlwaysSucceeds, chompWhile, chompIf, or)

zeroOrMoreDigits : Parser AlwaysSucceeds ()
zeroOrMoreDigits = 
  chompWhile Char.isDigit

oneAlpha : Parser mightFail ()
oneAlpha = 
  chompIf Char.isAlpha

-- This is fine, because the AlwaysSucceeds parser 
-- comes last:

oneAlpha
  |> or zeroOrMoreDigits

--: Parser AlwaysSucceeds ()
```

But!

```elm
-- This will fail with a compiler error, 
-- because we're trying to add more parsers 
-- after a AlwaysSucceeds parser:

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

With this package, we are forced to include a `MightFail` parser as the first
alternative, and any parser that can `continue` the loop must also be
`MightFail`. This means it's impossible to fall into an infinite loop (I
think...)

So, this won't compile:

```elm
import SafeParser exposing (Parser, AlwaysSucceeds, loop, chompWhile, continue, done, succeed)

ohNo = 
  loop 
    { initialState = ()
    , firstCallback = 
      \state -> 
        -- `chompWhile` is a `AlwaysSucceeds` parser, 
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
import SafeParser exposing (Parser, AlwaysSucceeds, loop, chompWhile, chompIf, continue, done, succeed, run)

ohYeah = 
  loop 
    { initialState = ()
    , firstCallback = 
      \state -> 
        -- `chompIf` is an `MightFail` parser, 
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