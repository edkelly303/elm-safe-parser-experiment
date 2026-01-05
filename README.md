# A safer parser package

This package uses phantom types to try to prevent a couple of footguns with
`elm/parser`'s API.

## Terminology

To understand the issues that this package addresses, we first need to
distinguish between two types of parser that `elm/parser` provides:

1. Parsers that can succeed without chomping any characters. They might not be
   able to chomp any characters at all, like `succeed`. Or they might be able to
   chomp zero or more characters, like `chompWhile`. We'll refer to these as
   **`MightNotChomp`** parsers.
2. Parsers that are guaranteed to chomp at least one character, like `chompIf`.
   We'll call these **`AlwaysChomp`** parsers.

## Problems and solutions

1. With `elm-parser`, if you put a `MightNotChomp` parser into the list that you
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
  
  With this package, we replace `oneOf` with `or`, which gives us access to some
  funky phantom type magic. As a result, we can ensure at compile time that a
  `MightNotChomp` parser can only be used as the _last_ in a set of alternative
  parsers.

  ```elm
  import SafeParser exposing (Parser, MightNotChomp, chompWhile, chompIf, or)
  
  zeroOrMoreDigits = 
    chompWhile Char.isDigit
  
  oneAlpha = 
    chompIf Char.isAlpha

  -- this is fine, because the MightNotChomp parser 
  -- comes last:

  oneAlpha
    |> or zeroOrMoreDigits
  
  --: Parser MightNotChomp ()
  ```

  ```elm
  -- but this fails with a compiler error:
  
  zeroOrMoreDigits
    |> or oneAlpha
    
  -- type error
  ```
2. With `elm-parser`, if you put a parser that can succeed without chomping into
  a `loop`, then your parser can get stuck in an infinite loop.
  ```elm
  ohDear = 
    loop () 
      (\state -> 
        oneOf 
          [ chompWhile Char.isDigit |> map Loop
          -- `chompWhile` always succeeds, and if it 
          -- can't chomp anything, we'll fall into...
          ]
      )
  
  run ohDear "!" -- ... an infinite loop!
  ```