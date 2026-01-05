# A safer parser package

This package uses phantom types to try to prevent a couple of footguns with
`elm/parser`'s API:

1. With `elm-parser`, if you put a parser that can succeed without chomping into
  a `oneOf`, then it's impossible for any subsequent parsers to run:
  ```elm
  oops = 
    oneOf 
      [ chompWhile Char.isDigit -- always succeeds, even if it can't chomp any digits
      , chompIf Char.isAlpha -- this will never run
      ] 
      |> getChompedString
  
  run oops "A" -- Ok ""
  ```
2. With `elm-parser`, if you put a parser that can succeed without chomping into
  a `loop`, then your parser can get stuck in an infinite loop.
  ```elm
  oops = 
    loop () 
      (\state -> 
        oneOf 
          [ chompIf Char.isAlpha |> map Done
          , chompWhile Char.isDigit |> map Loop
          ]
      )
  
  run oops "!" -- infinite loop!
  ```