module KeepPropagatesZeroOrMore exposing (..)

import SafeParser as SP


x : SP.Parser SP.OneOrMore ()
x =
    SP.succeed (always ())
        |> SP.keep0 (SP.succeed ())
