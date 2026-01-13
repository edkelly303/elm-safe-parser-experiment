module Skip0PropagatesZeroOrMore exposing (..)

import SafeParser as SP


x : SP.Parser SP.OneOrMore ()
x =
    SP.succeed (always ())
        |> SP.skip0 (SP.succeed ())
