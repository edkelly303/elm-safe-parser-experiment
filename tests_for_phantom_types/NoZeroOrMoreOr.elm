module NoZeroOrMoreOr exposing (..)

import SafeParser as SP


x =
    SP.succeed ()
        |> SP.or (SP.succeed ())
