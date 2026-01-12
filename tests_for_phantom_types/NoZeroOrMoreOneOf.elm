module NoZeroOrMoreOneOf exposing (..)

import SafeParser as SP


x =
    SP.oneOf
        [ SP.succeed ()
        ]
