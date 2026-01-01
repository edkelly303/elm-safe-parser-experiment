module Example exposing (..)

import SafeParser exposing (..)


example =
    run statements "1111"


oneDigitOrManyAlphaParser =
    oneOf
        [ chompIf Char.isDigit
        , chompWhile Char.isAlpha
        ]


statements : Parser chomps (List ( (), () ))
statements =
    loop [] statementsHelp


statementsHelp revStmts =
    oneOf
        [ succeed (\x y -> ( x, y ) :: revStmts)
            |> keep0 oneDigitOrManyAlphaParser
            -- |> skip0 oneDigitOrManyAlphaParser
            -- |> skip1 (chompWhile Char.isDigit)
            -- |> keep0 (chompWhile Char.isAlpha)
            |> keep (chompIf Char.isDigit)
            |> continue
        , succeed (List.reverse revStmts)
            |> done0
        ]
