module Example exposing (..)

import SafeParser exposing (..)


example =
    run dangerous "1111"


dangerous : Parser chomps (List String)
dangerous =
    loop []
        (\state ->
            oneOf
                [ succeed ()
                    |> map (\() -> "a" :: state)
                    |> unsafelyDone
                ]
        )


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
            |> keep (chompIf Char.isDigit)
            |> skip0 oneDigitOrManyAlphaParser
            |> skip0 (chompWhile Char.isDigit)
            -- |> keep0 (chompWhile Char.isAlpha)
            |> continue
        , succeed (List.reverse revStmts)
            |> unsafelyDone
        ]
