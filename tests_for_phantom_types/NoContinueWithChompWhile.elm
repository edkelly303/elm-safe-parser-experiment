module NoContinueWithChompWhile exposing (..)

import SafeParser as SP


x =
    SP.chompWhile (\c -> c == ' ')
        |> SP.continue
