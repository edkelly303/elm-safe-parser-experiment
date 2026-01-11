module NoContinueWithSucceed exposing (..)

import SafeParser as SP


noContinueWithSucceed =
    SP.succeed (\_ -> ())
        |> SP.continue
