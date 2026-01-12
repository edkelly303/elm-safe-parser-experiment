module NoZeroOrMoreContinue exposing (..)

import SafeParser as SP


noContinueWithSucceed =
    SP.succeed (\_ -> ())
        |> SP.continue
