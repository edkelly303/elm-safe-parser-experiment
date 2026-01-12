module Keep1CannotTakeZeroOrMore exposing (..)

import SafeParser as SP


x =
    SP.keep1 (SP.succeed ())
