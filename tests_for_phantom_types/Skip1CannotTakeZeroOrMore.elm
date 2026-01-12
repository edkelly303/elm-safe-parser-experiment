module Skip1CannotTakeZeroOrMore exposing (..)

import SafeParser as SP


x =
    SP.skip1 (SP.succeed ())
