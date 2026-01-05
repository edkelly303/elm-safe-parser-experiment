module SafeParser exposing
    ( Parser, run
    , AlwaysChomps, MightNotChomp, chompIf, chompWhile, getChompedString
    , symbol
    , succeed, problem
    , keep, keep0, skip, skip0
    , or, backtrackable
    , map, andThenMightNotChomp, andThenChompsBefore, andThenChompsAfter
    , Step, loop
    )

{-|


## Running parsers

@docs Parser, run


## Chomping inputs

@docs AlwaysChomps, MightNotChomp, chompIf, chompWhile, getChompedString


## Generally useful parsers

@docs symbol


## Succeeding and failing

@docs succeed, problem


## Combining parsers

@docs keep, keep0, skip, skip0


## Choosing parsers

@docs or, backtrackable


## Transforming parsers

@docs map, andThenMightNotChomp, andThenChompsBefore, andThenChompsAfter


## Looping parsers

@docs Step, loop

-}

import Parser as ElmParser exposing ((|.), (|=))



{-
   d8888b. db    db d8b   db d8b   db d888888b d8b   db  d888b
   88  `8D 88    88 888o  88 888o  88   `88'   888o  88 88' Y8b
   88oobY' 88    88 88V8o 88 88V8o 88    88    88V8o 88 88
   88`8b   88    88 88 V8o88 88 V8o88    88    88 V8o88 88  ooo
   88 `88. 88b  d88 88  V888 88  V888   .88.   88  V888 88. ~8~
   88   YD ~Y8888P' VP   V8P VP   V8P Y888888P VP   V8P  Y888P


-}


type Parser any a
    = P (ElmParser.Parser a)


run : Parser any a -> String -> Result (List ElmParser.DeadEnd) a
run (P p) string =
    ElmParser.run p string



{-
    .o88b. db   db  .d88b.  .88b  d88. d8888b. d888888b d8b   db  d888b
   d8P  Y8 88   88 .8P  Y8. 88'YbdP`88 88  `8D   `88'   888o  88 88' Y8b
   8P      88ooo88 88    88 88  88  88 88oodD'    88    88V8o 88 88
   8b      88~~~88 88    88 88  88  88 88~~~      88    88 V8o88 88  ooo
   Y8b  d8 88   88 `8b  d8' 88  88  88 88        .88.   88  V888 88. ~8~
    `Y88P' YP   YP  `Y88P'  YP  YP  YP 88      Y888888P VP   V8P  Y888P


-}


type AlwaysChomps
    = AlwayChomps Never


type MightNotChomp
    = MightNotChomp Never


chompIf : (Char -> Bool) -> Parser alwaysChomps ()
chompIf test =
    P (ElmParser.chompIf test)


chompWhile : (Char -> Bool) -> Parser MightNotChomp ()
chompWhile test =
    P (ElmParser.chompWhile test)


getChompedString : Parser any a -> Parser any String
getChompedString (P p) =
    P (ElmParser.getChompedString p)



{-
   db   db d88888b db      d8888b. d88888b d8888b. .d8888.
   88   88 88'     88      88  `8D 88'     88  `8D 88'  YP
   88ooo88 88ooooo 88      88oodD' 88ooooo 88oobY' `8bo.
   88~~~88 88~~~~~ 88      88~~~   88~~~~~ 88`8b     `Y8b.
   88   88 88.     88booo. 88      88.     88 `88. db   8D
   YP   YP Y88888P Y88888P 88      Y88888P 88   YD `8888Y'


-}


symbol : String -> Parser alwayChomps ()
symbol str =
    P
        (if String.isEmpty str then
            ElmParser.problem "The `symbol` parser cannot match an empty string"

         else
            ElmParser.symbol str
        )



{-
   .d8888. db    db  .o88b.  .o88b. d88888b d88888b d8888b. d888888b d8b   db  d888b        .d8b.  d8b   db d8888b.      d88888b  .d8b.  d888888b db      d888888b d8b   db  d888b
   88'  YP 88    88 d8P  Y8 d8P  Y8 88'     88'     88  `8D   `88'   888o  88 88' Y8b      d8' `8b 888o  88 88  `8D      88'     d8' `8b   `88'   88        `88'   888o  88 88' Y8b
   `8bo.   88    88 8P      8P      88ooooo 88ooooo 88   88    88    88V8o 88 88           88ooo88 88V8o 88 88   88      88ooo   88ooo88    88    88         88    88V8o 88 88
     `Y8b. 88    88 8b      8b      88~~~~~ 88~~~~~ 88   88    88    88 V8o88 88  ooo      88~~~88 88 V8o88 88   88      88~~~   88~~~88    88    88         88    88 V8o88 88  ooo
   db   8D 88b  d88 Y8b  d8 Y8b  d8 88.     88.     88  .8D   .88.   88  V888 88. ~8~      88   88 88  V888 88  .8D      88      88   88   .88.   88booo.   .88.   88  V888 88. ~8~
   `8888Y' ~Y8888P'  `Y88P'  `Y88P' Y88888P Y88888P Y8888D' Y888888P VP   V8P  Y888P       YP   YP VP   V8P Y8888D'      YP      YP   YP Y888888P Y88888P Y888888P VP   V8P  Y888P


-}


succeed : a -> Parser MightNotChomp a
succeed a =
    P (ElmParser.succeed a)


problem : String -> Parser MightNotChomp a
problem string =
    P (ElmParser.problem string)



{-
    .o88b.  .d88b.  .88b  d88. d8888b. d888888b d8b   db d888888b d8b   db  d888b
   d8P  Y8 .8P  Y8. 88'YbdP`88 88  `8D   `88'   888o  88   `88'   888o  88 88' Y8b
   8P      88    88 88  88  88 88oooY'    88    88V8o 88    88    88V8o 88 88
   8b      88    88 88  88  88 88~~~b.    88    88 V8o88    88    88 V8o88 88  ooo
   Y8b  d8 `8b  d8' 88  88  88 88   8D   .88.   88  V888   .88.   88  V888 88. ~8~
    `Y88P'  `Y88P'  YP  YP  YP Y8888P' Y888888P VP   V8P Y888888P VP   V8P  Y888P


-}


keep : Parser AlwaysChomps a -> Parser any (a -> b) -> Parser alwaysChomps b
keep =
    implKeep


keep0 : Parser MightNotChomp a -> Parser any (a -> b) -> Parser any b
keep0 =
    implKeep


implKeep : Parser any a -> Parser any2 (a -> b) -> Parser any3 b
implKeep (P x) (P f) =
    P (f |= x)


skip : Parser AlwaysChomps skip -> Parser any keep -> Parser alwaysChomps keep
skip =
    implSkip


skip0 : Parser MightNotChomp skip -> Parser any keep -> Parser any keep
skip0 =
    implSkip


implSkip : Parser any skip -> Parser any2 keep -> Parser any3 keep
implSkip (P skipper) (P keeper) =
    P (keeper |. skipper)



{-
    .o88b. db   db  .d88b.   .d88b.  .d8888. d888888b d8b   db  d888b
   d8P  Y8 88   88 .8P  Y8. .8P  Y8. 88'  YP   `88'   888o  88 88' Y8b
   8P      88ooo88 88    88 88    88 `8bo.      88    88V8o 88 88
   8b      88~~~88 88    88 88    88   `Y8b.    88    88 V8o88 88  ooo
   Y8b  d8 88   88 `8b  d8' `8b  d8' db   8D   .88.   88  V888 88. ~8~
    `Y88P' YP   YP  `Y88P'   `Y88P'  `8888Y' Y888888P VP   V8P  Y888P


-}


or : Parser any a -> Parser AlwaysChomps a -> Parser any a
or (P this) (P prev) =
    P (ElmParser.oneOf [ prev, this ])


backtrackable : Parser any a -> Parser any a
backtrackable (P p) =
    P (ElmParser.backtrackable p)



{-
   d888888b d8888b.  .d8b.  d8b   db .d8888. d88888b  .d88b.  d8888b. .88b  d88. d888888b d8b   db  d888b
   `~~88~~' 88  `8D d8' `8b 888o  88 88'  YP 88'     .8P  Y8. 88  `8D 88'YbdP`88   `88'   888o  88 88' Y8b
      88    88oobY' 88ooo88 88V8o 88 `8bo.   88ooo   88    88 88oobY' 88  88  88    88    88V8o 88 88
      88    88`8b   88~~~88 88 V8o88   `Y8b. 88~~~   88    88 88`8b   88  88  88    88    88 V8o88 88  ooo
      88    88 `88. 88   88 88  V888 db   8D 88      `8b  d8' 88 `88. 88  88  88   .88.   88  V888 88. ~8~
      YP    88   YD YP   YP VP   V8P `8888Y' YP       `Y88P'  88   YD YP  YP  YP Y888888P VP   V8P  Y888P


-}


map : (a -> b) -> Parser any a -> Parser any b
map f (P p) =
    P (ElmParser.map f p)


andThenMightNotChomp : (a -> Parser MightNotChomp b) -> Parser MightNotChomp a -> Parser MightNotChomp b
andThenMightNotChomp =
    implAndThen


andThenChompsBefore : (a -> Parser any b) -> Parser AlwaysChomps a -> Parser alwaysChomps b
andThenChompsBefore =
    implAndThen


andThenChompsAfter : (a -> Parser AlwaysChomps b) -> Parser any a -> Parser alwaysChomps b
andThenChompsAfter =
    implAndThen


implAndThen : (a -> Parser any1 b) -> Parser any2 a -> Parser any3 b
implAndThen f (P p) =
    let
        elmParserF x =
            let
                (P p2) =
                    f x
            in
            p2
    in
    P (ElmParser.andThen elmParserF p)



{-
   db       .d88b.   .d88b.  d8888b. d888888b d8b   db  d888b
   88      .8P  Y8. .8P  Y8. 88  `8D   `88'   888o  88 88' Y8b
   88      88    88 88    88 88oodD'    88    88V8o 88 88
   88      88    88 88    88 88~~~      88    88 V8o88 88  ooo
   88booo. `8b  d8' `8b  d8' 88        .88.   88  V888 88. ~8~
   Y88888P  `Y88P'   `Y88P'  88      Y888888P VP   V8P  Y888P


-}


type alias Step state a =
    ElmParser.Step state a


loop :
    { initialState : state
    , loopCallback : state -> Parser AlwaysChomps state
    , doneCallback : state -> Parser any a
    }
    -> Parser any a
loop { initialState, loopCallback, doneCallback } =
    let
        continue state =
            loopCallback state
                |> map ElmParser.Loop

        done state =
            doneCallback state
                |> map ElmParser.Done

        callback state =
            continue state
                |> or (done state)
                |> (\(P p) -> p)
    in
    P (ElmParser.loop initialState callback)
