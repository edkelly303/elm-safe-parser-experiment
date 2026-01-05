module SafeParser exposing
    ( Parser, run
    , AlwaysChomps, MightNotChomp, chompIf, chompWhile, getChompedString
    , succeed, problem, keep, keep0, skip, skip0
    , or, oneOf, backtrackable
    , map, andThen0, andThen1, andThen2
    , Step, loop, continue, done, unsafelyDone
    )

{-|

@docs Parser, run
@docs AlwaysChomps, MightNotChomp, chompIf, chompWhile, getChompedString
@docs succeed, problem, keep, keep0, skip, skip0
@docs or, oneOf, backtrackable
@docs map, andThen0, andThen1, andThen2
@docs Step, loop, continue, done, unsafelyDone

-}

import Parser as ElmParser exposing ((|.), (|=))


type Parser constraints a
    = P (ElmParser.Parser a)


run : Parser constraints a -> String -> Result (List ElmParser.DeadEnd) a
run (P p) string =
    ElmParser.run p string


type AlwaysChomps
    = AlwayChomps Never


type MightNotChomp
    = MightNotChomp Never


getChompedString : Parser constraints a -> Parser constraints String
getChompedString (P p) =
    P (ElmParser.getChompedString p)


problem : String -> Parser MightNotChomp a
problem string =
    P (ElmParser.problem string)


chompIf : (Char -> Bool) -> Parser alwaysChomps ()
chompIf test =
    P (ElmParser.chompIf test)


chompWhile : (Char -> Bool) -> Parser MightNotChomp ()
chompWhile test =
    P (ElmParser.chompWhile test)


succeed : a -> Parser MightNotChomp a
succeed a =
    P (ElmParser.succeed a)


keep0 : Parser MightNotChomp a -> Parser constraints (a -> b) -> Parser constraints b
keep0 =
    implKeep


keep : Parser AlwaysChomps a -> Parser constraints (a -> b) -> Parser alwaysChomps b
keep =
    implKeep


implKeep : Parser constraints a -> Parser constraints2 (a -> b) -> Parser constraints3 b
implKeep (P x) (P f) =
    P (f |= x)


skip0 : Parser MightNotChomp skip -> Parser constraints keep -> Parser constraints keep
skip0 =
    implSkip


skip : Parser AlwaysChomps skip -> Parser constraints keep -> Parser alwaysChomps keep
skip =
    implSkip


implSkip : Parser constraints skip -> Parser constraints2 keep -> Parser constraints3 keep
implSkip (P skipper) (P keeper) =
    P (keeper |. skipper)


or : Parser constraints a -> Parser AlwaysChomps a -> Parser constraints a
or (P this) (P prev) =
    P (ElmParser.oneOf [ prev, this ])


oneOf : List (Parser constraints a) -> Parser constraints a
oneOf list =
    P (ElmParser.oneOf (List.map (\(P p) -> p) list))


backtrackable : Parser constraints a -> Parser constraints a
backtrackable (P p) =
    P (ElmParser.backtrackable p)


map : (a -> b) -> Parser constraints a -> Parser constraints b
map f (P p) =
    P (ElmParser.map f p)


andThenMightNotChomp : (a -> Parser MightNotChomp b) -> Parser MightNotChomp a -> Parser MightNotChomp b
andThenMightNotChomp =
    implAndThen


andThenChompsBefore : (a -> Parser constraints b) -> Parser AlwaysChomps a -> Parser alwaysChomps b
andThenChompsBefore =
    implAndThen


andThenChompsAfter : (a -> Parser AlwaysChomps b) -> Parser constraints a -> Parser alwaysChomps b
andThenChompsAfter =
    implAndThen


implAndThen : (a -> Parser constraints1 b) -> Parser constraints2 a -> Parser constraints3 b
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


type alias Step state a =
    ElmParser.Step state a


loop : state -> (state -> Parser AlwaysChomps (Step state a)) -> Parser alwaysChomps a
loop state callback =
    let
        unwrappedCallback s =
            let
                (P p) =
                    callback s
            in
            p
    in
    P (ElmParser.loop state unwrappedCallback)


continue : Parser constraints state -> Parser constraints (Step state a)
continue =
    map ElmParser.Loop


done : Parser AlwaysChomps a -> Parser alwaysChomps (Step state a)
done =
    implDone


doneUnsafely : Parser MightNotChomp a -> Parser alwaysChomps (Step state a)
doneUnsafely =
    implDone


implDone : Parser constraints a -> Parser constraints2 (ElmParser.Step state a)
implDone (P p) =
    P (ElmParser.map ElmParser.Done p)
