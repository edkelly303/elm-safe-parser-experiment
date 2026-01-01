module SafeParser exposing
    ( Chomps
    , Parser
    , Step
    , andThen0
    , andThen1
    , andThen2
    , backtrackable
    , chompIf
    , chompWhile
    , continue
    , done
    , keep
    , keep0
    , loop
    , map
    , oneOf
    , run
    , skip
    , skip0
    , succeed
    , unsafelyDone
    )

import Parser as ElmParser exposing ((|.), (|=))


type Parser constraints a
    = P (ElmParser.Parser a)


type Step state a
    = Loop state
    | Done a


type Chomps
    = Chomps Never


run : Parser constraints a -> String -> Result (List ElmParser.DeadEnd) a
run (P p) string =
    ElmParser.run p string


backtrackable : Parser constraints a -> Parser constraints a
backtrackable (P p) =
    P (ElmParser.backtrackable p)


chompIf : (Char -> Bool) -> Parser chomps ()
chompIf test =
    P (ElmParser.chompIf test)


chompWhile : (Char -> Bool) -> Parser (Maybe Chomps) ()
chompWhile test =
    P (ElmParser.chompWhile test)


oneOf : List (Parser constraints a) -> Parser constraints a
oneOf list =
    P (ElmParser.oneOf (List.map (\(P p) -> p) list))


succeed : a -> Parser (Maybe Chomps) a
succeed a =
    P (ElmParser.succeed a)


done : Parser Chomps a -> Parser chomps (Step state a)
done (P p) =
    P (ElmParser.map Done p)


unsafelyDone : Parser (Maybe Chomps) a -> Parser chomps (Step state a)
unsafelyDone (P p) =
    P (ElmParser.map Done p)


continue : Parser constraints state -> Parser constraints (Step state a)
continue =
    map Loop


map : (a -> b) -> Parser constraints a -> Parser constraints b
map f (P p) =
    P (ElmParser.map f p)


andThen0 : (a -> Parser (Maybe Chomps) b) -> Parser (Maybe Chomps) a -> Parser (Maybe Chomps) b
andThen0 =
    andThenImplementation


andThen1 : (a -> Parser constraints b) -> Parser Chomps a -> Parser chomps b
andThen1 =
    andThenImplementation


andThen2 : (a -> Parser Chomps b) -> Parser constraints a -> Parser chomps b
andThen2 =
    andThenImplementation


andThenImplementation : (a -> Parser constraints1 b) -> Parser constraints2 a -> Parser constraints3 b
andThenImplementation f (P p) =
    let
        unwrappedF =
            \x ->
                let
                    (P p2) =
                        f x
                in
                p2
    in
    P (ElmParser.andThen unwrappedF p)


keep0 : Parser (Maybe Chomps) a -> Parser constraints (a -> b) -> Parser constraints b
keep0 (P x) (P f) =
    P (f |= x)


keep : Parser Chomps a -> Parser constraints (a -> b) -> Parser chomps b
keep (P x) (P f) =
    P (f |= x)


skip0 : Parser (Maybe Chomps) skip -> Parser constraints keep -> Parser constraints keep
skip0 _ (P keeper) =
    P keeper


skip : Parser Chomps skip -> Parser constraints keep -> Parser chomps keep
skip _ (P keeper) =
    P keeper


loop : state -> (state -> Parser Chomps (Step state a)) -> Parser chomps a
loop state callback =
    let
        unwrappedCallback s =
            let
                (P p) =
                    callback s
            in
            p
                |> ElmParser.map
                    (\p_ ->
                        case p_ of
                            Loop s_ ->
                                ElmParser.Loop s_

                            Done a ->
                                ElmParser.Done a
                    )
    in
    P (ElmParser.loop state unwrappedCallback)
