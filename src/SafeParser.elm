module SafeParser exposing
    ( Chomps
    , Parser
    , Step
    , backtrackable
    , chompIf
    , chompWhile
    , continue
    , done
    , done0
    , keep
    , keep0
    , loop
    , map
    , oneOf
    , run
    , skip
    , skip0
    , succeed
    )

import Parser as OP exposing ((|.), (|=))


type Parser constraints a
    = P (OP.Parser a)


type Step state a
    = Loop state
    | Done a


type Chomps
    = Chomps Never


run : Parser constraints a -> String -> Result (List OP.DeadEnd) a
run (P p) string =
    OP.run p string


backtrackable : Parser constraints a -> Parser constraints a
backtrackable (P p) =
    P (OP.backtrackable p)


chompIf : (Char -> Bool) -> Parser chomps ()
chompIf test =
    P (OP.chompIf test)


chompWhile : (Char -> Bool) -> Parser (Maybe Chomps) ()
chompWhile test =
    P (OP.chompWhile test)


oneOf : List (Parser constraints a) -> Parser constraints a
oneOf list =
    P (OP.oneOf (List.map (\(P p) -> p) list))


succeed : a -> Parser (Maybe Chomps) a
succeed a =
    P (OP.succeed a)


done : Parser Chomps a -> Parser chomps (Step state a)
done (P p) =
    P (OP.map Done p)


done0 : Parser (Maybe Chomps) a -> Parser chomps (Step state a)
done0 (P p) =
    P (OP.map Done p)


continue : Parser constraints state -> Parser constraints (Step state a)
continue =
    map Loop


map : (a -> b) -> Parser constraints a -> Parser constraints b
map f (P p) =
    P (OP.map f p)


keep0 : Parser (Maybe Chomps) a -> Parser constraints (a -> b) -> Parser (Maybe Chomps) b
keep0 (P x) (P f) =
    P (f |= x)


keep : Parser Chomps a -> Parser constraints (a -> b) -> Parser chomps b
keep (P x) (P f) =
    P (f |= x)


skip0 : Parser (Maybe Chomps) skip -> Parser constraints keep -> Parser (Maybe Chomps) keep
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
                |> OP.map
                    (\p_ ->
                        case p_ of
                            Loop s_ ->
                                OP.Loop s_

                            Done a ->
                                OP.Done a
                    )
    in
    P (OP.loop state unwrappedCallback)
