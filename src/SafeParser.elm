module SafeParser exposing
    ( Parser, run
    , OneOrMore, ZeroOrMore, chompIf, chompWhile, getChompedString
    , symbol
    , succeed, problem
    , keep1, keep0, skip1, skip0
    , or, backtrackable
    , map, andThen00, andThen10, andThen01
    , Step, continue, done, loop
    )

{-|


## Running parsers

@docs Parser, run


## Chomping inputs

@docs OneOrMore, ZeroOrMore, chompIf, chompWhile, getChompedString


## Generally useful parsers

@docs symbol


## Succeeding and failing

@docs succeed, problem


## Combining parsers

@docs keep1, keep0, skip1, skip0


## Choosing parsers

@docs or, backtrackable


## Transforming parsers

@docs map, andThen00, andThen10, andThen01


## Looping parsers

@docs Step, continue, done, loop

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


{-| Very similar to `elm/parser`'s `Parser` type, but with an extra phantom type
variable that allows us to track and propagate information about whether the
parser is guaranteed to chomp characters when it succeeds.
-}
type Parser any a
    = P (ElmParser.Parser a)


{-| Run a `Parser` over a string. This is exactly like `elm/parser`'s `run`.
-}
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


{-| A phantom type indicating that a `Parser` is guaranteed to chomp at least
one character when it succeeds.
-}
type OneOrMore
    = OneOrMore Never


{-| A phantom type indicating that a `Parser` might succeed without chomping any
characters.
-}
type ZeroOrMore
    = ZeroOrMore Never


{-| Chomp one character if it passes the test.

    import SafeParser exposing (Parser, chompIf, run)

    chompUpper : Parser oneOrMore ()
    chompUpper =
        chompIf Char.isUpper

    run chompUpper "T" --> Ok ()

So this can chomp a character like `"T"` and produces a `()` value.

-}
chompIf : (Char -> Bool) -> Parser oneOrMore ()
chompIf test =
    P (ElmParser.chompIf test)


{-| Chomp zero or more characters if they pass the test. This is commonly
useful for chomping whitespace or variable names:

    import SafeParser exposing (Parser, ZeroOrMore, chompIf, chompWhile, getChompedString, succeed, keep1, skip0, run)

    whitespace : Parser ZeroOrMore ()
    whitespace =
        chompWhile (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}')

    elmVar : Parser oneOrMore String
    elmVar =
        succeed identity
            |> keep1 (chompIf Char.isLower)
            |> skip0 (chompWhile (\c -> Char.isAlphaNum c || c == '_'))
            |> getChompedString

    run elmVar "helloWorld" --> Ok "helloWorld"

Note: a `chompWhile` parser always succeeds!

-}
chompWhile : (Char -> Bool) -> Parser ZeroOrMore ()
chompWhile test =
    P (ElmParser.chompWhile test)


{-| Sometimes parsers like int or variable cannot do exactly what you need. The
"chomping" family of functions is meant for that case! Maybe you need to parse
valid PHP variables like $x and $txt:

    import SafeParser exposing (Parser, chompIf, chompWhile, getChompedString, succeed, skip1, skip0, run)

    php : Parser oneOrMore String
    php =
        succeed ()
            |> skip1 (chompIf (\c -> c == '$'))
            |> skip1 (chompIf (\c -> Char.isAlpha c || c == '_'))
            |> skip0 (chompWhile (\c -> Char.isAlphaNum c || c == '_'))
            |> getChompedString

    run php "$my_var" --> Ok "$my_var"

The idea is that you create a bunch of chompers that validate the underlying
characters. Then getChompedString extracts the underlying String efficiently.

-}
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


{-| Parse symbols like `(` and `,`.

    import SafeParser exposing (symbol, run)
    import Parser

    run (symbol "[") "["
    --> Ok ()

    run (symbol "[") "4"
    --> Err [ { row = 1, col = 1, problem = Parser.ExpectingSymbol "[" } ]

    run (symbol "") "whatever"
    --> Err [ { row = 1, col = 1, problem = Parser.Problem "The `symbol` parser cannot match an empty string" } ]

**Note:** unlike the `elm/parser` version of this function, `symbol` will always
fail if asked to match an empty string.

-}
symbol : String -> Parser oneOrMore ()
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


{-| A parser that succeeds without chomping any characters.

    import SafeParser exposing (run, succeed)

    run (succeed 90210  ) "mississippi" --> Ok 90210
    run (succeed 3.141  ) "mississippi" --> Ok 3.141
    run (succeed ()     ) "mississippi" --> Ok ()
    run (succeed Nothing) "mississippi" --> Ok Nothing

Seems weird on its own, but it is very useful in combination with other
functions. The docs for `keep1` and `andThen` have some neat examples.

-}
succeed : a -> Parser ZeroOrMore a
succeed a =
    P (ElmParser.succeed a)


{-| Indicate that a parser has reached a dead end. "Everything was going fine
until I ran into this problem." Check out the `andThen` docs to see an example
usage.
-}
problem : String -> Parser ZeroOrMore a
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


{-| Keep values produced by a `OneOrMore` parser in a parser pipeline.

For example, we could say:

    import SafeParser exposing (OneOrMore, Parser, andThen10, chompIf, chompWhile, getChompedString, keep1, keep0, problem, skip0, succeed, symbol, run)

    type alias Point =
        { x : Int, y : Int }

    int : Parser OneOrMore Int
    int =
        succeed (++)
            |> keep1
                (chompIf Char.isDigit
                    |> getChompedString
                )
            |> keep0
                (chompWhile Char.isDigit
                    |> getChompedString
                )
            |> andThen10
                (\str ->
                    case String.toInt str of
                        Just n ->
                            succeed n

                        Nothing ->
                            problem "not an int"
                )

    point : Parser OneOrMore Point
    point =
        succeed Point
            |> skip0 (symbol "(")
            |> keep1 int
            |> skip0 (symbol ",")
            |> keep1 int
            |> skip0 (symbol ")")

    run point "(123,456)" --> Ok { x = 123, y = 456 }

All the parsers in the `point` pipeline will chomp characters and produce
values. So `symbol "("` will chomp one paren and produce a `()` value.
Similarly, `int` will chomp some digits and produce an `Int` value. The
`keep1`/`keep0` and `skip1`/`skip0` functions just decide whether we give the
values to the Point function.

So in this case, we skip the `()` from `symbol "("`, we keep the `Int` from
`int`, etc.

-}
keep1 : Parser OneOrMore a -> Parser any (a -> b) -> Parser oneOrMore b
keep1 =
    implKeep


{-| Keep values produced by a `ZeroOrMore` parser in a parser pipeline.

See docs for [`keep1`](#keep1).

-}
keep0 : Parser ZeroOrMore a -> Parser any (a -> b) -> Parser any b
keep0 =
    implKeep


implKeep : Parser any a -> Parser any2 (a -> b) -> Parser any3 b
implKeep (P x) (P f) =
    P (f |= x)


{-| Skip values produced by a `OneOrMore` parser in a parser pipeline.

For example, maybe we want to parse some JavaScript variables:

    import SafeParser exposing (Parser, chompIf, chompWhile, getChompedString, succeed, skip1, skip0, run)

    var : Parser oneOrMore String
    var =
        succeed ()
            |> skip1 (chompIf isStartChar)
            |> skip0 (chompWhile isInnerChar)
            |> getChompedString

    isStartChar : Char -> Bool
    isStartChar char =
        Char.isAlpha char || char == '_' || char == '$'

    isInnerChar : Char -> Bool
    isInnerChar char =
        isStartChar char || Char.isDigit char

    run var "$hello" --> Ok "$hello"

`chompIf isStartChar` can chomp one character and produce a `()` value.
`chompWhile isInnerChar` can chomp zero or more characters and produce a `()`
value. The `skip1`/`skip0` functions are saying to still chomp all the
characters, but skip the two `()` values that get produced. No one cares about
them.

-}
skip1 : Parser OneOrMore skip -> Parser any keep -> Parser oneOrMore keep
skip1 =
    implSkip


{-| Skip values produced by a `ZeroOrMore` parser in a parser pipeline.

See docs for [`skip1`](#skip1).

-}
skip0 : Parser ZeroOrMore skip -> Parser any keep -> Parser any keep
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


{-| Use this instead of `elm/parser`'s `oneOf` to try a bunch of parsers and go
with the first one that succeeds.

Only the _last_ parser passed to a pipeline of `or`s can be a `ZeroOrMore`
parser. This prevents you from accidentally creating pipelines where some of the
parsers are unreachable.

    import SafeParser exposing (Parser, or, map, symbol, run)

    type NullableBool
        = Boolean Bool
        | Null

    nullableBool : Parser oneOrMore NullableBool
    nullableBool =
        (map (\_ -> Boolean True) (symbol "true"))
            |> or (map (\_ -> Boolean False) (symbol "false"))
            |> or (map (\_ -> Null) (symbol "null"))

    run nullableBool "true" --> Ok (Boolean True)
    run nullableBool "null" --> Ok (Null)

-}
or : Parser any a -> Parser OneOrMore a -> Parser any a
or (P this) (P prev) =
    P (ElmParser.oneOf [ prev, this ])


{-| Like [`elm/parser`'s
`backtrackable`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#backtrackable).Go
read those docs!
-}
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


{-| Like [`elm/parser`'s
`map`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#map). Go
read those docs!
-}
map : (a -> b) -> Parser any a -> Parser any b
map f (P p) =
    P (ElmParser.map f p)


{-| Like [`elm/parser`'s
`andThen`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#andThen).
Go read those docs!

`andThen00` is used when the parser returned by the callback in the first
argument is `ZeroOrMore`, and the parser passed as the second argument is _also_
`ZeroOrMore`. Since neither of these parsers is guaranteed to chomp any
characters, `andThen00` returns a `ZeroOrMore` parser.

-}
andThen00 : (a -> Parser ZeroOrMore b) -> Parser ZeroOrMore a -> Parser ZeroOrMore b
andThen00 =
    implAndThen


{-| Like [`elm/parser`'s
`andThen`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#andThen).
Go read those docs!

`andThen10` is used when the parser passed as the second argument is
`OneOrMore`. This guarantees that if the parser succeeds, it must chomp one or
more characters, so `andThen10` can return a `OneOrMore` parser.

-}
andThen10 : (a -> Parser any b) -> Parser OneOrMore a -> Parser oneOrMore b
andThen10 =
    implAndThen


{-| Like [`elm/parser`'s
`andThen`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#andThen).
Go read those docs!

`andThen01` is used when the parser that results from the callback in the first
argument is `OneOrMore`. This guarantees that if the parser succeeds, it must
chomp one or more characters, so `andThen10` can return a `OneOrMore` parser.

-}
andThen01 : (a -> Parser OneOrMore b) -> Parser any a -> Parser oneOrMore b
andThen01 =
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


{-| Instead of directly exposing the `elm/parser` constructors for `Step`
(`Loop` and `Done`), we provide two functions, `continue` and `done` that are
equivalent to `map Loop` and `map Done`.
-}
type alias Step state a =
    ElmParser.Step state a


{-| Ensure that any parser that is going to continue looping is always going to
chomp something if it succeeds. This makes it impossible to fall into infinite
loops.
-}
continue : Parser OneOrMore state -> Parser any (Step state a)
continue (P p) =
    P (ElmParser.map ElmParser.Loop p)


{-| For parsers that end a loop, it doesn't matter if they chomp or not - either
way, there's no risk of an infinite loop. So you can pass any parser to `done`.
-}
done : Parser any a -> Parser any (Step state a)
done =
    map ElmParser.Done


{-| Run the first callback with the initial state and if it succeeds, either
continue the loop or return a value. If the first callback fails, run the second
callback and either continue or return a value.

    import SafeParser exposing (loop, chompIf, continue, or, succeed, done, getChompedString, run)

    digits =
        loop
            ()
            (\state ->
                (chompIf Char.isDigit |> continue)
                    |> or (succeed () |> done)
            )
            |> getChompedString

    run digits "1234abc" --> Ok "1234"
-}
loop :
    state
    -> (state -> Parser any (Step state a))
    -> Parser any a
loop initialState callback =
    P
        (ElmParser.loop
            initialState
            (callback >> (\(P p) -> p))
        )
