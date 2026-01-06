module SafeParser exposing
    ( Parser, run
    , AlwaysChomps, MightNotChomp, chompIf, chompWhile, getChompedString
    , symbol
    , succeed, problem
    , keep, keep0, skip, skip0
    , or, backtrackable
    , map, andThenMightNotChomp, andThenChompsBefore, andThenChompsAfter
    , Step, continue, done, loop
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
type AlwaysChomps
    = AlwayChomps Never


{-| A phantom type indicating that a `Parser` might succeed without chomping any
characters.
-}
type MightNotChomp
    = MightNotChomp Never


{-| Chomp one character if it passes the test.

    import SafeParser exposing (Parser, chompIf, run)

    chompUpper : Parser alwaysChomps ()
    chompUpper =
        chompIf Char.isUpper

    run chompUpper "T" --> Ok ()

So this can chomp a character like `"T"` and produces a `()` value.

-}
chompIf : (Char -> Bool) -> Parser alwaysChomps ()
chompIf test =
    P (ElmParser.chompIf test)


{-| Chomp zero or more characters if they pass the test. This is commonly
useful for chomping whitespace or variable names:

    import SafeParser exposing (Parser, MightNotChomp, chompIf, chompWhile, getChompedString, succeed, keep, skip0, run)

    whitespace : Parser MightNotChomp ()
    whitespace =
        chompWhile (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}')

    elmVar : Parser alwaysChomps String
    elmVar =
        succeed identity
            |> keep (chompIf Char.isLower)
            |> skip0 (chompWhile (\c -> Char.isAlphaNum c || c == '_'))
            |> getChompedString

    run elmVar "helloWorld" --> Ok "helloWorld"

Note: a `chompWhile` parser always succeeds!

-}
chompWhile : (Char -> Bool) -> Parser MightNotChomp ()
chompWhile test =
    P (ElmParser.chompWhile test)


{-| Sometimes parsers like int or variable cannot do exactly what you need. The
"chomping" family of functions is meant for that case! Maybe you need to parse
valid PHP variables like $x and $txt:

    import SafeParser exposing (Parser, chompIf, chompWhile, getChompedString, succeed, skip, skip0, run)

    php : Parser alwaysChomps String
    php =
        succeed ()
            |> skip (chompIf (\c -> c == '$'))
            |> skip (chompIf (\c -> Char.isAlpha c || c == '_'))
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


{-| A parser that succeeds without chomping any characters.

    import SafeParser exposing (run, succeed)

    run (succeed 90210  ) "mississippi" --> Ok 90210
    run (succeed 3.141  ) "mississippi" --> Ok 3.141
    run (succeed ()     ) "mississippi" --> Ok ()
    run (succeed Nothing) "mississippi" --> Ok Nothing

Seems weird on its own, but it is very useful in combination with other
functions. The docs for `keep` and `andThen` have some neat examples.

-}
succeed : a -> Parser MightNotChomp a
succeed a =
    P (ElmParser.succeed a)


{-| Indicate that a parser has reached a dead end. "Everything was going fine
until I ran into this problem." Check out the `andThen` docs to see an example
usage.
-}
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


{-| Keep values in a parser pipeline. For example, we could say:

    import SafeParser exposing (AlwaysChomps, Parser, andThenChompsBefore, chompIf, chompWhile, getChompedString, keep, keep0, problem, skip0, succeed, symbol, run)

    type alias Point =
        { x : Int, y : Int }

    int : Parser AlwaysChomps Int
    int =
        succeed (++)
            |> keep
                (chompIf Char.isDigit
                    |> getChompedString
                )
            |> keep0
                (chompWhile Char.isDigit
                    |> getChompedString
                )
            |> andThenChompsBefore
                (\str ->
                    case String.toInt str of
                        Just n ->
                            succeed n

                        Nothing ->
                            problem "not an int"
                )

    point : Parser AlwaysChomps Point
    point =
        succeed Point
            |> skip0 (symbol "(")
            |> keep int
            |> skip0 (symbol ",")
            |> keep int
            |> skip0 (symbol ")")

    run point "(123,456)" --> Ok { x = 123, y = 456 }

All the parsers in the `point` pipeline will chomp characters and produce
values. So `symbol "("` will chomp one paren and produce a `()` value.
Similarly, `int` will chomp some digits and produce an `Int` value. The
`keep`/`keep0` and `skip`/`skip0` functions just decide whether we give the
values to the Point function.

So in this case, we skip the `()` from `symbol "("`, we keep the `Int` from
`int`, etc.

-}
keep : Parser AlwaysChomps a -> Parser any (a -> b) -> Parser alwaysChomps b
keep =
    implKeep


{-| See docs for [`keep`](#keep). The `keep0` version of this function must be
used if you want to keep the value from a `MightNotChomp` parser.
-}
keep0 : Parser MightNotChomp a -> Parser any (a -> b) -> Parser any b
keep0 =
    implKeep


implKeep : Parser any a -> Parser any2 (a -> b) -> Parser any3 b
implKeep (P x) (P f) =
    P (f |= x)


{-| Skip values in a parser pipeline. For example, maybe we want to parse some
JavaScript variables:

    import SafeParser exposing (Parser, chompIf, chompWhile, getChompedString, succeed, skip, skip0, run)

    var : Parser alwaysChomps String
    var =
        succeed ()
            |> skip (chompIf isStartChar)
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
value. The `skip`/`skip0` functions are saying to still chomp all the
characters, but skip the two `()` values that get produced. No one cares about
them.

-}
skip : Parser AlwaysChomps skip -> Parser any keep -> Parser alwaysChomps keep
skip =
    implSkip


{-| See docs for [`skip`](#skip). The `skip0` version of this function must be
used if you want to skip the value produced by a `MightNotChomp` parser.
-}
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


{-| Use this instead of `elm/parser`'s `oneOf` to try a bunch of parsers and go
with the first one that succeeds.

    import SafeParser exposing (Parser, or, map, symbol, run)

    type NullableBool
        = Boolean Bool
        | Null

    nullableBool : Parser alwaysChomps NullableBool
    nullableBool =
        (map (\_ -> Boolean True) (symbol "true"))
            |> or (map (\_ -> Boolean False) (symbol "false"))
            |> or (map (\_ -> Null) (symbol "null"))

    run nullableBool "true" --> Ok (Boolean True)
    run nullableBool "null" --> Ok (Null)

-}
or : Parser any a -> Parser AlwaysChomps a -> Parser any a
or (P this) (P prev) =
    P (ElmParser.oneOf [ prev, this ])


{-| Like [`elm/parser`'s `backtrackable`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#backtrackable) - go read those docs!
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


{-| Like [`elm/parser`'s `map`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#map) - go read those docs!
-}
map : (a -> b) -> Parser any a -> Parser any b
map f (P p) =
    P (ElmParser.map f p)


{-| Like [`elm/parser`'s `andThen`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#andThen) - go read those docs!

This version of `andThen` is used when the initial and resulting parsers are both `MightNotChomp`.

-}
andThenMightNotChomp : (a -> Parser MightNotChomp b) -> Parser MightNotChomp a -> Parser MightNotChomp b
andThenMightNotChomp =
    implAndThen


{-| Like [`elm/parser`'s `andThen`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#andThen) - go read those docs!

This version of `andThen` is used when the initial parser is `AlwaysChomps`.

-}
andThenChompsBefore : (a -> Parser any b) -> Parser AlwaysChomps a -> Parser alwaysChomps b
andThenChompsBefore =
    implAndThen


{-| Like [`elm/parser`'s `andThen`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#andThen) - go read those docs!

This version of `andThen` is used when the resulting parser is `AlwaysChomps`.

-}
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
continue : Parser AlwaysChomps state -> Parser any (Step state a)
continue (P p) =
    P (ElmParser.map ElmParser.Loop p)


{-| For parsers that end a loop, it doesn't matter if they chomp or not - either
way, there's no risk of an infinite loop. So you can pass any parser to `done`.
-}
done : Parser any a -> Parser any (Step state a)
done =
    map ElmParser.Done


{-| The first callback for `loop` must always chomp - if it didn't, then:

  - Either it would be a non-chomping `Done`, in which case there's no need to
    use `loop` at all (it will always succeed, so you'd be better off just using
    the parser on its own without putting it in a `loop`)

  - Or it would be a non-chomping `Loop`, in which case we'd immediately fall
    into an infinite loop.

Subsequent callbacks that continue looping must also be `AlwaysChomps`, but if
they end the loop, then it's ok for them to be `MightNotChomp`.

If they are `MightNotChomp`, then the whole `loop` will be classified as
`MightNotChomp`. This is important to ensure that if we pass this `loop` into
_another_ `loop`, we won't end up with an infinite loop.

-}
loop :
    { initialState : state
    , firstCallback : state -> Parser AlwaysChomps (Step state a)
    , restCallbacks : state -> Parser any (Step state a)
    }
    -> Parser any a
loop { initialState, firstCallback, restCallbacks } =
    let
        first state =
            firstCallback state

        rest state =
            restCallbacks state

        callback state =
            first state
                |> or (rest state)
                |> (\(P p) -> p)
    in
    P (ElmParser.loop initialState callback)
