module SafeParser exposing
    ( Parser, run, toElmParser
    , OneOrMore, ZeroOrMore, chompIf, chompWhile, getChompedString, mapChompedString
    , spaces, symbol, token, keyword, variable, end
    , getOffset
    , succeed, problem
    , keep1, keep0, skip1, skip0
    , oneOf, or, commit, backtrackable
    , map, andThen00, andThen10, andThen01
    , Step, continue, done, loop
    )

{-|


## Running parsers

@docs Parser, run, toElmParser


## Chomping inputs

@docs OneOrMore, ZeroOrMore, chompIf, chompWhile, getChompedString, mapChompedString


## Generally useful parsers

@docs spaces, symbol, token, keyword, variable, end


## Metadata parsers

@docs getOffset


## Succeeding and failing

@docs succeed, problem


## Combining parsers

@docs keep1, keep0, skip1, skip0


## Choosing parsers

@docs oneOf, or, commit, backtrackable


## Transforming parsers

@docs map, andThen00, andThen10, andThen01


## Looping parsers

@docs Step, continue, done, loop

-}

import Parser as ElmParser exposing ((|.), (|=))
import Set



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


{-| Convert a `Parser` from this package to `elm/parser`'s `Parser` type.

This will lose type safety, but might be useful if you need to interoperate with
other Elm parser libraries like `dmy/elm-pratt-parser`.

-}
toElmParser : Parser any a -> ElmParser.Parser a
toElmParser (P p) =
    p



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

    import SafeParser as SP

    chompUpper : SP.Parser oneOrMore ()
    chompUpper =
        SP.chompIf Char.isUpper

    SP.run chompUpper "T" --> Ok ()

So this can chomp a character like `"T"` and produces a `()` value.

-}
chompIf : (Char -> Bool) -> Parser oneOrMore ()
chompIf test =
    P (ElmParser.chompIf test)


{-| Chomp zero or more characters if they pass the test. This is commonly
useful for chomping whitespace or variable names:

    import SafeParser as SP

    elmVar : SP.Parser oneOrMore String
    elmVar =
        SP.succeed identity
            |> SP.keep1 (SP.chompIf Char.isLower)
            |> SP.skip0 (SP.chompWhile (\c -> Char.isAlphaNum c || c == '_'))
            |> SP.getChompedString

    SP.run elmVar "helloWorld" --> Ok "helloWorld"

Note: a `chompWhile` parser always succeeds!

-}
chompWhile : (Char -> Bool) -> Parser ZeroOrMore ()
chompWhile test =
    P (ElmParser.chompWhile test)


{-| Like [`elm/parser`'s
`getChompedString`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#getChompedString).
Go read those docs!
-}
getChompedString : Parser any a -> Parser any String
getChompedString (P p) =
    P (ElmParser.getChompedString p)


{-| Like [`elm/parser`'s
`mapChompedString`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#mapChompedString).
Go read those docs!
-}
mapChompedString : (String -> a -> b) -> Parser any a -> Parser any b
mapChompedString f (P p) =
    P (ElmParser.mapChompedString f p)


{-| Like [`elm/parser`'s
`getOffset`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#getOffset).
Go read those docs!
-}
getOffset : Parser ZeroOrMore Int
getOffset =
    P ElmParser.getOffset



{-
   db   db d88888b db      d8888b. d88888b d8888b. .d8888.
   88   88 88'     88      88  `8D 88'     88  `8D 88'  YP
   88ooo88 88ooooo 88      88oodD' 88ooooo 88oobY' `8bo.
   88~~~88 88~~~~~ 88      88~~~   88~~~~~ 88`8b     `Y8b.
   88   88 88.     88booo. 88      88.     88 `88. db   8D
   YP   YP Y88888P Y88888P 88      Y88888P 88   YD `8888Y'


-}


{-| Parse symbols like `(` and `,`.

    import SafeParser as SP
    import Parser

    SP.run (SP.symbol "[") "["
    --> Ok ()

    SP.run (SP.symbol "[") "4"
    --> Err [ { row = 1, col = 1, problem = Parser.ExpectingSymbol "[" } ]

    SP.run (SP.symbol "") "whatever"
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


{-| Parse exactly the given `String`, without caring what comes next.

Here is how you could use `token` to implement `keyword`:

    import SafeParser as SP
    import Parser

    keyword : String -> SP.Parser oneOrMore ()
    keyword kwd =
        SP.succeed identity
            |> SP.skip1 (SP.backtrackable (SP.token kwd))
            |> SP.keep0
                (SP.map (\_ -> True) (SP.backtrackable (SP.chompIf isVarChar))
                    |> SP.or (SP.succeed False)
                )
            |> SP.andThen10 (checkEnding kwd)

    checkEnding : String -> Bool -> SP.Parser SP.ZeroOrMore ()
    checkEnding kwd isBadEnding =
        if isBadEnding then
            SP.problem ("expecting the `" ++ kwd ++ "` keyword")
        else
            SP.commit ()

    isVarChar : Char -> Bool
    isVarChar char =
        Char.isAlphaNum char || char == '_'

    SP.run (SP.token "hello") "helloWorld"
    --> Ok ()

    SP.run (SP.token "") "whatever"
    --> Err [ { row = 1, col = 1, problem = Parser.Problem "The `token` parser cannot match an empty string" } ]

**Note:** unlike the `elm/parser` version of this function, `token` will always
fail if asked to match an empty string.

-}
token : String -> Parser oneOrMore ()
token str =
    P
        (if String.isEmpty str then
            ElmParser.problem "The `token` parser cannot match an empty string"

         else
            ElmParser.token str
        )


{-| Parse keywords like `let`, `case`, and `type`.

    import SafeParser as SP
    import Parser as P

    SP.run (SP.keyword "let") "let" --> Ok ()
    SP.run (SP.keyword "let") "var" --> Err [ { col = 1, problem = P.ExpectingKeyword "let", row = 1 } ]
    SP.run (SP.keyword "let") "letters" --> Err [ { col = 1, problem = P.ExpectingKeyword "let", row = 1 } ]

Note: Notice the third case there! `keyword` actually looks ahead one character to
make sure it is not a letter, number, or underscore. The goal is to help with
parsers like this:

    import SafeParser as SP

    elmVar : SP.Parser oneOrMore String
    elmVar =
        SP.succeed identity
            |> SP.keep1 (SP.chompIf Char.isLower)
            |> SP.skip0 (SP.chompWhile (\c -> Char.isAlphaNum c || c == '_'))
            |> SP.getChompedString

    letBinding =
        SP.succeed identity
            |> SP.skip1 (SP.keyword "let")
            |> SP.skip0 SP.spaces
            |> SP.keep1 elmVar
            |> SP.skip0 SP.spaces
            |> SP.skip1 (SP.symbol "=")

    SP.run letBinding "let x ="
    --> Ok "x"

The trouble is that `spaces` may chomp zero characters (to handle expressions like
`[1,2]` and `[ 1 , 2 ]`) and in this case, it would mean `letters` could be parsed as
`let ters` and then wonder where the equals sign is! Check out the token docs if
you need to customize this!

-}
keyword : String -> Parser oneOrMore ()
keyword str =
    P
        (if String.isEmpty str then
            ElmParser.problem "The `keyword` parser cannot match an empty string"

         else
            ElmParser.keyword str
        )


{-| Parse zero or more ' ', '\\n', and '\\r' characters.

The implementation is pretty simple:

    import SafeParser as SP

    spaces : SP.Parser SP.ZeroOrMore ()
    spaces =
        SP.chompWhile (\c -> c == ' ' || c == '\n' || c == '\u{000D}')

    SP.run spaces "  \n" --> Ok ()
    SP.run spaces "" --> Ok ()
    SP.run spaces "xxx" --> Ok ()

Note that as a `ZeroOrMore` parser, `spaces` will alway succeed.

-}
spaces : Parser ZeroOrMore ()
spaces =
    P ElmParser.spaces


{-| Create a parser for variables. If we wanted to parse type variables in Elm,
we could try something like this:

    import SafeParser as SP
    import Set
    import Parser as P

    typeVar : SP.Parser oneOrMore String
    typeVar =
        SP.variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "let", "in", "case", "of" ]
            }

    SP.run typeVar "abc" --> Ok "abc"
    SP.run typeVar "Abc" --> Err [ { col = 1, problem = P.ExpectingVariable, row = 1 } ]
    SP.run typeVar "let" --> Err [ { col = 1, problem = P.ExpectingVariable, row = 1 } ]

This is saying it must start with a lower-case character. After that, characters
can be letters, numbers, or underscores. It is also saying that if you run into
any of these reserved names, it is definitely not a variable.

-}
variable :
    { start : Char -> Bool, inner : Char -> Bool, reserved : Set.Set String }
    -> Parser oneOrMore String
variable args =
    P (ElmParser.variable args)


{-| Check if you have reached the end of the string you are parsing.

    import SafeParser as SP
    import Parser as P

    oneNumber : SP.Parser oneOrMore String
    oneNumber =
        SP.succeed ()
            |> SP.skip1 (SP.chompIf (Char.isDigit))
            |> SP.skip0 (SP.chompWhile (Char.isDigit))
            |> SP.getChompedString

    justOneNumber : SP.Parser oneOrMore String
    justOneNumber =
        SP.succeed identity
            |> SP.keep1 oneNumber
            |> SP.skip1 SP.end

    SP.run justOneNumber "90210" --> Ok "90210"
    SP.run justOneNumber "1 + 2" --> Err [ { col = 2, problem = P.ExpectingEnd, row = 1 } ]
    SP.run oneNumber     "1 + 2" --> Ok "1"

Parsers can succeed without parsing the whole string. Ending your parser with end guarantees that you have successfully parsed the whole string.

-}
end : Parser oneOrMore ()
end =
    P ElmParser.end



{-
   .d8888. db    db  .o88b.  .o88b. d88888b d88888b d8888b. d888888b d8b   db  d888b        .d8b.  d8b   db d8888b.      d88888b  .d8b.  d888888b db      d888888b d8b   db  d888b
   88'  YP 88    88 d8P  Y8 d8P  Y8 88'     88'     88  `8D   `88'   888o  88 88' Y8b      d8' `8b 888o  88 88  `8D      88'     d8' `8b   `88'   88        `88'   888o  88 88' Y8b
   `8bo.   88    88 8P      8P      88ooooo 88ooooo 88   88    88    88V8o 88 88           88ooo88 88V8o 88 88   88      88ooo   88ooo88    88    88         88    88V8o 88 88
     `Y8b. 88    88 8b      8b      88~~~~~ 88~~~~~ 88   88    88    88 V8o88 88  ooo      88~~~88 88 V8o88 88   88      88~~~   88~~~88    88    88         88    88 V8o88 88  ooo
   db   8D 88b  d88 Y8b  d8 Y8b  d8 88.     88.     88  .8D   .88.   88  V888 88. ~8~      88   88 88  V888 88  .8D      88      88   88   .88.   88booo.   .88.   88  V888 88. ~8~
   `8888Y' ~Y8888P'  `Y88P'  `Y88P' Y88888P Y88888P Y8888D' Y888888P VP   V8P  Y888P       YP   YP VP   V8P Y8888D'      YP      YP   YP Y888888P Y88888P Y888888P VP   V8P  Y888P


-}


{-| A parser that succeeds without chomping any characters.

    import SafeParser as SP

    SP.run (SP.succeed 90210  ) "mississippi" --> Ok 90210
    SP.run (SP.succeed 3.141  ) "mississippi" --> Ok 3.141
    SP.run (SP.succeed ()     ) "mississippi" --> Ok ()
    SP.run (SP.succeed Nothing) "mississippi" --> Ok Nothing

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
problem : String -> Parser any a
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

    import SafeParser as SP

    type alias Point =
        { x : Int, y : Int }

    int : SP.Parser SP.OneOrMore Int
    int =
        SP.succeed (++)
            |> SP.keep1
                (SP.chompIf Char.isDigit
                    |> SP.getChompedString
                )
            |> SP.keep0
                (SP.chompWhile Char.isDigit
                    |> SP.getChompedString
                )
            |> SP.andThen10
                (\str ->
                    case String.toInt str of
                        Just n ->
                            SP.succeed n

                        Nothing ->
                            SP.problem "not an int"
                )

    point : SP.Parser SP.OneOrMore Point
    point =
        SP.succeed Point
            |> SP.skip0 (SP.symbol "(")
            |> SP.keep1 int
            |> SP.skip0 (SP.symbol ",")
            |> SP.keep1 int
            |> SP.skip0 (SP.symbol ")")

    SP.run point "(123,456)" --> Ok { x = 123, y = 456 }

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

    import SafeParser as SP

    var : SP.Parser oneOrMore String
    var =
        SP.succeed ()
            |> SP.skip1 (SP.chompIf isStartChar)
            |> SP.skip0 (SP.chompWhile isInnerChar)
            |> SP.getChompedString

    isStartChar : Char -> Bool
    isStartChar char =
        Char.isAlpha char || char == '_' || char == '$'

    isInnerChar : Char -> Bool
    isInnerChar char =
        isStartChar char || Char.isDigit char

    SP.run var "$hello" --> Ok "$hello"

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


{-| Take a list of `OneOrMore` parsers and try them one after the other until
one of them succeeds.

    import SafeParser as SP
    import Parser as P

    bool : SP.Parser oneOrMore Bool
    bool =
        SP.oneOf
            [ SP.symbol "true" |> SP.map (\_ -> True)
            , SP.symbol "false" |> SP.map (\_ -> False)
            ]

    SP.run bool "true" --> Ok True
    SP.run bool "tru" --> Err [ { col = 1, problem = ExpectingSymbol "true", row = 1 }, { col = 1, problem = P.ExpectingSymbol "false", row = 1 } ]
    SP.run (SP.oneOf []) "a" --> Err [ { col = 1, problem = P.Problem "The `oneOf` parser should be passed a list of at least two parsers", row = 1 } ]

-}
oneOf : List (Parser OneOrMore a) -> Parser any a
oneOf ps =
    case ps of
        _ :: _ :: _ ->
            ps
                |> List.map (\(P p) -> p)
                |> ElmParser.oneOf
                |> P

        _ ->
            problem "The `oneOf` parser should be passed a list of at least two parsers"


{-| Use this if you need to add a `ZeroOrMore` parser as the final alternative
at the end of a `oneOf` list. By ensuring that only the _last_ alternative
parser can be `ZeroOrMore`, this prevents you from accidentally creating parsers
where some of the alternatives are unreachable.

    import SafeParser as SP

    type NullableBool
        = Boolean Bool
        | Null

    nullableBool : SP.Parser oneOrMore NullableBool
    nullableBool =
        SP.oneOf
            [ SP.symbol "true" |> SP.map (\_ -> Boolean True)
            , SP.symbol "false" |> SP.map (\_ -> Boolean False)
            ]
            |> SP.or (SP.symbol "null" |> SP.map (\_ -> Null))


    SP.run nullableBool "true" --> Ok (Boolean True)
    SP.run nullableBool "null" --> Ok (Null)

-}
or : Parser any a -> Parser OneOrMore a -> Parser any a
or (P this) (P prev) =
    P (ElmParser.oneOf [ prev, this ])


{-| Like [`elm/parser`'s
`commit`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#commit).
Go read those docs!
-}
commit : a -> Parser any a
commit p =
    P (ElmParser.commit p)


{-| Like [`elm/parser`'s
`backtrackable`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#backtrackable).
Go read those docs!
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
chomp one or more characters, so `andThen01` can return a `OneOrMore` parser.

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

    import SafeParser as SP

    digits =
        SP.loop
            ()
            (\state ->
                (SP.chompIf Char.isDigit |> SP.continue)
                    |> SP.or (SP.succeed () |> SP.done)
            )
            |> SP.getChompedString

    SP.run digits "1234abc" --> Ok "1234"

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
