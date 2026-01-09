module Main exposing (main)

{-| This example is adapted from Alex Korban's `elm/parser` tutorial at
<https://korban.net/posts/elm/2018-09-07-introduction-elm-parser>
-}

import Browser
import Html exposing (Html, button, div, h2, input, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Parser
import SafeParser
    exposing
        ( OneOrMore
        , Parser
          --, Step
        , ZeroOrMore
        , andThen
          --, backtrackable
        , chompIf
        , chompWhile
        , continue
        , done
        , getChompedString
        , keep
        , loop
        , map
        , oneOf
        , or
        , problem
        , run
        , skip
        , succeed
        , symbol
        )


type alias Phone =
    { countryCode : Maybe Int
    , areaCode : Maybe Int
    , phone : Int
    }


whitespace : Parser zeroOrMore ()
whitespace =
    chompWhile (\c -> c == ' ')


countryCode : Parser zeroOrMore (Maybe Int)
countryCode =
    let
        justValidCountryCode =
            succeed Just
                |> skip whitespace
                |> skip (symbol "+")
                |> keep int
                |> skip whitespace

        nothing =
            succeed Nothing
    in
    justValidCountryCode
        |> or nothing


int : Parser OneOrMore Int
int =
    succeed (\first rest -> first ++ rest)
        |> keep (chompIf Char.isDigit |> getChompedString)
        |> keep (chompWhile Char.isDigit |> getChompedString)
        |> andThen
            (\str ->
                case String.toInt str of
                    Just n ->
                        succeed n

                    Nothing ->
                        problem "Not an integer"
            )


areaCode : Parser zeroOrMore (Maybe Int)
areaCode =
    let
        justValidAreaCode =
            succeed String.toInt
                |> skip (symbol "(")
                |> skip whitespace
                |> keep (getChompedString <| chompWhile Char.isDigit)
                |> skip whitespace
                |> skip (symbol ")")
                |> skip whitespace

        nothing =
            succeed Nothing
    in
    justValidAreaCode
        |> or nothing


localNumberString : Parser zeroOrMore String
localNumberString =
    let
        chompDigit state =
            chompIf Char.isDigit
                |> getChompedString
                |> map (\str -> str :: state)
                |> continue

        chompSpace state =
            chompIf (\c -> c == ' ')
                |> map (\_ -> state)
                |> continue

        reverseAndConcat state =
            state
                |> List.reverse
                |> String.concat
                |> succeed
                |> done
    in
    loop
        []
        (\state ->
            oneOf
                [ chompDigit state
                , chompSpace state
                ]
                |> or (reverseAndConcat state)
        )


localNumber : Parser zeroOrMore Int
localNumber =
    let
        checkDigits s =
            if String.length s == 7 then
                succeed s

            else
                problem "A NZ phone number has 7 digits"
    in
    localNumberString
        |> andThen checkDigits
        |> map String.toInt
        |> andThen
            (\maybe ->
                case maybe of
                    Just n ->
                        succeed n

                    Nothing ->
                        problem "Invalid local number"
            )


phoneParser : Parser zeroOrMore Phone
phoneParser =
    succeed
        Phone
        |> skip whitespace
        |> keep countryCode
        |> keep areaCode
        |> keep localNumber


type alias Model =
    { input : String
    , phone : Result (List Parser.DeadEnd) Phone
    }


initialModel : Model
initialModel =
    { input = ""
    , phone = Err []
    }


type Msg
    = Go
    | Input String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Go ->
            { model | phone = run phoneParser model.input }

        Input s ->
            { model | input = s }


view : Model -> Html Msg
view model =
    let
        phoneOutput =
            case model.phone of
                Err [] ->
                    "No input parsed"

                Err _ ->
                    "Errors: " ++ Debug.toString model.phone

                Ok phone ->
                    Debug.toString phone
    in
    div []
        [ input [ placeholder "Phone number", onInput Input, value model.input ] []
        , button [ onClick Go ] [ text "Parse" ]
        , div [ class "result" ] [ text phoneOutput ]
        , h2 [] [ text "Examples:" ]
        , div [ class "sample" ] [ text "\"  +65(04)1234567\"" ]
        , text <| Debug.toString <| run phoneParser "  +64(04)1234567"
        , div [ class "sample" ] [ text "\"(04)1234567\"" ]
        , text <| Debug.toString <| run phoneParser "(04)1234567"
        , div [ class "sample" ] [ text "\"  123 45 67   \"" ]
        , text <| Debug.toString <| run phoneParser "  123 45 67   "
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
