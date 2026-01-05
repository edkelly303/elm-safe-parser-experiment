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
        ( --AlwaysChomps
          --,
          MightNotChomp
        , Parser
          --, Step
          --, andThenChompsAfter
        , andThenChompsBefore
        , andThenMightNotChomp
          --, backtrackable
        , chompIf
        , chompWhile
        , getChompedString
        , keep
        , keep0
        , loop, cont, done
        , map
        , or
        , problem
        , run
        , skip
        , skip0
        , succeed
        , symbol
        )


type alias Phone =
    { countryCode : Maybe Int
    , areaCode : Maybe Int
    , phone : Int
    }


whitespace : Parser MightNotChomp ()
whitespace =
    chompWhile (\c -> c == ' ')


countryCode : Parser MightNotChomp (Maybe Int)
countryCode =
    let
        justValidCountryCode =
            succeed Just
                |> skip0 whitespace
                |> skip (symbol "+")
                |> keep int
                |> skip0 whitespace

        nothing =
            succeed Nothing
    in
    justValidCountryCode
        |> or nothing


int : Parser alwaysChomps Int
int =
    succeed (\first rest -> first ++ rest)
        |> keep (chompIf Char.isDigit |> getChompedString)
        |> keep0 (chompWhile Char.isDigit |> getChompedString)
        |> andThenChompsBefore
            (\str ->
                case String.toInt str of
                    Just n ->
                        succeed n

                    Nothing ->
                        problem "Not an integer"
            )


areaCode : Parser MightNotChomp (Maybe Int)
areaCode =
    let
        justValidAreaCode =
            succeed String.toInt
                |> skip (symbol "(")
                |> skip0 whitespace
                |> keep0 (getChompedString <| chompWhile Char.isDigit)
                |> skip0 whitespace
                |> skip (symbol ")")
                |> skip0 whitespace

        nothing =
            succeed Nothing
    in
    justValidAreaCode
        |> or nothing


localNumberString : Parser MightNotChomp String
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
        { initialState = []
        , firstCallback = 
            \state -> 
                chompDigit state 
        , restCallbacks = 
            \state -> 
                chompSpace state
                    |> or (reverseAndConcat state)
        }

localNumber : Parser MightNotChomp Int
localNumber =
    let
        checkDigits s =
            if String.length s == 7 then
                succeed s

            else
                problem "A NZ phone number has 7 digits"
    in
    localNumberString
        |> andThenMightNotChomp checkDigits
        |> map String.toInt
        |> andThenMightNotChomp
            (\maybe ->
                case maybe of
                    Just n ->
                        succeed n

                    Nothing ->
                        problem "Invalid local number"
            )


phoneParser : Parser MightNotChomp Phone
phoneParser =
    succeed
        Phone
        |> skip0 whitespace
        |> keep0 countryCode
        |> keep0 areaCode
        |> keep0 localNumber


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
