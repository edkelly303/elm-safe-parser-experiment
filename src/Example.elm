module Example exposing (main)

import Browser
import Html exposing (Html, button, div, h2, input, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Parser
import SafeParser exposing (..)


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
        option1 =
            succeed Just
                |> skip0 whitespace
                |> skip0 (symbol "+")
                |> keep int
                |> skip0 whitespace
    in
    option1
        |> or (succeed Nothing)


symbol : String -> Parser alwaysChomps String
symbol str =
    let
        helper state =
            oneOf
                [ chompIf (\c -> Just c == (state |> String.uncons |> Maybe.map Tuple.first))
                    |> map (\() -> String.dropLeft 1 state)
                    |> continue
                , chompIf (\c -> c == ' ')
                    |> andThen1
                        (\() ->
                            if state == "" then
                                succeed str

                            else
                                problem "not a match"
                        )
                    |> done
                ]
    in
    loop str helper


int : Parser alwaysChomps Int
int =
    succeed ()
        |> skip (chompIf Char.isDigit)
        |> skip0 (chompWhile Char.isDigit)
        |> getChompedString
        |> andThen1
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
        option1 =
            succeed String.toInt
                |> skip (symbol "(")
                |> skip0 whitespace
                |> keep0 (getChompedString <| chompWhile Char.isDigit)
                |> skip0 whitespace
                |> skip (symbol ")")
                |> skip0 whitespace
    in
    option1
        |> or (succeed Nothing)


localNumberStr : Parser constraints String
localNumberStr =
    loop [] localHelp
        |> map String.concat



-- localHelp : List String -> Parser MightNotChomp (Step (List String) (List String))


localHelp nums =
    let
        checkNum numsSoFar num =
            if String.length num > 0 then
                Parser.Loop (num :: numsSoFar)

            else
                Parser.Done (List.reverse numsSoFar)
    in
    succeed (checkNum nums)
        |> keep0 (getChompedString <| chompWhile Char.isDigit)
        |> skip0 whitespace


localNumber : Parser MightNotChomp Int
localNumber =
    let
        checkDigits s =
            if String.length s == 7 then
                succeed s

            else
                problem "A NZ phone number has 7 digits"
    in
    localNumberStr
        |> andThen0 checkDigits
        |> map String.toInt
        |> andThen0
            (\maybe ->
                case maybe of
                    Just n ->
                        succeed n

                    Nothing ->
                        problem "Invalid local number"
            )


phoneParser : Parser MightNotChomp Phone
phoneParser =
    succeed Phone
        |> skip0 whitespace
        |> keep0 countryCode
        |> keep0 areaCode
        |> keep0 localNumber


parse : String -> Result (List Parser.DeadEnd) Phone
parse str =
    run phoneParser str


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

                Err deadEnds ->
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
