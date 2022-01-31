module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline as Pipeline exposing (required)



---- MODEL ----


type Model
    = Start
    | Failure String
    | Loading
    | Success Person

type alias Person =
    { name : String
    , age : Int
    , hobbies : List String
    }

{--
    {
        "name": "John Smith",
        "age": 25,
        "hobbies": [
            "running",
            "coding",
            "camping"
    ]
    }

--}



init : () -> ( Model, Cmd Msg )
init _ =
    ( Start, getPerson)



---- UPDATE ----

api : String
api =
    "https://coderbyte.com/api/challenges/json/rest-get-simple"

getPerson : Cmd Msg
getPerson =
    Http.get
        { url = api
        , expect = Http.expectJson NewPerson personDecoder
        }

personDecoder : Decoder Person
personDecoder =
    Decode.succeed Person
        |> required "name" Decode.string
        |> required "age" Decode.int
        |> required "hobbies" (Decode.list Decode.string)
type Msg
    = GetPerson
    | NewPerson (Result Http.Error Person)

buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetPerson ->
            ( model, Cmd.none )

        NewPerson (Ok newPerson) ->
            ( Success newPerson, Cmd.none )

        NewPerson (Err httpError) ->
            ( Failure (buildErrorMessage httpError), Cmd.none )

---- VIEW ----

viewHeader : Model -> String
viewHeader model =
    case model of
        Start -> "Start"
        Failure error_message -> "Failure: " ++ error_message
        Loading -> "Loading"
        Success person -> "Success"

view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (viewHeader model) ]
        ]


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
