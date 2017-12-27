module Main exposing (..)

import Html exposing (Html, Attribute, div, input, text, h1, h2, button, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Gravatar exposing (defaultOptions, img)
import Time exposing (second, Time, inSeconds)
import Maybe
import Random.List
import Random
import Task
import Audio


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Mobber =
    { name : String
    , email : String
    }


type alias Model =
    { name : String
    , email : String
    , mobbers : List Mobber
    , mobbersLeft : List Mobber
    , time : Time
    , remaining : Int
    , running : Bool
    , current : Maybe Mobber
    , state : State
    }


roundLength : number
roundLength =
    5


gravatarOptions : Gravatar.Options
gravatarOptions =
    { size = Just 400, default = Gravatar.MonsterID, rating = Gravatar.RatedX, forceDefault = False }


defaultUser : Mobber
defaultUser =
    Mobber "Ingen" "ingen@ingen.com"


type State
    = Enter
    | Running
    | Selecting


init : ( Model, Cmd Msg )
init =
    { name = "", email = "", mobbers = [], mobbersLeft = [], time = 0, remaining = roundLength, running = False, current = Nothing, state = Enter } => Cmd.none


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        Time.every second Tick
    else
        Sub.none



-- UPDATE


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


type Msg
    = ChangeName String
    | ChangeEmail String
    | AddMobber
    | Start
    | Tick Time
    | Pause
    | Reset
    | Select ( Maybe Mobber, List Mobber )
    | Next
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeName newName ->
            { model | name = newName } => Cmd.none

        ChangeEmail newEmail ->
            { model | email = newEmail } => Cmd.none

        AddMobber ->
            let
                m =
                    Mobber model.name model.email
            in
                { model | mobbers = (m :: model.mobbers), name = "", email = "" } => Cmd.none

        Tick time ->
            let
                ru =
                    if model.remaining <= 0 then
                        False
                    else
                        True

                re =
                    if model.remaining <= 0 then
                        roundLength
                    else
                        model.remaining - 1

                st =
                    if model.remaining <= 0 then
                        Selecting
                    else
                        Running

                cu =
                    if model.remaining <= 0 then
                        Random.generate Select (Random.List.choose model.mobbersLeft)
                    else
                        Cmd.none
            in
                { model | remaining = re, running = ru, state = st } => cu

        Start ->
            { model | running = True, state = Running } => Cmd.none

        Pause ->
            { model | running = False } => Cmd.none

        Reset ->
            { model | remaining = roundLength } => Cmd.none

        Select ( mob, mobbersLeft ) ->
            let
                ml =
                    if List.isEmpty mobbersLeft then
                        model.mobbers
                    else
                        mobbersLeft
            in
                { model | current = mob, state = Selecting, mobbersLeft = ml } => Task.attempt doNothing (loadAndPlaySound "dist/Alarm.mp3")

        Next ->
            model => Random.generate Select (Random.List.choose model.mobbers)

        Noop ->
            model => Cmd.none


doNothing : Result error value -> Msg
doNothing result =
    case result of
        Ok _ ->
            Noop

        Err _ ->
            Noop



-- VIEW


view : Model -> Html Msg
view model =
    case model.state of
        Enter ->
            div [ class "page" ]
                [ h1 [] [ text "Mobbing" ]
                , inputform model
                , deltagarlista model.mobbers
                , largeButton Next "Nästa"
                ]

        Running ->
            div [ class "page2" ]
                [ div [ class "next" ] [ text "Mobba!" ]
                , litenDeltagare 1 (Maybe.withDefault defaultUser model.current)
                , div [ class "counter" ] [ text (model.remaining |> toMinutesAndSeconds) ]
                , smallButton Start model.running "Starta!"
                , smallButton Pause (not model.running) "Pausa"
                , smallButton Reset True "Återställ"
                ]

        Selecting ->
            div [ class "page3" ]
                [ div [ class "next" ] [ text "Nästa mobbare" ]
                , storDeltagare (Maybe.withDefault defaultUser model.current)
                , largeButton Start "Starta!"
                ]


smallButton : Msg -> Bool -> String -> Html Msg
smallButton msg d s =
    span [ class "small" ] [ Html.a [ href "#", class "button", onClick msg, disabled d ] [ span [] [ text s ] ] ]


largeButton : Msg -> String -> Html Msg
largeButton msg s =
    div [] [ Html.a [ href "#", class "button", onClick msg ] [ span [] [ text s ] ] ]


storDeltagare : Mobber -> Html Msg
storDeltagare mobber =
    div [ class "largeimg" ]
        [ img gravatarOptions mobber.email
        , div [ class "nametag" ] [ text mobber.name ]
        ]


litenDeltagare : Int -> Mobber -> Html Msg
litenDeltagare i mobber =
    div []
        [ case i % 2 == 0 of
            True ->
                div [ class "textbox", class "l" ]
                    [ div [] [ text mobber.name ]
                    , img gravatarOptions mobber.email
                    ]

            False ->
                div [ class "textbox", class "r" ]
                    [ img gravatarOptions mobber.email
                    , div [] [ text mobber.name ]
                    ]
        ]


deltagarlista : List Mobber -> Html Msg
deltagarlista mobbers =
    div [ class "deltagarlista" ] (List.indexedMap litenDeltagare mobbers)


inputform : Model -> Html Msg
inputform model =
    div [ class "newuser" ]
        [ input [ placeholder "Namn", onInput ChangeName, value model.name ] []
        , input [ placeholder "E-post", onInput ChangeEmail, value model.email ] []
        , div [ class "small" ] [ Html.a [ href "#", class "button", onClick AddMobber ] [ span [] [ text "Lägg till" ] ] ]
        ]



-- Util


toMinutesAndSeconds : Int -> String
toMinutesAndSeconds t =
    let
        m =
            (t // 60) |> toString

        s =
            (rem t 60)

        ss =
            if s < 10 then
                "0" ++ (toString <| s)
            else
                s |> toString
    in
        m ++ ":" ++ ss


loadAndPlaySound : String -> Task.Task String ()
loadAndPlaySound soundUrl =
    Audio.loadSound soundUrl |> Task.andThen (Task.mapError (\_ -> "") << Audio.playSound Audio.defaultPlaybackOptions)
