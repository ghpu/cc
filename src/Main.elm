port module Main exposing (..)

import Audio exposing (Audio, AudioCmd, AudioData)
import Duration
import Html exposing (Html)
import Html.Events
import Html.Attributes
import Json.Decode
import Json.Encode
import List.Nonempty exposing (Nonempty(..))
import Task
import Time
import Browser.Dom
import Browser.Events
import Svg
import Svg.Attributes


type alias LoadedModel_ =
    { sound : Audio.Source
    , scene : ( Int, Int )
    , r : Int
    , p : Int
    , soundState : SoundState
    }


type SoundState
    = NotPlaying
    | Playing Time.Posix
    | FadingOut Time.Posix Time.Posix


type Model
    = LoadingModel
    | LoadedModel LoadedModel_
    | LoadFailedModel


type Msg
    = SoundLoaded (Result Audio.LoadError Audio.Source)
    | PressedPlay
    | PressedPlayAndGotTime Time.Posix
    | PressedStop
    | PressedStopAndGotTime Time.Posix
    | Frame Time.Posix
    | GotViewport Browser.Dom.Viewport
    | Resize Int Int


init : flags -> ( Model, Cmd Msg, AudioCmd Msg )
init _ =
    ( LoadingModel
    , Cmd.none
    , Audio.loadAudio
        SoundLoaded
        "/music.mp3"
    )


update : AudioData -> Msg -> Model -> ( Model, Cmd Msg, AudioCmd Msg )
update _ msg model =
    case ( msg, model ) of
        ( GotViewport vp, LoadedModel loadedModel ) ->
            ( LoadedModel { loadedModel | scene = ( round vp.scene.width, round vp.scene.height ) }, Cmd.none, Audio.cmdNone )

        ( Resize width height, LoadedModel loadedModel ) ->
            ( LoadedModel { loadedModel | scene = ( width, height ) }, Cmd.none, Audio.cmdNone )

        ( Frame time, LoadedModel loadedModel ) ->
            let
                playtime =
                    case loadedModel.soundState of
                        Playing startTime ->
                            round (Duration.inMilliseconds (Duration.from startTime time))

                        _ ->
                            0

                modtime =
                    modBy 10000 playtime

                dif =
                    if modtime < 5000 then
                        modtime // 50 * 2
                    else
                        (10000 - modtime) // 50 * 2
            in
                ( LoadedModel { loadedModel | r = dif, p = playtime }, Cmd.none, Audio.cmdNone )

        ( SoundLoaded result, LoadingModel ) ->
            case result of
                Ok sound ->
                    ( LoadedModel { sound = sound, soundState = NotPlaying, scene = ( 640, 480 ), r = 0, p = 0 }
                    , Task.perform GotViewport Browser.Dom.getViewport
                    , Audio.cmdNone
                    )

                Err _ ->
                    ( LoadFailedModel
                    , Cmd.none
                    , Audio.cmdNone
                    )

        ( PressedPlay, LoadedModel loadedModel ) ->
            ( LoadedModel loadedModel
            , Task.perform PressedPlayAndGotTime Time.now
            , Audio.cmdNone
            )

        ( PressedPlayAndGotTime time, LoadedModel loadedModel ) ->
            ( LoadedModel { loadedModel | soundState = Playing time }
            , Cmd.none
            , Audio.cmdNone
            )

        ( PressedStop, LoadedModel loadedModel ) ->
            ( LoadedModel loadedModel
            , Task.perform PressedStopAndGotTime Time.now
            , Audio.cmdNone
            )

        ( PressedStopAndGotTime stopTime, LoadedModel loadedModel ) ->
            case loadedModel.soundState of
                Playing startTime ->
                    ( LoadedModel { loadedModel | soundState = FadingOut startTime stopTime, r = 0, p = 0 }
                    , Cmd.none
                    , Audio.cmdNone
                    )

                _ ->
                    ( model, Cmd.none, Audio.cmdNone )

        _ ->
            ( model, Cmd.none, Audio.cmdNone )


view : AudioData -> Model -> Html Msg
view _ model =
    case model of
        LoadingModel ->
            Html.text "Loading..."

        LoadedModel loadedModel ->
            exercise_view loadedModel

        LoadFailedModel ->
            Html.text "Failed to load sound."


exercise_view loadedModel =
    let
        button =
            case loadedModel.soundState of
                Playing _ ->
                    Html.button [ Html.Events.onClick PressedStop ] [ Html.text "Stop" ]

                _ ->
                    Html.button [ Html.Events.onClick PressedPlay ] [ Html.text "Start" ]
    in
        Html.div [ Html.Attributes.style "width" (String.fromInt (Tuple.first loadedModel.scene)), Html.Attributes.style "height" (String.fromInt (Tuple.second loadedModel.scene)), Html.Attributes.class "background" ]
            [ button
            , progress loadedModel
            , circle loadedModel
            ]


progress loadedModel =
    let
        prog =
            loadedModel.p // 3000 * 4
    in
        Svg.svg
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "left" "0"
            , Html.Attributes.style "top" "32"
            , Svg.Attributes.width "400"
            , Svg.Attributes.height "32"
            , Svg.Attributes.viewBox "0 0 400 32"
            ]
            [ Svg.rect [ Svg.Attributes.x "0", Svg.Attributes.y "0", Svg.Attributes.width "400", Svg.Attributes.height "32", Svg.Attributes.fill "blue", Svg.Attributes.fillOpacity "20%" ] []
            , Svg.rect [ Svg.Attributes.x "0", Svg.Attributes.y "0", Svg.Attributes.width (String.fromInt prog), Svg.Attributes.height "64", Svg.Attributes.fill "blue", Svg.Attributes.fillOpacity "50%" ] []
            ]


circle loadedModel =
    let
        xx =
            (Tuple.first loadedModel.scene) // 2

        yy =
            (Tuple.second loadedModel.scene) // 2

        r =
            loadedModel.r

        x =
            xx - r

        y =
            yy - r
    in
        Svg.svg
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "left" "100"
            , Html.Attributes.style "top" "100"
            , Svg.Attributes.width "400"
            , Svg.Attributes.height "400"
            , Svg.Attributes.viewBox "-200 -200 400 400"
            ]
            [ Svg.circle [ Svg.Attributes.cx "0", Svg.Attributes.cy "0", Svg.Attributes.r "200", Svg.Attributes.fill "blue", Svg.Attributes.fillOpacity "20%" ] []
            , Svg.circle [ Svg.Attributes.cx "0", Svg.Attributes.cy "0", Svg.Attributes.r (String.fromInt r), Svg.Attributes.fill "blue", Svg.Attributes.fillOpacity "30%" ] []
            ]


audio : AudioData -> Model -> Audio
audio _ model =
    case model of
        LoadedModel loadedModel ->
            case loadedModel.soundState of
                NotPlaying ->
                    Audio.silence

                Playing time ->
                    let
                        default =
                            Audio.audioDefaultConfig
                    in
                        Audio.audioWithConfig { default | loop = Just { loopStart = Duration.seconds 0, loopEnd = Duration.seconds 300 } } loadedModel.sound time

                FadingOut startTime stopTime ->
                    Audio.audio loadedModel.sound startTime
                        |> Audio.scaleVolumeAt [ ( stopTime, 1 ), ( Duration.addTo stopTime (Duration.seconds 2), 0 ) ]

        _ ->
            Audio.silence


port audioPortToJS : Json.Encode.Value -> Cmd msg


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg


subscriptions _ _ =
    Sub.batch
        [ Browser.Events.onAnimationFrame Frame
        , Browser.Events.onResize Resize
        ]


main : Platform.Program () (Audio.Model Msg Model) (Audio.Msg Msg)
main =
    Audio.elementWithAudio
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , audio = audio
        , audioPort = { toJS = audioPortToJS, fromJS = audioPortFromJS }
        }
