module Main exposing (..)

import Browser exposing (UrlRequest)
import Browser.Dom exposing (getViewport)
import Browser.Events
import GraphicSVG exposing (..)
import GraphicSVG.App exposing (AppWithTick, GetKeyState, appWithTick)
import Task
import Url exposing (Url)



-- This example must be viewed by elm reactor or another web server, not directly after using elm-make


type alias Model =
    { time : Float
    , w : Int
    , h : Int
    , scale : Float
    , score : Int
    , x : Float
    , y : Float
    , v : Float
    , mode : Mode
    , pushing : Bool
    , showSpeed : Bool
    }


type Msg
    = Tick Float GetKeyState
    | OnUrlChange Url
    | OnUrlRequest UrlRequest
    | OnResize Int Int
    | PushTapped
    | ToggleShowSpeed
    | ResetStone


main : AppWithTick () Model Msg
main =
    appWithTick Tick
        { init =
            \_ url key ->
                ( init, Task.perform (\vp -> OnResize (round vp.viewport.width) (round vp.viewport.height)) getViewport )

        -- get initial screen width
        , update = update
        , view = \model -> { body = view model, title = title model }
        , subscriptions = \_ -> Browser.Events.onResize OnResize -- subscribe to any other changes in screen size
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


init : Model
init =
    { time = 0
    , w = 0
    , h = 0
    , scale = 1.0
    , score = 0
    , x = 0
    , y = stoneDistance
    , v = 0
    , mode = Push
    , pushing = False
    , showSpeed = True
    }


type Mode
    = Push
    | Wait
    | Win
    | Lose


force =
    5


mass =
    10


friction =
    1


stoneDistance =
    -200


stoneRadius =
    20


hogglinje =
    0


arenaHeight =
    800


arenaWidth =
    420


targetDistance =
    300


targetRadius =
    30


tooFar =
    targetDistance + stoneRadius + targetRadius


statusX =
    -200


statusY =
    hogglinje + 10


dark : Color
dark =
    rgba 10 10 10 1


buttonColor : Color
buttonColor =
    rgba 64 64 64 1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ( _, ( _, _ ), ( dx1, dy1 ) ) ->
            ( let
                dt =
                    t - model.time

                newMode =
                    if model.y < hogglinje then
                        Push

                    else if model.y > tooFar then
                        Lose

                    else if model.v > 0 then
                        Wait

                    else if model.y < tooFar && model.y > targetDistance - stoneRadius - targetRadius then
                        Win

                    else
                        Lose
              in
              { model
                | time = t
                , y =
                    clamp (toFloat model.h * -0.5 + stoneRadius)
                        (toFloat model.h * 0.5 - stoneRadius)
                        (model.y + (model.v * dt))
                , v =
                    clamp 0
                        100
                        (model.v
                            + dt
                            * clamp -1
                                50
                                ((if model.y < hogglinje then
                                    if model.pushing then
                                        1

                                    else
                                        0

                                  else
                                    0
                                 )
                                    * force
                                    - friction
                                )
                        )
                , mode = newMode
                , score =
                    if newMode == Win && not (model.mode == Win) then
                        model.score + 1

                    else if newMode == Lose then
                        0

                    else
                        model.score
              }
            , Cmd.none
            )

        OnUrlChange _ ->
            ( model, Cmd.none )

        OnUrlRequest _ ->
            ( model, Cmd.none )

        OnResize w h ->
            ( { model | w = w, h = h, scale = min (toFloat h / arenaHeight) (toFloat w / arenaWidth) }, Cmd.none )

        PushTapped ->
            ( { model | pushing = not model.pushing }, Cmd.none )

        ToggleShowSpeed ->
            ( { model | showSpeed = not model.showSpeed }, Cmd.none )

        ResetStone ->
            ( { model | x = 0, y = stoneDistance, v = 0, mode = Push, pushing = False }, Cmd.none )



-- store new screen size in the model


title : Model -> String
title model =
    "Curling"


view : Model -> Collage Msg
view model =
    collage (toFloat model.w)
        (toFloat model.h)
        [ rect (toFloat model.w) (toFloat model.h)
            |> filled dark
        , line ( toFloat model.w * -0.5, hogglinje ) ( toFloat model.w * 0.5, hogglinje ) |> outlined (solid 2) white
        , group
            [ if model.showSpeed then
                group
                    [ text ("Speed: " ++ String.fromInt (truncate (model.v * 100))) |> alignLeft |> filled white |> move ( 100, 12 )
                    , roundedRect 100 50 4
                        |> filled buttonColor
                        |> move ( 150, -35 )
                        |> notifyTap ToggleShowSpeed
                    , text "Hide Speed"
                        |> size 14
                        |> centered
                        |> filled white
                        |> move ( 150, -34 )
                        |> notifyTap ToggleShowSpeed
                    , text "(hard mode)"
                        |> size 12
                        |> centered
                        |> filled white
                        |> move ( 150, -47 )
                        |> notifyTap ToggleShowSpeed
                    ]

              else
                group
                    [ roundedRect 100 50 4
                        |> filled buttonColor
                        |> move ( 150, -35 )
                        |> notifyTap ToggleShowSpeed
                    , text "Show Speed"
                        |> size 14
                        |> centered
                        |> filled white
                        |> move ( 150, -34 )
                        |> notifyTap ToggleShowSpeed
                    , text "(easy mode)"
                        |> size 12
                        |> centered
                        |> filled white
                        |> move ( 150, -47 )
                        |> notifyTap ToggleShowSpeed
                    ]
            , text (String.fromInt model.score) |> size 36 |> bold |> centered |> filled white |> move ( 150, 280 )
            , text "SCORE" |> centered |> filled white |> move ( 150, 312 )
            , case model.mode of
                Push ->
                    group
                        [ text "Push the stone" |> size 16 |> bold |> alignLeft |> filled white |> move ( statusX, statusY )
                        , roundedRect 100 50 4
                            |> filled
                                (if model.pushing then
                                    green

                                 else
                                    buttonColor
                                )
                            |> move ( -150, -35 )
                            |> notifyTap PushTapped
                        , text
                            (if model.pushing then
                                "Pushing"

                             else
                                "Push"
                            )
                            |> size 15
                            |> centered
                            |> filled white
                            |> move ( -150, -40 )
                            |> notifyTap PushTapped
                        ]

                Wait ->
                    text "Hope and pray..." |> size 16 |> bold |> alignLeft |> filled blue |> move ( statusX, statusY )

                Win ->
                    group
                        [ text "You Win!" |> size 28 |> bold |> alignLeft |> filled green |> move ( statusX, statusY )
                        , roundedRect 100 50 4
                            |> filled buttonColor
                            |> move ( -150, -35 )
                            |> notifyTap ResetStone
                        , text "Play Again"
                            |> size 15
                            |> centered
                            |> filled white
                            |> move ( -150, -40 )
                            |> notifyTap ResetStone
                        ]

                Lose ->
                    group
                        [ text "You lose..." |> size 18 |> bold |> alignLeft |> filled red |> move ( statusX, statusY )
                        , roundedRect 100 50 4
                            |> filled buttonColor
                            |> move ( -150, -35 )
                            |> notifyTap ResetStone
                        , text "Play Again"
                            |> size 15
                            |> centered
                            |> filled white
                            |> move ( -150, -40 )
                            |> notifyTap ResetStone
                        ]
            , circle targetRadius
                |> filled red
                |> move ( 0, targetDistance )
            , circle stoneRadius
                |> filled gray
                |> move ( model.x, model.y )
            ]
            |> scale model.scale
        ]
