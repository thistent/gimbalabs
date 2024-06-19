module Main exposing (..)

-- import Pane

import Array exposing (Array)
import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events exposing (onResize)
import Browser.Navigation as Nav
import Calendar exposing (CalendarDate)
import Date exposing (Date)
import Delay exposing (Timer)
import Dict
import Docs exposing (..)
import Ease
import Html exposing (Html)
import Http
import Markup exposing (hBar, iconButton, renderMd, vBar)
import Pic
import Return exposing (Return)
import Style exposing (..)
import Task
import Time exposing (Month(..))
import Types exposing (..)
import Ui exposing (..)
import Ui.Background as Bg
import Ui.Border as Border
import Ui.Events as Ev
import Ui.Font as Font
import Url



-- Main --


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view =
            \model ->
                { title = "Gimbalabs"
                , body =
                    [ model.size
                        |> Delay.switch
                            (loadingPage model)
                            (view model)
                    ]
                }
        , subscriptions = subs
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


loadingPage : Model -> Timer -> Html Msg
loadingPage model t =
    let
        ease : Float
        ease =
            ((t.original - t.current) / t.original)
                |> Ease.outQuart

        bg : Color
        bg =
            Style.mix ease
                (rgb 0 0 0)
                model.pal.bg

        fg : Color
        fg =
            Style.mix ease
                (rgb 0 0 0)
                model.pal.link
    in
    layout
        [ fillSpace
        , Bg.color bg
        , Font.size <| round <| model.fontSize * 2
        , Font.family [ Font.serif ]
        ]
    <|
        el [ centerXY, moveUp <| ease * 60.0 ] <|
            column [ centerX, spacing <| round <| model.fontSize / 3 ]
                [ row
                    [ Font.color fg
                    , Font.letterSpacing 1.25
                    ]
                    [ el
                        [ Bg.color fg
                        , height <| px <| round <| model.fontSize * 3
                        , width <| px <| round <| model.fontSize * 3
                        , paddingXY 0 5
                        ]
                      <|
                        el [ centerXY, scale 1.4 ] <|
                            Pic.gimbalogo bg <|
                                model.fontSize
                                    * 1.6
                    , text " GIMBA"
                    , el [ Font.bold ] <| text "LABS"
                    ]
                , el
                    [ Font.color <| Style.mix 0.5 fg bg
                    , centerX
                    , Font.size <| round model.fontSize
                    , Font.letterSpacing 2.5
                    ]
                  <|
                    text "Loading..."
                ]



--Initial State --


init : Flags -> Url.Url -> Nav.Key -> Return Msg Model
init flags url key =
    let
        startDoc : String
        startDoc =
            -- "Types.md"
            "Main.md"
    in
    Return.return
        { navKey = key
        , url = url
        , page = Home
        , menu = MenuClosed
        , pal = newspaper -- dark
        , size = Delay.wait 500 Nothing
        , zone = Time.utc
        , time = Nothing
        , currentSlide = "start"
        , slides = Dict.empty
        , docText = ""
        , docName = ""
        , hemisphere = North
        , selectDate = Nothing
        , events =
            Array.fromList
                [ { firstDate = Date.fromCalendarDate 2024 Mar 11
                  , lastDate = Date.fromCalendarDate 2024 Dec (30 + 1)
                  , startTime = 13 * 60 + 0
                  , duration = 1 * 60 + 0
                  , exceptions =
                        []
                  , title = "Cardano Go Live Coding"
                  , description = "[Zoom Link](https://us06web.zoom.us/meeting/register/tZwtcemrqTwoG9fYL2pYvrCwQG9u2tJNmqa6)"
                  , color = rgb255 0xCC 0xAA 0x00 -- #ccaa00
                  }
                , { firstDate = Date.fromCalendarDate 2024 Mar 5
                  , lastDate = Date.fromCalendarDate 2024 Aug (27 + 1)
                  , startTime = 13 * 60 + 0
                  , duration = 1 * 60 + 0
                  , exceptions =
                        [ Date.fromCalendarDate 2024 Jun 4
                        ]
                  , title = "Mesh Live Coding"
                  , description = "[Zoom Link](https://us06web.zoom.us/meeting/register/tZEqcuGgpz8oG9d-DgQBDEnICud-mF4uyQCs)"
                  , color = rgb255 0xBB 0x77 0xDD -- #bb77dd
                  }
                , { firstDate = Date.fromCalendarDate 2024 Jan 9
                  , lastDate = Date.fromCalendarDate 2024 Jun (25 + 1)
                  , startTime = 18 * 60 + 0
                  , duration = 1 * 60 + 30
                  , exceptions =
                        [ Date.fromCalendarDate 2024 Jun 18
                        ]
                  , title = "Gimbalabs Playground"
                  , description = "[Zoom Link](https://us06web.zoom.us/meeting/register/tZYoduuqpjsqGtdzMHXoRVVnJqcQGOtpQRQv)"
                  , color = rgb255 0x00 0x99 0xCC -- #0099cc
                  }
                , { firstDate = Date.fromCalendarDate 2024 Feb 29
                  , lastDate = Date.fromCalendarDate 2024 Dec (26 + 1)
                  , startTime = 14 * 60 + 30
                  , duration = 1 * 60 + 30
                  , exceptions =
                        []
                  , title = "Gimbalabs Open Spaces"
                  , description = "[Zoom Link](https://us06web.zoom.us/meeting/register/tZErceCqpzMtG9XldfuPnBQEus5MBivl9OZe)"
                  , color = rgb255 0x99 0xCC 0x66 -- #99cc66
                  }
                , { firstDate = Date.fromCalendarDate 2024 May 22
                  , lastDate = Date.fromCalendarDate 2024 Dec (25 + 1)
                  , startTime = 14 * 60 + 0
                  , duration = 1 * 60 + 30
                  , exceptions =
                        []
                  , title = "Plutus PBL Live Coding"
                  , description = "[Zoom Link](https://us06web.zoom.us/meeting/register/tZwoduCgrTgiHt3u34gOSGRL5pY4pdZlT5MM)"
                  , color = rgb255 0xDD 0x77 0x99 -- #dd7799
                  }
                ]
        , fontSize = 20 -- 12 * flags.dpi
        , dpi = flags.dpi
        }
    <|
        Cmd.batch
            [ Task.perform SceneInfo Dom.getViewport
            , Task.perform AdjustTimeZone Time.here
            , Http.get
                { url = "Notes/" ++ startDoc
                , expect =
                    Http.expectString <| ReceiveDoc startDoc
                }
            ]



-- Update --


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        GotoPage page ->
            Return.singleton { model | page = page }

        ChangeMenu menu ->
            Return.singleton { model | menu = menu }

        ChangeColor scheme ->
            Return.singleton
                { model | pal = scheme }

        NextSlide ns ->
            Return.singleton
                { model | currentSlide = ns }

        WindowResize x y ->
            let
                newVp : Viewport
                newVp =
                    sizeToVp (toFloat x) (toFloat y)
            in
            Return.singleton
                { model
                    | size = Delay.update model.size newVp
                }

        SceneInfo viewport ->
            Return.singleton
                { model
                    | size = Delay.update model.size viewport
                }

        Tick time ->
            Return.singleton
                { model
                    | time = Just time
                }

        FrameDelta delta ->
            Return.singleton
                { model
                    | size = Delay.tick delta model.size
                }

        AdjustTimeZone zone ->
            Return.singleton { model | zone = zone }

        UrlChanged url ->
            Return.singleton { model | url = url }

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    Return.return model <|
                        Nav.pushUrl model.navKey <|
                            Url.toString url

                Browser.External url ->
                    Return.return model <|
                        Nav.load url

        GetDoc doc ->
            Return.return
                model
            <|
                Http.get
                    { url = "Notes/" ++ doc
                    , expect =
                        Http.expectString <| ReceiveDoc doc
                    }

        ReceiveDoc doc res ->
            -- ReceiveDoc str res ->
            case res of
                Ok p ->
                    Return.singleton
                        { model
                            | docText = p
                            , docName = doc
                        }

                Err _ ->
                    Return.singleton model

        ToggleClockOrientation ->
            case model.hemisphere of
                North ->
                    Return.singleton
                        { model | hemisphere = South }

                South ->
                    Return.singleton
                        { model | hemisphere = North }

        SelectDate maybeDate ->
            Return.singleton { model | selectDate = maybeDate }



-- Subscriptions --


subs : Model -> Sub Msg
subs _ =
    Sub.batch
        [ onResize WindowResize
        , Events.onAnimationFrameDelta FrameDelta
        , Time.every 20 Tick
        ]



-- View --


view : Model -> Viewport -> Html Msg
view model vp =
    let
        m : Pal -> Pal -> Model
        m oldPal newPal =
            { model
                | pal =
                    { newPal
                        | bg = Style.mix 0.6 oldPal.bg newPal.bg
                        , fg = Style.mix 0.85 oldPal.bg newPal.fg
                    }
            }
    in
    layout
        [ Font.size <| round model.fontSize
        , Font.family [ Font.serif ]
        , Font.color model.pal.fg
        , Font.letterSpacing 0.2
        , Bg.color model.pal.bg
        , inFront <|
            el
                [ padding <| round <| model.fontSize / 2
                , width fill
                , Bg.color <| addAlpha 0.9 model.pal.bg
                , Style.shadow
                ]
            <|
                titleBar model vp
        ]
    <|
        column
            [ fillSpace
            , Bg.color model.pal.bg
            , spacing <| round <| model.fontSize
            , padding <| round <| model.fontSize / 2
            ]
            [ el [ height <| px <| round <| model.fontSize * 3.5 ] none
            , case model.page of
                Home ->
                    column
                        [ fillSpace
                        , spacing <| round <| model.fontSize * 2
                        , padding <| round <| model.fontSize / 2
                        ]
                        [ el [ width <| fillPortion 3, height fill ] <|
                            topGroup model
                                [ heading model "Welcome to Gimbalabs!"
                                , item model <|
                                    "Right now, we are building Plutus PBL 2024, "
                                        ++ "running weekly live coding sessions, "
                                        ++ "and hosting Gimbalabs Open Spaces."
                                ]
                        , heading model "Today's Events"
                        , column
                            [ spacing <| round <| model.fontSize * 2
                            , paddingXY (round <| model.fontSize / 2) 0
                            , width fill
                            ]
                            (case model.time of
                                Nothing ->
                                    [ text "(Nothing Today)"
                                    ]

                                Just time ->
                                    model.events
                                        |> eventsOfDay (Date.fromPosix model.zone time)
                                        |> List.map (renderEvent model vp)
                            )
                        ]

                Calendar ->
                    calendarView model vp

                Blog ->
                    blogView model vp

                Solutions ->
                    el [ centerXY ] <| text "Solutions"

                Settings ->
                    settingsView model vp

                Graph ->
                    graphView model vp
            , hBar
            , textColumn
                [ fillSpace
                ]
                [ wrappedRow
                    [ fillSpace
                    , spacing <| round <| model.fontSize * 2
                    , padding <| round <| model.fontSize
                    ]
                    [ turningPage (m model.pal orangeNote) 0.02 <|
                        el [ fillSpace ] <|
                            column [ centerX, spacing <| round model.fontSize ]
                                [ heading model "Learn"
                                , el [] <| text "⯀ Starter Kits"
                                , el [] <| text "⯀ Plutus"
                                , el [] <| text "⯀ Playground"
                                ]
                    , turningPage (m model.pal yellowNote) 0 <|
                        el [ fillSpace ] <|
                            column [ centerX, spacing <| round model.fontSize ]
                                [ heading model "APIs"
                                , el [] <| text "⯀ Dandelion"
                                , el [] <| text "⯀ Endpoints"
                                ]
                    , turningPage (m model.pal greenNote) -0.02 <|
                        el [ fillSpace ] <|
                            column [ centerX, spacing <| round model.fontSize ]
                                [ heading model "Updates"
                                , el [] <| text "⯀ Updates"
                                ]
                    , turningPage (m model.pal blueNote) 0.01 <|
                        el [ fillSpace ] <|
                            column [ centerX, spacing <| round model.fontSize ]
                                [ heading model "About Us"
                                , el [] <| text "⯀ Team"
                                , el [] <| text "⯀ Calendar"
                                , el [] <| text "⯀ Stake Pool"
                                ]
                    ]
                ]
            ]


graphView : Model -> Viewport -> Element Msg
graphView model vp =
    el [ centerXY ] <| text "Graph View"


calendarView : Model -> Viewport -> Element Msg
calendarView model vp =
    let
        time :
            { localHours : Int
            , hours : Int
            , minutes : Int
            , seconds : Int
            , millis : Int
            }
        time =
            model.time
                |> Maybe.map
                    (\t ->
                        { localHours = Time.toHour model.zone t
                        , hours = Time.toHour Time.utc t
                        , minutes = Time.toMinute Time.utc t
                        , seconds = Time.toSecond Time.utc t
                        , millis = Time.toMillis Time.utc t
                        }
                    )
                |> Maybe.withDefault
                    { localHours = 0
                    , hours = 0
                    , minutes = 0
                    , seconds = 0
                    , millis = 0
                    }

        c :
            { hourRotation : Float
            , localHourRotation : Float
            , minuteRotation : Float
            , secondRotation : Float
            , leftLabel : String
            , rightLabel : String
            , picUrl : String
            , hourLines : String
            , op : String
            }
        c =
            case model.hemisphere of
                North ->
                    { hourRotation = 0 - (toFloat time.hours + toFloat time.minutes / 60) / 12 * pi
                    , localHourRotation = 0 - (toFloat time.localHours + toFloat time.minutes / 60) / 12 * pi
                    , minuteRotation = 0 - (toFloat time.minutes + toFloat time.seconds / 60) / 30 * pi
                    , secondRotation = 0 - (toFloat time.seconds + toFloat time.millis / 1000) / 30 * pi
                    , leftLabel = "06"
                    , rightLabel = "18"
                    , picUrl = "assets/earth-north.png"
                    , hourLines = "assets/earth-hours-north.png"
                    , op = "Southern"
                    }

                South ->
                    { hourRotation = (toFloat time.hours + toFloat time.minutes / 60) / 12 * pi
                    , localHourRotation = (toFloat time.localHours + toFloat time.minutes / 60) / 12 * pi
                    , minuteRotation = (toFloat time.minutes + toFloat time.seconds / 60) / 30 * pi
                    , secondRotation = (toFloat time.seconds + toFloat time.millis / 1000) / 30 * pi
                    , leftLabel = "18"
                    , rightLabel = "06"
                    , picUrl = "assets/earth-south.png"
                    , hourLines = "assets/earth-hours-south.png"
                    , op = "Northern"
                    }
    in
    column
        [ fillSpace
        , spacing <| round <| model.fontSize
        , paddingEach { edges | bottom = round <| model.fontSize }
        ]
        [ el
            [ fillSpace
            , inFront <|
                dayView model vp
            ]
          <|
            calendar model
        , hBar
        , el [ centerX ] <|
            image
                [ centerXY
                , clip
                , inFront <|
                    image
                        [ rotate c.localHourRotation
                        ]
                        { src = "assets/earth-local-hour.png"
                        , description = "local hour hand"
                        }
                , behindContent <|
                    image
                        [ rotate c.hourRotation
                        , inFront <|
                            image
                                [ rotate c.secondRotation
                                ]
                                { src = "assets/earth-second.png"
                                , description = "second hand"
                                }
                        , inFront <|
                            image
                                [ rotate c.minuteRotation
                                ]
                                { src = "assets/earth-minute.png"
                                , description = "minute hand"
                                }
                        ]
                        { src = c.picUrl
                        , description = "earth clock"
                        }
                ]
                { src = c.hourLines, description = "earth" }
        , el [ centerX ] <|
            iconButton model ToggleClockOrientation Nothing <|
                text ("Flip Clock to " ++ c.op ++ " Hemisphere")
        , el
            [ centerX
            , Font.bold
            ]
          <|
            text "Current Time:"
        , el
            [ centerX
            ]
          <|
            text <|
                String.fromInt time.hours
                    ++ ":"
                    ++ (String.padLeft 2 '0' <| String.fromInt time.minutes)
                    ++ " UTC"
        , el
            [ centerX
            ]
          <|
            text <|
                String.fromInt time.localHours
                    ++ ":"
                    ++ (String.padLeft 2 '0' <| String.fromInt time.minutes)
                    ++ " Local"
        ]


blogView : Model -> Viewport -> Element Msg
blogView model vp =
    column
        [ fillSpace
        , spacing <| round <| model.fontSize
        , paddingEach
            { edges
                | right = round model.fontSize
                , left = round model.fontSize
                , bottom = round model.fontSize
            }
        ]
        [ row
            [ width fill
            , paddingXY lineSize 0
            , spacing <| round model.fontSize
            ]
            [ el [ Font.bold ] <| text "File:"
            , text <| "/Notes/" ++ model.docName
            , el [ alignRight ] <| iconButton model (GetDoc "Main.md") Nothing <| text "Main.md"
            ]
        , hBar
        , renderMd model vp model.docText
        ]


settingsView : Model -> Viewport -> Element Msg
settingsView model vp =
    column [ centerXY, spacing <| round model.fontSize ]
        [ el [ centerX, Font.bold, Font.size <| round <| model.fontSize * 2 ] <| text "Under Construction!"
        , el [ centerXY ] <| text "( Settings )"
        , vp.viewport
            |> (\{ height, width } ->
                    { height = height / model.dpi |> round
                    , width = width / model.dpi |> round
                    }
               )
            |> classifyDevice
            |> Debug.toString
            |> text
            |> el [ centerX ]
        , vp.viewport
            |> (\{ height, width } ->
                    "{ width = "
                        ++ String.fromFloat (width / model.dpi)
                        ++ ", height = "
                        ++ String.fromFloat (height / model.dpi)
                        ++ " }"
               )
            |> text
            |> el [ centerX ]
        , model.fontSize
            |> String.fromFloat
            |> text
            |> el [ centerX ]
        , model.dpi
            |> String.fromFloat
            |> text
            |> el [ centerX ]
        ]


dayView : Model -> Viewport -> Element Msg
dayView model vp =
    case model.selectDate of
        Nothing ->
            none

        Just date ->
            column
                [ height fill
                , width <| minimum 500 fill
                , padding <| round model.fontSize
                , spacing <| round model.fontSize * 2
                , Border.width lineSize
                , Border.roundEach
                    { corners
                        | topLeft = round <| model.fontSize / 2
                        , topRight = round <| model.fontSize / 2
                    }
                , Bg.color <| Style.addAlpha 0.9 model.pal.bg
                , scrollbarY
                ]
                [ column
                    [ spacing <| round <| model.fontSize * 0.65
                    , width fill
                    , inFront <|
                        el
                            [ alignTop
                            , alignRight
                            ]
                        <|
                            iconButton model
                                (SelectDate Nothing)
                                Nothing
                            <|
                                text "Close"
                    ]
                    [ el
                        [ centerX
                        , Font.size <| round <| model.fontSize * 1.5
                        , Font.bold
                        ]
                      <|
                        text <|
                            Date.format "EEEE" date
                    , el
                        [ centerX
                        , Font.size <| round <| model.fontSize * 1.5
                        , Font.bold
                        ]
                      <|
                        text <|
                            Date.format "MMMM ddd, y" date
                    , hBar
                    ]
                , column
                    [ spacing <| round <| model.fontSize * 2
                    , paddingXY (round <| model.fontSize / 2) 0
                    , width fill
                    ]
                    (model.events
                        |> eventsOfDay date
                        |> List.map (renderEvent model vp)
                    )
                ]


renderEvent : Model -> Viewport -> WeeklyEvent -> Element Msg
renderEvent model vp event =
    textColumn
        [ width fill
        , spacing <| round model.fontSize
        , paddingEach { edges | left = round model.fontSize }
        , Border.widthEach { edges | left = lineSize * 2 }
        , Border.roundEach
            { corners
                | topLeft = round <| model.fontSize / 2
                , bottomLeft = round <| model.fontSize / 2
            }
        ]
        [ paragraph [ width fill, Font.size <| round <| model.fontSize * 1.75, Font.bold ] [ text event.title ]
        , paragraph [ width fill, Font.bold ] [ text <| eventTimeString event.startTime event.duration ]
        , renderMd model vp event.description
        ]


calendar : Model -> Element Msg
calendar model =
    column
        [ fillSpace
        , Border.width lineSize
        , Border.roundEach
            { corners
                | topLeft = round <| model.fontSize / 2
                , topRight = round <| model.fontSize / 2
            }
        , Style.shadow
        ]
        [ column
            [ width fill
            , Border.widthEach { edges | bottom = lineSize }
            , Border.roundEach
                { corners
                    | topLeft = round <| model.fontSize / 2
                    , topRight = round <| model.fontSize / 2
                }
            , Bg.color <| Style.mix 0.075 model.pal.bg model.pal.fg
            ]
            [ el
                [ centerX
                , Font.size <| round <| model.fontSize * 1.5
                , Font.bold
                , paddingXY 0 <| round model.fontSize
                ]
              <|
                text <|
                    (model.time
                        |> Maybe.map (Date.fromPosix model.zone)
                        |> Maybe.map (Date.format "MMMM y")
                        |> Maybe.withDefault "Calendar"
                    )
            , row
                [ width fill
                , paddingXY 0 <| round <| model.fontSize * 0.4
                ]
                ([ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" ]
                    |> List.map (text >> el [ centerX ] >> el [ width fill, Font.bold ])
                )
            ]
        , el
            [ fillSpace
            ]
          <|
            (model.time
                |> Maybe.map (Calendar.fromTime Nothing Time.utc)
                |> Maybe.map (List.map (weekOnCalendar model) >> column [ fillSpace ])
                |> Maybe.withDefault none
            )
        ]


weekOnCalendar : Model -> List CalendarDate -> Element Msg
weekOnCalendar model =
    List.map (dayOnCalendar model) >> row [ fillSpace ]


dayOnCalendar : Model -> CalendarDate -> Element Msg
dayOnCalendar model day =
    let
        date :
            { year : Int
            , month : Time.Month
            , day : Int
            , posix : Time.Posix
            }
        date =
            model.time
                |> Maybe.map
                    (\t ->
                        { year = Time.toYear Time.utc t
                        , month = Time.toMonth Time.utc t
                        , day = Time.toDay Time.utc t
                        , posix = t
                        }
                    )
                |> Maybe.withDefault
                    { year = 0
                    , month = Time.Jan
                    , day = 0
                    , posix = Time.millisToPosix 0
                    }

        isToday : Bool
        isToday =
            Date.compare day.date (Date.fromPosix Time.utc date.posix) == EQ

        isSelected : Bool
        isSelected =
            case model.selectDate of
                Just sd ->
                    Date.compare day.date sd == EQ

                Nothing ->
                    False

        isThisMonth : Bool
        isThisMonth =
            Date.month day.date == date.month

        bg : Color
        bg =
            case ( isSelected, isThisMonth ) of
                ( True, True ) ->
                    Style.mix 0.5 model.pal.link model.pal.bg

                ( True, False ) ->
                    Style.mix 0.5 model.pal.link <| Style.mix 0.35 model.pal.bg model.pal.fg

                ( False, True ) ->
                    model.pal.bg

                ( False, False ) ->
                    Style.mix 0.35 model.pal.bg model.pal.fg

        internalBorder : Color
        internalBorder =
            if isToday then
                model.pal.fg

            else
                bg
    in
    el
        [ fillSpace
        , padding <| round <| model.fontSize / 6
        , Border.width 1
        , Bg.color internalBorder
        ]
    <|
        link
            [ fillSpace
            , Ev.onClick <|
                SelectDate <|
                    if isSelected then
                        Nothing

                    else
                        Just day.date
            ]
            { url = ""
            , label =
                column
                    [ fillSpace

                    --, padding <| round <| model.fontSize / 2 - model.fontSize / 6
                    , spacing <| round <| model.fontSize / 2 - model.fontSize / 6
                    , Font.size 10
                    , Bg.color bg
                    ]
                <|
                    el
                        [ Font.size 16
                        , Font.bold
                        , Font.color <| model.pal.bg
                        , Bg.color <| Style.mix 0.75 model.pal.bg model.pal.fg
                        , padding <| lineSize * 2
                        , Border.roundEach
                            { corners
                                | topRight = lineSize * 4
                                , bottomLeft = lineSize * 4
                                , bottomRight = lineSize * 4
                            }
                        ]
                        (day.date
                            |> Date.day
                            |> String.fromInt
                            |> String.padLeft 2 '0'
                            |> text
                        )
                        :: (model.events
                                |> eventsOfDay day.date
                                |> List.map (calendarItem model)
                           )

            {-
               paragraph
                   [ width fill
                   , Bg.color <| Style.addAlpha 0.7 <| Style.mix 0.25 model.pal.bg model.pal.error
                   , Border.color <| Style.mix 0.5 model.pal.bg model.pal.error
                   , Border.width 1
                   , Border.rounded <| round <| model.fontSize / 3
                   , padding <| round <| model.fontSize / 2 - model.fontSize / 6
                   ]
                   [ text "Open Spaces"
                   ]
               , paragraph
                   [ width fill
                   , Bg.color <| Style.addAlpha 0.7 <| Style.mix 0.25 model.pal.bg model.pal.link
                   , Border.color <| Style.mix 0.5 model.pal.bg model.pal.link
                   , Border.width 1
                   , Border.rounded <| round <| model.fontSize / 3
                   , padding <| round <| model.fontSize / 2 - model.fontSize / 6
                   ]
                   [ text "Live Coding"
                   ]
               , paragraph
                   [ width fill
                   , Bg.color <| Style.addAlpha 0.7 <| Style.mix 0.25 model.pal.bg model.pal.extLink
                   , Border.color <| Style.mix 0.5 model.pal.bg model.pal.extLink
                   , Border.width 1
                   , Border.rounded <| round <| model.fontSize / 3
                   , padding <| round <| model.fontSize / 2 - model.fontSize / 6
                   ]
                   [ text "Playground"
                   ]
            -}
            }


calendarItem : Model -> WeeklyEvent -> Element Msg
calendarItem model event =
    paragraph
        [ width fill
        , Bg.color <| Style.addAlpha 0.7 <| Style.mix 0.25 model.pal.bg event.color
        , Border.color <| Style.mix 0.5 model.pal.bg event.color
        , Border.width 1
        , Border.rounded <| round <| model.fontSize / 3
        , padding <| round <| model.fontSize / 2 - model.fontSize / 6
        ]
        [ text event.title
        ]


turningPage : Model -> Float -> Element Msg -> Element Msg
turningPage model rot content =
    row
        [ Font.color model.pal.fg
        , Bg.color model.pal.bg
        , Border.roundEach { corners | bottomRight = round <| model.fontSize * 2 }
        , shadow
        , fillSpace
        , rotate rot
        ]
        [ el
            [ fillSpace
            , Bg.color model.pal.bg
            , paddingEach
                { edges
                    | left = round <| model.fontSize * 2
                    , top = round <| model.fontSize * 2
                    , bottom = round <| model.fontSize * 3
                }
            , otherSide <|
                el
                    [ fillSpace
                    , paddingEach
                        { edges
                            | left = round <| model.fontSize * 2
                            , top = round <| model.fontSize * 2
                            , bottom = round <| model.fontSize * 3
                        }
                    , alpha 0.03
                    , style "pointer-events" "none"
                    ]
                <|
                    content
            ]
          <|
            content
        , column
            [ height fill
            , width <| px <| round <| model.fontSize * 2
            ]
            [ el
                [ fillSpace
                , Bg.color model.pal.bg
                ]
                none
            , el
                [ width <| px <| round <| model.fontSize * 2
                , height <| px <| round <| model.fontSize * 2

                --, Bg.color model.pal.bg
                , Border.roundEach { corners | bottomRight = round <| model.fontSize * 2 }
                ]
              <|
                Pic.pageCurl model.pal <|
                    model.fontSize
                        * 2
            ]
        ]


titleBar : Model -> Viewport -> Element Msg
titleBar model vp =
    row
        [ width fill
        , spacing <| round <| model.fontSize / 2
        , case model.menu of
            MenuClosed ->
                batch []

            MenuOpen ->
                below <|
                    column
                        [ alignTop
                        , alignRight
                        , width <| px <| round <| vp.viewport.width * 0.9
                        , height <| px <| round <| vp.viewport.height * 0.9
                        ]
                        [ row [ fillSpace ]
                            [ el
                                [ fillSpace
                                , Ev.onClick <| ChangeMenu MenuClosed
                                ]
                                none
                            , el
                                [ alignTop
                                , alignRight
                                , Bg.color <| addAlpha 0.95 <| model.pal.bg
                                , Font.size <| round model.fontSize
                                , padding <| round <| model.fontSize
                                , Style.shadow
                                ]
                              <|
                                mainMenu model
                            ]
                        , el
                            [ fillSpace
                            , Ev.onClick <| ChangeMenu MenuClosed
                            ]
                            none
                        ]
        ]
        [ link
            [ Ev.onClick <| GotoPage Home ]
            { url = ""
            , label =
                el
                    [ Bg.color model.pal.link
                    , height <| px <| round <| model.fontSize * 3
                    , width <| px <| round <| model.fontSize * 3
                    , paddingXY 0 5
                    ]
                <|
                    el [ centerXY, scale 1.4 ] <|
                        Pic.gimbalogo model.pal.bg <|
                            model.fontSize
                                * 1.6
            }
        , el [ fillSpace, paddingXY 0 lineSize ] <|
            row
                [ fillSpace
                , Border.widthEach { edges | top = lineSize, bottom = lineSize }
                , spacing <| round <| model.fontSize
                , paddingXY 0 <| round <| model.fontSize / 2
                ]
                [ row
                    [ spacing <| round <| model.fontSize
                    , width fill
                    , centerY
                    ]
                  <|
                    [ link [ Ev.onClick <| GotoPage Home ]
                        { url = ""
                        , label =
                            row
                                [ Font.size <| round <| model.fontSize * 1.5
                                , Font.color model.pal.link
                                , Font.letterSpacing 1.25
                                ]
                                [ text "GIMBA"
                                , el [ Font.heavy, Font.bold ] <| text "LABS"
                                ]
                        }
                    , el [ alignRight ] <|
                        text <|
                            viewTimeDate model.zone <|
                                model.time
                    , case model.menu of
                        MenuClosed ->
                            iconButton model
                                (ChangeMenu MenuOpen)
                                (Just Pic.menuClosed)
                                none

                        {- Pic.menuClosed model.pal.link
                           model.fontSize
                        -}
                        MenuOpen ->
                            iconButton model
                                (ChangeMenu MenuClosed)
                                (Just Pic.menuOpen)
                                none

                    {- Pic.menuOpen model.pal.link
                       model.fontSize
                       (ChangeMenu MenuClosed)
                    -}
                    ]
                ]
        ]



{-

   colorPicker : Model -> Element Msg
   colorPicker model =
       row
           [ spacing <| round <| fontSize * 1.5
           , Font.letterSpacing 1.25
           ]
           [ iconButton model (ChangeMenu MenuOpen) Nothing <| text "🠈 Back"
           , iconButton model ResetView Nothing <| text "Reset"
           ]



      settingsMenu : Model -> Element Msg
      settingsMenu model =
          row
              [ spacing <| round <| fontSize * 1.5
              , Font.letterSpacing 1.25
              ]
              [ iconButton model
                  (ChangeMenu ThemePicker)
                  (Just Pic.pal)
                <|
                  text "Theme"
              ]
-}


mainMenu : Model -> Element Msg
mainMenu model =
    column
        [ spacing <| round <| model.fontSize * 1.5
        , Font.letterSpacing 1.25
        ]
        [ iconButton model
            (GotoPage Home)
            (Just Pic.home)
          <|
            text "Home"
        , iconButton model
            (GotoPage Calendar)
            (Just Pic.calendar)
          <|
            text "Calendar"
        , iconButton model
            (GotoPage Blog)
            (Just Pic.blog)
          <|
            text "Blog"
        , iconButton model
            (GotoPage Solutions)
            (Just Pic.solutions)
          <|
            text "Solutions"
        , iconButton model (GotoPage Graph) (Just Pic.dims) <| text "Graph"
        , hBar
        , if model.pal.name == "Newspaper" then
            iconButton model (ChangeColor dark) (Just Pic.dark) <| text "Mode"

          else
            iconButton model (ChangeColor newspaper) (Just Pic.light) <| text "Mode"
        , iconButton model (GotoPage Settings) (Just Pic.settings) <| text "Settings"
        ]


weekdayToString : Time.Weekday -> String
weekdayToString weekday =
    case weekday of
        Time.Mon ->
            "Monday"

        Time.Tue ->
            "Tuesday"

        Time.Wed ->
            "Wednesday"

        Time.Thu ->
            "Thursday"

        Time.Fri ->
            "Friday"

        Time.Sat ->
            "Saturday"

        Time.Sun ->
            "Sunday"


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "January"

        Time.Feb ->
            "February"

        Time.Mar ->
            "March"

        Time.Apr ->
            "April"

        Time.May ->
            "May"

        Time.Jun ->
            "June"

        Time.Jul ->
            "July"

        Time.Aug ->
            "August"

        Time.Sep ->
            "September"

        Time.Oct ->
            "October"

        Time.Nov ->
            "November"

        Time.Dec ->
            "December"


viewTimeDate : Time.Zone -> Maybe Time.Posix -> String
viewTimeDate zone maybeTime =
    maybeTime
        |> Maybe.map (Date.fromPosix zone)
        |> Maybe.map (Date.format "EEEE, MMMM ddd, y")
        |> Maybe.withDefault ""



-- Helper Functions --


sizeToVp : Float -> Float -> Dom.Viewport
sizeToVp w h =
    { scene =
        { width = w
        , height = h
        }
    , viewport =
        { x = 0
        , y = 0
        , width = w
        , height = h
        }
    }
