module Main exposing (..)

-- import Pane

import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events exposing (onResize)
import Browser.Navigation as Nav
import Calendar
import Date exposing (Date)
import Delay exposing (Timer)
import Dict
import Docs exposing (..)
import Ease
import Html exposing (Html)
import Http
import Markup exposing (renderMd)
import Pic
import Return exposing (Return)
import Style exposing (..)
import Task
import Time
import Types exposing (..)
import Ui exposing (..)
import Ui.Background as Bg
import Ui.Border as Border
import Ui.Events as Ev
import Ui.Font as Font
import Url



-- Main --


main : Program () Model Msg
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
                model.color.bg

        fg : Color
        fg =
            Style.mix ease
                (rgb 0 0 0)
                model.color.link
    in
    layout
        [ fillSpace
        , Bg.color bg
        , Font.size <| fontSize * 2
        , Font.family [ Font.serif ]
        ]
    <|
        el [ centerXY, moveUp <| ease * 60.0 ] <|
            column [ centerX, spacing <| fontSize // 3 ]
                [ row
                    [ Font.color fg
                    , Font.letterSpacing 1.25
                    ]
                    [ el
                        [ Bg.color fg
                        , height <| px <| fontSize * 3
                        , width <| px <| fontSize * 3
                        , paddingXY 0 5
                        ]
                      <|
                        el [ centerXY, scale 1.4 ] <|
                            Pic.gimbalogo bg
                    , text " GIMBA"
                    , el [ Font.bold ] <| text "LABS"
                    ]
                , el
                    [ Font.color <| Style.mix 0.5 fg bg
                    , centerX
                    , Font.size <| fontSize
                    , Font.letterSpacing 2.5
                    ]
                  <|
                    text "Loading..."
                ]



--Initial State --


init : () -> Url.Url -> Nav.Key -> Return Msg Model
init () url key =
    let
        startDoc : String
        startDoc =
            "Main.md"
    in
    Return.return
        { navKey = key
        , url = url
        , page = Home
        , menu = MenuClosed
        , color = newspaper
        , size = Delay.wait 100 Nothing
        , zone = Time.utc
        , time = Nothing
        , currentSlide = "start"
        , slides = initSlides
        , docText = ""
        , docName = ""
        , clock = North
        , selectDate = Nothing
        }
    <|
        Cmd.batch
            [ Task.perform SceneInfo Dom.getViewport
            , Task.perform AdjustTimeZone Time.here
            , Http.get
                { url = "notes/" ++ startDoc
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
                { model | color = scheme }

        ResetView ->
            Return.singleton
                { model
                    | -- page = Home
                      --, menu = MenuClosed
                      size = Delay.reset model.size
                }

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
                    { url = "notes/" ++ doc
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
            case model.clock of
                North ->
                    Return.singleton { model | clock = South }

                South ->
                    Return.singleton { model | clock = North }

        SelectDate maybeDate ->
            Return.singleton { model | selectDate = maybeDate }



-- Subscriptions --


subs : Model -> Sub Msg
subs _ =
    Sub.batch
        [ onResize WindowResize
        , Events.onAnimationFrameDelta FrameDelta
        , Time.every 100 Tick
        ]



-- View --


view : Model -> Viewport -> Html Msg
view model vp =
    layout
        [ Font.size fontSize
        , Font.family [ Font.serif ]
        , Font.color model.color.fg
        , Font.letterSpacing 0.2
        , Bg.color <| Style.mix 0.5 model.color.bg model.color.fg
        ]
    <|
        el
            [ Bg.color <| Style.mix 0.5 model.color.bg model.color.fg
            , fillSpace
            ]
        <|
            column
                [ fillSpace
                , spacing <| fontSize * 2 // 5
                ]
                [ turningPage model 0 <|
                    column
                        [ fillSpace
                        , spacing <| fontSize * 2
                        ]
                        [ titleBar model
                        , case model.page of
                            Home ->
                                let
                                    m : Pal -> Model
                                    m pal =
                                        { model | color = pal }

                                    p : Pal
                                    p =
                                        model.color
                                in
                                column
                                    [ fillSpace
                                    , spacing <| fontSize * 2
                                    , padding <| fontSize // 2
                                    ]
                                    [ row [ fillSpace, spacing <| fontSize * 2 ]
                                        [ el [ width <| fillPortion 3, height fill ] <|
                                            topGroup p
                                                [ heading p "Welcome to Gimbalabs!"
                                                , item p <|
                                                    "Right now, we are building Plutus PBL 2024, "
                                                        ++ "running weekly live coding sessions, "
                                                        ++ "and hosting Gimbalabs Open Spaces."
                                                , item p <|
                                                    "This version of the website is still under construction!"
                                                ]
                                        , vBar
                                        , topGroup p
                                            [ heading p "Events"
                                            , item p "Open Spaces"
                                            , item p "Playground"
                                            ]
                                        ]
                                    , row
                                        [ fillSpace
                                        , spacing <| fontSize * 2
                                        ]
                                        [ turningPage (m orangeNote) 0.02 <|
                                            el [ fillSpace ] <|
                                                column [ centerX, spacing fontSize ]
                                                    [ heading p "Learn"
                                                    , el [] <| text "⯀ Starter Kits"
                                                    , el [] <| text "⯀ Plutus"
                                                    , el [] <| text "⯀ Playground"
                                                    ]
                                        , turningPage (m yellowNote) 0 <|
                                            el [ fillSpace ] <|
                                                column [ centerX, spacing fontSize ]
                                                    [ heading p "APIs"
                                                    , el [] <| text "⯀ Dandelion"
                                                    , el [] <| text "⯀ Endpoints"
                                                    ]
                                        , turningPage (m greenNote) -0.02 <|
                                            el [ fillSpace ] <|
                                                column [ centerX, spacing fontSize ]
                                                    [ heading p "Updates"
                                                    , el [] <| text "⯀ Updates"
                                                    ]
                                        , turningPage (m blueNote) 0.01 <|
                                            el [ fillSpace ] <|
                                                column [ centerX, spacing fontSize ]
                                                    [ heading p "About Us"
                                                    , el [] <| text "⯀ Team"
                                                    , el [] <| text "⯀ Calendar"
                                                    , el [] <| text "⯀ Stake Pool"
                                                    ]
                                        ]
                                    ]

                            Calendar ->
                                row [ fillSpace, spacing <| fontSize * 2 ]
                                    [ el [ width <| fillPortion 5, height fill ] <|
                                        calendar model
                                    , el
                                        [ width <| fillPortion 2
                                        , inFront <|
                                            case model.selectDate of
                                                Nothing ->
                                                    none

                                                Just date ->
                                                    column
                                                        [ fillSpace
                                                        , padding fontSize
                                                        , spacing <| fontSize * 2
                                                        , Border.width lineSize
                                                        , Border.rounded <| fontSize // 2
                                                        , Bg.color <| Style.addAlpha 0.9 model.color.bg
                                                        ]
                                                        [ column
                                                            [ spacing <| round <| fontSize * 0.65
                                                            , width fill
                                                            ]
                                                            [ el
                                                                [ centerX
                                                                , Font.size <| round <| fontSize * 1.5
                                                                , Font.bold
                                                                ]
                                                              <|
                                                                text <|
                                                                    (weekdayToString <| Date.weekday date)
                                                            , el
                                                                [ centerX
                                                                , Font.size <| round <| fontSize * 1.5
                                                                , Font.bold
                                                                ]
                                                              <|
                                                                text <|
                                                                    (monthToString <| Date.month date)
                                                                        ++ " "
                                                                        ++ String.fromInt (Date.day date)
                                                                        ++ ", "
                                                                        ++ String.fromInt (Date.year date)
                                                            , hBar
                                                            ]
                                                        , column
                                                            [ spacing <| fontSize * 2
                                                            , paddingXY (fontSize // 2) 0
                                                            , width fill
                                                            ]
                                                            [ el [ width fill ] <| text "This is the first event!"
                                                            , el [ width fill ] <| text "This is the second event!"
                                                            , el [ width fill ] <| text "This is the third event!"
                                                            ]
                                                        ]
                                        , height fill
                                        ]
                                      <|
                                        el [ alignTop, centerX, moveDown 20.0 ] <|
                                            clock model
                                    ]

                            Blog ->
                                column [ fillSpace, spacing <| fontSize * 2 ]
                                    [ row [ width fill, spacing fontSize ]
                                        [ el [ Font.bold ] <| text "File:"
                                        , text <| "/notes/" ++ model.docName
                                        , el [ alignRight ] <| iconButton model (GetDoc "Main.md") Nothing <| text "Go back to the Main Page"
                                        ]
                                    , renderMd model vp model.docText
                                    ]

                            Solutions ->
                                let
                                    slide : Pal -> Element Msg
                                    slide =
                                        Maybe.withDefault (\_ -> notFound) <|
                                            Dict.get model.currentSlide model.slides
                                in
                                row
                                    [ fillSpace
                                    , spacing <| fontSize * 2
                                    ]
                                    [ el
                                        [ width <| fillPortion 3
                                        , alignTop
                                        , alignLeft
                                        , spacing <| fontSize * 2
                                        ]
                                      <|
                                        slide model.color
                                    , vBar
                                    , column
                                        [ spacing <| fontSize * 2
                                        , fillSpace
                                        ]
                                      <|
                                        heading model.color "Outline"
                                            :: outline model.color
                                    ]

                            Settings ->
                                el [ fillSpace ] <| el [ centerXY ] <| text "Settings!"
                        ]
                ]


calendar : Model -> Element Msg
calendar model =
    column
        [ fillSpace
        , Border.width lineSize
        , Border.roundEach { corners | topLeft = fontSize // 2, topRight = fontSize // 2 }
        , Style.shadow
        ]
        [ column
            [ width fill
            , Border.widthEach { edges | bottom = lineSize }
            , Border.roundEach { corners | topLeft = fontSize // 2, topRight = fontSize // 2 }
            , Bg.color <| Style.mix 0.075 model.color.bg model.color.fg
            ]
            [ el
                [ centerX
                , Font.size <| round <| fontSize * 1.5
                , Font.bold
                , paddingXY 0 fontSize
                ]
              <|
                text <|
                    (model.time
                        |> Maybe.map
                            (\t ->
                                monthToString (Time.toMonth Time.utc t)
                                    ++ " "
                                    ++ String.fromInt (Time.toYear Time.utc t)
                            )
                        |> Maybe.withDefault "Calendar"
                    )
            , row
                [ width fill
                , paddingXY 0 <| round <| fontSize * 0.4
                ]
                [ el [ width fill, Font.bold ] <| el [ centerX ] <| text "Sunday"
                , el [ width fill, Font.bold ] <| el [ centerX ] <| text "Monday"
                , el [ width fill, Font.bold ] <| el [ centerX ] <| text "Tuesday"
                , el [ width fill, Font.bold ] <| el [ centerX ] <| text "Wednesday"
                , el [ width fill, Font.bold ] <| el [ centerX ] <| text "Thursday"
                , el [ width fill, Font.bold ] <| el [ centerX ] <| text "Friday"
                , el [ width fill, Font.bold ] <| el [ centerX ] <| text "Saturday"
                ]
            ]
        , el
            [ fillSpace
            ]
          <|
            (model.time
                |> Maybe.map (Calendar.fromTime Nothing Time.utc)
                |> Maybe.map
                    (\weeks ->
                        column
                            [ fillSpace
                            ]
                        <|
                            List.map
                                (\days ->
                                    row
                                        [ fillSpace
                                        ]
                                        (List.map
                                            (\d ->
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
                                                        Date.compare d.date (Date.fromPosix Time.utc date.posix) == EQ

                                                    isSelected : Bool
                                                    isSelected =
                                                        case model.selectDate of
                                                            Just sd ->
                                                                Date.compare d.date sd == EQ

                                                            Nothing ->
                                                                False

                                                    isThisMonth : Bool
                                                    isThisMonth =
                                                        Date.month d.date == date.month

                                                    bg : Color
                                                    bg =
                                                        case ( isSelected, isThisMonth ) of
                                                            ( True, True ) ->
                                                                Style.mix 0.5 model.color.link model.color.bg

                                                            ( True, False ) ->
                                                                Style.mix 0.5 model.color.link <| Style.mix 0.35 model.color.bg model.color.fg

                                                            ( False, True ) ->
                                                                model.color.bg

                                                            ( False, False ) ->
                                                                Style.mix 0.35 model.color.bg model.color.fg

                                                    internalBorder : Color
                                                    internalBorder =
                                                        if isToday then
                                                            --Style.mix 0.5 model.color.bg model.color.link
                                                            model.color.fg

                                                        else
                                                            bg
                                                in
                                                el
                                                    [ fillSpace
                                                    , padding <| fontSize // 6
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
                                                                    Just d.date
                                                        ]
                                                        { url = ""
                                                        , label =
                                                            column
                                                                [ fillSpace
                                                                , padding <| fontSize // 2 - fontSize // 6
                                                                , spacing <| fontSize // 2 - fontSize // 6
                                                                , Font.size 10
                                                                , Bg.color bg
                                                                ]
                                                                [ el [ Font.size 16, Font.bold ] <| text <| String.fromInt <| Date.day d.date
                                                                , paragraph
                                                                    [ width fill
                                                                    , Bg.color <| Style.addAlpha 0.7 <| Style.mix 0.25 model.color.bg model.color.error
                                                                    , Border.color <| Style.mix 0.5 model.color.bg model.color.error
                                                                    , Border.width 1
                                                                    , Border.rounded <| fontSize // 3
                                                                    , padding <| fontSize // 2 - fontSize // 6
                                                                    ]
                                                                    [ text "Event: This is Important!"
                                                                    ]
                                                                , paragraph
                                                                    [ width fill
                                                                    , Bg.color <| Style.addAlpha 0.7 <| Style.mix 0.25 model.color.bg model.color.link
                                                                    , Border.color <| Style.mix 0.5 model.color.bg model.color.link
                                                                    , Border.width 1
                                                                    , Border.rounded <| fontSize // 3
                                                                    , padding <| fontSize // 2 - fontSize // 6
                                                                    ]
                                                                    [ text "Event: This is Important!"
                                                                    ]
                                                                , paragraph
                                                                    [ width fill
                                                                    , Bg.color <| Style.addAlpha 0.7 <| Style.mix 0.25 model.color.bg model.color.extLink
                                                                    , Border.color <| Style.mix 0.5 model.color.bg model.color.extLink
                                                                    , Border.width 1
                                                                    , Border.rounded <| fontSize // 3
                                                                    , padding <| fontSize // 2 - fontSize // 6
                                                                    ]
                                                                    [ text "Event: This is Important!"
                                                                    ]
                                                                ]
                                                        }
                                            )
                                            days
                                        )
                                )
                                weeks
                    )
                |> Maybe.withDefault none
            )
        ]


clock : Model -> Element Msg
clock model =
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
            case model.clock of
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
        [ clip ]
        [ image
            [ centerXY
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
        , el [ height <| px <| fontSize * 2 ] none
        , el [ centerX ] <|
            iconButton model ToggleClockOrientation Nothing <|
                text ("Flip Clock to " ++ c.op ++ " Hemisphere")
        , el [ height <| px <| fontSize * 2 ] none
        , el
            [ centerX
            , Font.size <| fontSize * 4 // 3
            , Font.bold
            ]
          <|
            text "Current Time:"
        , el [ height <| px <| fontSize ] none
        , el [ centerX, Font.size <| fontSize * 4 // 3 ] <|
            text <|
                String.fromInt time.hours
                    ++ ":"
                    ++ (String.padLeft 2 '0' <| String.fromInt time.minutes)
                    ++ " UTC"
        , el [ height <| px <| fontSize ] none
        , el [ centerX, Font.size <| fontSize * 4 // 3 ] <|
            text <|
                String.fromInt time.localHours
                    ++ ":"
                    ++ (String.padLeft 2 '0' <| String.fromInt time.minutes)
                    ++ " Local"
        ]


turningPage : Model -> Float -> Element Msg -> Element Msg
turningPage model rot content =
    row
        [ Font.color model.color.fg
        , Border.roundEach { corners | bottomRight = fontSize * 2 }
        , shadow
        , fillSpace
        , rotate rot
        ]
        [ el
            [ fillSpace
            , Bg.color model.color.bg
            , paddingEach
                { edges
                    | left = fontSize * 2
                    , top = fontSize * 2
                    , bottom = fontSize * 3
                }
            , otherSide <|
                el
                    [ fillSpace
                    , paddingEach
                        { edges
                            | left = fontSize * 2
                            , top = fontSize * 2
                            , bottom = fontSize * 3
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
            , width <| px <| fontSize * 2
            ]
            [ el
                [ fillSpace
                , Bg.color model.color.bg
                ]
                none
            , el
                [ width <| px <| fontSize * 2
                , height <| px <| fontSize * 2
                , Bg.color model.color.bg
                , Border.roundEach { corners | bottomRight = fontSize * 2 }
                ]
              <|
                Pic.pageCurl model.color
            ]
        ]


titleBar : Model -> Element Msg
titleBar model =
    row
        [ width fill
        , spacing <| fontSize // 2
        ]
        [ link
            [ Ev.onClick <| GotoPage Home ]
            { url = ""
            , label =
                el
                    [ Bg.color model.color.link
                    , height <| px <| fontSize * 3
                    , width <| px <| fontSize * 3
                    , paddingXY 0 5
                    ]
                <|
                    el [ centerXY, scale 1.4 ] <|
                        Pic.gimbalogo model.color.bg
            }
        , el [ fillSpace, paddingXY 0 lineSize ] <|
            row
                [ fillSpace
                , Border.widthEach { edges | top = lineSize, bottom = lineSize }
                , spacing fontSize
                , paddingXY 0 <| fontSize // 2
                ]
                [ row
                    [ spacing fontSize
                    , width fill
                    , centerY
                    ]
                  <|
                    case model.menu of
                        MenuClosed ->
                            [ link [ Ev.onClick <| GotoPage Home ]
                                { url = ""
                                , label =
                                    row
                                        [ Font.size <| 3 * fontSize // 2
                                        , Font.color model.color.link
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
                            , Pic.menuClosed model.color.link
                                (ChangeMenu MainMenu)
                            ]

                        MainMenu ->
                            [ el
                                [ Font.size <| 3 * fontSize // 2
                                , Font.bold
                                ]
                              <|
                                text "Menu:"
                            , el [ alignRight ] <| mainMenu model
                            , Pic.menuOpen model.color.link
                                (ChangeMenu MenuClosed)
                            ]

                        ThemePicker ->
                            [ el
                                [ Font.size <| 3 * fontSize // 2
                                , Font.bold
                                ]
                              <|
                                text "Select Theme:"
                            , el [ alignRight ] <| colorPicker model
                            , Pic.menuOpen model.color.link
                                (ChangeMenu MenuClosed)
                            ]
                ]
        ]


colorPicker : Model -> Element Msg
colorPicker model =
    row
        [ spacing <| round <| fontSize * 1.5
        , Font.letterSpacing 1.25
        ]
        [ iconButton model (ChangeMenu MainMenu) Nothing <| text "🠈 Back"
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


mainMenu : Model -> Element Msg
mainMenu model =
    row
        [ spacing <| round <| fontSize * 1.5
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
        , iconButton model (GotoPage Settings) (Just Pic.settings) <| text "Settings"
        , row
            [ Border.widthEach { edges | left = lineSize, right = lineSize }
            , paddingXY (fontSize // 2) 0
            , spacing (fontSize // 2)
            ]
          <|
            if model.color.name == "Newspaper" then
                [ iconButton model (ChangeColor dark) (Just Pic.dark) none
                , iconButton model (GotoPage Settings) (Just Pic.dims) none

                --, iconButton model (GotoPage Settings) (Just Pic.gimbalogo) <| text "Gimbalabs"
                ]

            else
                [ iconButton model (ChangeColor newspaper) (Just Pic.light) none
                , iconButton model (GotoPage Settings) (Just Pic.dims) none
                ]
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
    case maybeTime of
        Just time ->
            let
                date : Date
                date =
                    Date.fromPosix zone time
            in
            weekdayToString (Date.weekday date)
                ++ ", "
                ++ monthToString (Date.month date)
                ++ " "
                ++ String.fromInt (Date.day date)
                ++ ", "
                ++ String.fromInt (Date.year date)

        Nothing ->
            ""



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
