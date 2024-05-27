module Main exposing (..)

-- import Pane

import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events exposing (onResize)
import Browser.Navigation as Nav
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
                    [ Pic.gimbalogo fg bg, text " GIMBA", el [ Font.bold ] <| text "LABS" ]
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
        , page = Solutions --Home
        , menu = MenuClosed
        , color = newspaper
        , size = Delay.wait 1000 Nothing
        , zone = Time.utc
        , time = Nothing
        , currentSlide = "start"
        , slides = initSlides
        , mdText = ""
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

        ReceiveDoc str res ->
            case res of
                Ok p ->
                    Return.singleton
                        { model
                            | mdText = p
                        }

                Err _ ->
                    Return.singleton model



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

        --, Font.justify
        , Bg.color <| Style.mix 0.5 model.color.bg model.color.fg

        --, fillSpace
        , padding <| lineSize * 2
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

                        Blog ->
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

                        Solutions ->
                            renderMd model vp model.mdText

                        Calendar ->
                            el [ fillSpace ] <| el [ centerXY ] <| text "Calendar!"

                        Settings ->
                            el [ fillSpace ] <| el [ centerXY ] <| text "Settings!"
                    ]
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
                Pic.gimbalogo model.color.link model.color.bg
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
        , el [ Border.widthEach { edges | left = lineSize, right = lineSize }, paddingXY (fontSize // 2) 0 ] <|
            if model.color.name == "Newspaper" then
                iconButton model (ChangeColor dark) (Just Pic.dark) none

            else
                iconButton model (ChangeColor newspaper) (Just Pic.light) none
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
            weekdayToString (Time.toWeekday zone time)
                ++ ", "
                ++ monthToString (Time.toMonth zone time)
                ++ " "
                ++ String.fromInt (Time.toDay zone time)
                ++ ", "
                ++ (String.fromInt <| Time.toYear zone time)

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
