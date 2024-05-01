module Paper exposing (..)

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
import Markdown.Block as Md
import Markdown.Html as MdHtml
import Markdown.Parser as Md
import Markdown.Renderer as Md
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
        { init = init newspaper
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

        bg =
            Pic.mix ease
                (rgb 0 0 0)
                model.color.bg

        fg =
            Pic.mix ease
                (rgb 0 0 0)
                model.color.link
    in
    layout
        [ fillSpace
        , Bg.color bg
        , Font.size spcNum
        , Font.family [ Font.serif ]
        ]
    <|
        el [ centerXY, moveUp <| ease * 60.0 ] <|
            column [ centerX, spacing <| spcNum // 6 ]
                [ row
                    [ Font.color fg
                    , Font.letterSpacing 1.25
                    ]
                    [ Pic.gimbalogo fg bg, text " GIMBA", el [ Font.bold ] <| text "LABS" ]
                , el
                    [ Font.color <| Pic.mix 0.5 fg bg
                    , centerX
                    , Font.size <| spcNum * 3 // 5
                    , Font.letterSpacing 2.5
                    ]
                  <|
                    text "Loading..."
                ]



--Initial State --


init : Pal -> () -> Url.Url -> Nav.Key -> Return Msg Model
init color () url key =
    let
        startDoc : String
        startDoc =
            "Main.md"
    in
    Return.return
        { navKey = key
        , url = url
        , page = Blog
        , menu = MenuClosed
        , color = color
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
    let
        slide : Pal -> Element Msg
        slide =
            Maybe.withDefault (\_ -> notFound) <|
                Dict.get model.currentSlide model.slides
    in
    layout
        [ Font.size <| round <| spcNum * 0.55
        , Font.family [ Font.serif ]
        , Font.color model.color.fg
        , Font.justify
        , Bg.color <| Pic.mix 0.5 model.color.bg model.color.fg
        , fillSpace
        , padding <| lineSize * 2 --spcNum // 6
        ]
    <|
        column
            [ fillSpace
            , spacing <| spcNum // 5
            ]
            [ turningPage model <|
                column
                    [ fillSpace
                    , spacing spcNum
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
                                , spacing spcNum
                                , padding <| spcNum // 4
                                ]
                                [ row [ fillSpace, spacing spcNum ]
                                    [ el [ width <| fillPortion 3, height fill ] <|
                                        topGroup p
                                            [ title p "Welcome to Gimbalabs!"
                                            , item p <|
                                                "Right now, we are building Plutus PBL 2024, "
                                                    ++ "running weekly live coding sessions, "
                                                    ++ "and hosting Gimbalabs Open Spaces."
                                            ]
                                    , vBar
                                    , topGroup p
                                        [ title p "Events"
                                        , item p "Open Spaces"
                                        , item p "Playground"
                                        ]
                                    ]
                                , row
                                    [ fillSpace
                                    , spacing spcNum
                                    ]
                                    [ turningPage (m orangeNote) <|
                                        el [ fillSpace ] <|
                                            column [ centerX, spacing <| spcNum // 2 ]
                                                [ title p "Learn"
                                                , el [] <| text "⯀ Starter Kits"
                                                , el [] <| text "⯀ Plutus"
                                                , el [] <| text "⯀ Playground"
                                                ]
                                    , turningPage (m yellowNote) <|
                                        el [ fillSpace ] <|
                                            column [ centerX, spacing <| spcNum // 2 ]
                                                [ title p "APIs"
                                                , el [] <| text "⯀ Dandelion"
                                                , el [] <| text "⯀ Endpoints"
                                                ]
                                    , turningPage (m greenNote) <|
                                        el [ fillSpace ] <|
                                            column [ centerX, spacing <| spcNum // 2 ]
                                                [ title p "Updates"
                                                , el [] <| text "⯀ Updates"
                                                ]
                                    , turningPage (m blueNote) <|
                                        el [ fillSpace ] <|
                                            column [ centerX, spacing <| spcNum // 2 ]
                                                [ title p "About Us"
                                                , el [] <| text "⯀ Team"
                                                , el [] <| text "⯀ Calendar"
                                                , el [] <| text "⯀ Stake Pool"
                                                ]
                                    ]
                                ]

                        Blog ->
                            row
                                [ fillSpace
                                , spacing spcNum
                                ]
                                [ el
                                    [ width <| fillPortion 3
                                    , alignTop
                                    , alignLeft
                                    , spacing spcNum
                                    ]
                                  <|
                                    slide model.color
                                , vBar
                                , column
                                    [ spacing spcNum
                                    , fillSpace
                                    ]
                                  <|
                                    title model.color "Outline"
                                        :: outline model.color
                                ]

                        Solutions ->
                            renderMd model vp model.mdText

                        _ ->
                            el [ fillSpace ] <|
                                el
                                    [ centerXY
                                    , Font.size <| 3 * spcNum // 4
                                    ]
                                <|
                                    text "Not Found Yet!"
                    ]
            ]


turningPage : Model -> Element Msg -> Element Msg
turningPage model content =
    row
        [ -- Bg.color model.color.bg
          Bg.color <| rgba 0 0 0 0
        , Font.color model.color.fg
        , Border.roundEach { corners | bottomRight = spcNum }
        , shadow
        , fillSpace
        ]
        [ el
            [ fillSpace
            , Bg.color model.color.bg
            , paddingEach { edges | left = spcNum, top = spcNum, bottom = round <| toFloat spcNum * 1.5 }
            , otherSide <|
                el
                    [ fillSpace
                    , paddingEach { edges | left = spcNum, top = spcNum, bottom = round <| toFloat spcNum * 1.5 }
                    , alpha 0.04
                    , style "pointer-events" "none"
                    ]
                <|
                    content
            ]
          <|
            content
        , column
            [ height fill
            , width <| px spcNum
            ]
            [ el
                [ fillSpace
                , Bg.color model.color.bg
                ]
                none
            , el
                [ width <| px spcNum
                , height <| px spcNum
                ]
              <|
                Pic.pageCurl model.color
            ]
        ]


titleBar : Model -> Element Msg
titleBar model =
    let
        color =
            model.color
    in
    row
        [ width fill
        , spacing <| spcNum // 4
        ]
        [ link
            [ Ev.onClick <| GotoPage Home ]
            { url = ""
            , label =
                Pic.gimbalogo color.link color.bg
            }
        , el [ fillSpace, paddingXY 0 lineSize ] <|
            row
                [ fillSpace
                , Border.widthEach { edges | top = lineSize, bottom = lineSize }
                , spacing 20
                ]
                [ row
                    [ spacing <| spcNum // 2
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
                                        [ Font.size <| 3 * spcNum // 4
                                        , Font.color color.link
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
                                [ Font.size <| 3 * spcNum // 4
                                , Font.bold
                                ]
                              <|
                                text "Menu:"
                            , el [ alignRight ] <| mainMenu model
                            , Pic.menuOpen model.color.link
                                (ChangeMenu MenuClosed)
                            ]

                        Settings ->
                            [ el
                                [ Font.size <| 3 * spcNum // 4
                                , Font.bold
                                ]
                              <|
                                text "Settings:"
                            , el [ alignRight ] <| settingsMenu model
                            , Pic.menuOpen model.color.link
                                (ChangeMenu MenuClosed)
                            ]

                        ThemePicker ->
                            [ el
                                [ Font.size <| 3 * spcNum // 4
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
        [ spacing <| round <| spcNum * 0.75
        , Font.letterSpacing 1.25
        ]
        [ iconButton model
            (ChangeMenu MainMenu)
            Nothing
          <|
            text "🠈 Back"
        , iconButton model (ChangeColor newspaper) Nothing <| text "Newspaper"
        , iconButton model (ChangeColor blueprint) Nothing <| text "Blueprint"
        , iconButton model (ChangeColor term) Nothing <| text "Terminal"
        , iconButton model (ChangeColor dark) Nothing <| text "Dark Mode"
        , iconButton model
            ResetView
            Nothing
          <|
            text "Reset"
        ]


settingsMenu : Model -> Element Msg
settingsMenu model =
    row
        [ spacing <| round <| spcNum * 0.75
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
        [ spacing <| round <| spcNum * 0.75
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
        , iconButton model
            (ChangeMenu ThemePicker)
            (Just Pic.pal)
          <|
            text "Theme"
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



-- Markdown Stuff --


renderMd : Model -> Viewport -> String -> Element Msg
renderMd model vp str =
    str
        |> Md.parse
        |> Result.withDefault
            [ Md.Paragraph [ Md.Text "Markdown parsing error!" ] ]
        |> Md.render (markdownRenderer model vp)
        --|> Md.render Md.defaultHtmlRenderer
        --|> Result.map (List.map html)
        --|> Md.render (debugRenderer model)
        --|> Result.map (List.map text)
        |> Result.withDefault
            [ text "Markdown rendering error!" ]
        |> (\stuff ->
                column
                    [ spacing spcNum
                    , width <| px <| round <| vp.viewport.width - 2 * spcNum
                    ]
                    stuff
           )


markdownRenderer : Model -> Viewport -> Md.Renderer (Element Msg)
markdownRenderer model vp =
    let
        vw =
            width <| px <| round <| vp.viewport.width - 2 * spcNum
    in
    { heading =
        -- {HeadingLevel, String, List view} -> view
        \{ level, rawText, children } ->
            let
                hAttrs : Float -> List (Attribute Msg)
                hAttrs i =
                    [ Font.size <|
                        round <|
                            spcNum
                                * (1.0 - ((i - 1) * 0.1))
                    , Bg.color <|
                        Pic.mix 0.9 model.color.fg model.color.bg
                    , paddingXY (spcNum // 2) (spcNum // 3)
                    , spacing <| spcNum // 4
                    , vw
                    ]
            in
            case level of
                Md.H1 ->
                    wrappedRow
                        (hAttrs 1)
                        children

                Md.H2 ->
                    wrappedRow
                        (hAttrs 2)
                        children

                Md.H3 ->
                    wrappedRow
                        (hAttrs 3)
                        children

                Md.H4 ->
                    wrappedRow
                        (hAttrs 4)
                        children

                Md.H5 ->
                    wrappedRow
                        (hAttrs 5)
                        children

                Md.H6 ->
                    wrappedRow
                        (hAttrs 6)
                        children
    , paragraph =
        -- List view -> view
        \list ->
            wrappedRow
                [ fillSpace
                ]
                list
    , blockQuote =
        -- List view -> view
        \list ->
            paragraph [ width fill ] list

    -- List view -> view
    , html = MdHtml.oneOf []

    -- Renderer (List view -> view)
    , text =
        -- String -> view
        \str ->
            text str
    , codeSpan =
        -- String -> view
        \str ->
            wrappedRow
                [ Font.family [ Font.monospace ]
                , Bg.color <| Pic.mix 0.05 model.color.bg model.color.fg
                , padding 15
                ]
                [ text str ]
    , strong =
        -- List view -> view
        wrappedRow
            [ Font.bold
            ]
    , emphasis =
        -- List view -> view
        wrappedRow [ Font.italic ]
    , strikethrough =
        -- List view -> view
        wrappedRow [ Font.strike ]
    , hardLineBreak =
        -- view
        none
    , link =
        {-
           { title : Maybe String
           , destination : String
           }
           -> List view
           -> view
        -}
        \{ title, destination } list ->
            iconButton model
                (GetDoc <| destination ++ ".md")
                Nothing
            <|
                wrappedRow
                    []
                    list
    , image =
        {-
           { alt : String
           , src : String
           , title : Maybe String
           }
           -> view
        -}
        \img -> none
    , unorderedList =
        -- List (ListItem view) -> view
        \items ->
            {- FIXME: row [spacing <| spcNum // 2]
               []
            -}
            none
    , orderedList =
        -- Int -> List (List view) -> view
        \startIndex items -> none
    , codeBlock =
        {- { body : String
           , language : Maybe String
           }
           -> view
        -}
        \{ body, language } ->
            let
                lang =
                    case language of
                        Just l ->
                            el
                                [ Font.size 15
                                , paddingXY 15 5
                                , Bg.color <|
                                    Pic.mix 0.1 model.color.bg model.color.fg
                                ]
                            <|
                                text l

                        Nothing ->
                            none
            in
            column
                [ width fill
                , Font.family [ Font.monospace ]
                ]
                [ lang
                , el
                    [ width fill
                    , spacing 15
                    , Bg.color <| Pic.mix 0.05 model.color.bg model.color.fg
                    , padding 15
                    ]
                  <|
                    pre body
                ]
    , thematicBreak =
        -- view
        hBar
    , table =
        -- List view -> view
        \list -> none
    , tableHeader =
        -- List view -> view
        \list -> none
    , tableBody =
        -- List view -> view
        \list -> none
    , tableRow =
        -- List view -> view
        \list -> none
    , tableCell =
        -- Maybe Alignment -> List view -> view
        \maybeArg list -> none
    , tableHeaderCell =
        -- Maybe Alignment -> List view -> view
        \maybeArg list -> none
    }


debugRenderer : Model -> Md.Renderer String
debugRenderer model =
    let
        ls : List String -> String
        ls l =
            l
                |> List.intersperse ", "
                |> String.concat
                |> (\s -> "[" ++ s ++ "]")
    in
    { heading =
        -- {HeadingLevel, String, List view} -> view
        \{ level, rawText, children } ->
            case level of
                Md.H1 ->
                    "(H1 (rawText " ++ rawText ++ ") " ++ ls children ++ ")"

                Md.H2 ->
                    "(H2 (rawText " ++ rawText ++ ") " ++ ls children ++ ")"

                Md.H3 ->
                    "(H3 (rawText " ++ rawText ++ ") " ++ ls children ++ ")"

                Md.H4 ->
                    "(H4 (rawText " ++ rawText ++ ") " ++ ls children ++ ")"

                Md.H5 ->
                    "(H5 (rawText " ++ rawText ++ ") " ++ ls children ++ ")"

                Md.H6 ->
                    "(H6 (rawText " ++ rawText ++ ") " ++ ls children ++ ")"
    , paragraph =
        -- List view -> view
        \list -> "(paragraph " ++ ls list ++ ")"
    , blockQuote =
        -- List view -> view
        \list -> "(blockQuote " ++ ls list ++ ")"
    , html =
        -- Renderer (List view -> view)
        --\list -> "(html " ++ ls list ++ ")"
        MdHtml.oneOf []
    , text =
        -- String -> view
        \s -> "(text " ++ s ++ ")"
    , codeSpan =
        -- String -> view
        \s -> "(codeSpan " ++ s ++ ")"
    , strong =
        -- List view -> view
        \list -> "(strong " ++ ls list ++ ")"
    , emphasis =
        -- List view -> view
        \list -> "(emphasis " ++ ls list ++ ")"
    , strikethrough =
        -- List view -> view
        \list -> "(strikethrough " ++ ls list ++ ")"
    , hardLineBreak =
        -- view
        "(hardLineBreak)"
    , link =
        {-
           { title : Maybe String
           , destination : String
           }
           -> List view
           -> view
        -}
        \{ title, destination } list ->
            "(link (title " ++ Maybe.withDefault "" title ++ ") (destination " ++ destination ++ ") " ++ ls list ++ ")"
    , image =
        {-
           { alt : String
           , src : String
           , title : Maybe String
           }
           -> view
        -}
        \{ alt, src, title } -> "(image (alt " ++ alt ++ ") (src " ++ src ++ ") (title " ++ Maybe.withDefault "" title ++ "))"
    , unorderedList =
        -- List (ListItem view) -> view
        \items ->
            "(unorderedList)"
    , orderedList =
        -- Int -> List (List view) -> view
        \startIndex items ->
            "(orderedList)"
    , codeBlock =
        {- { body : String
           , language : Maybe String
           }
           -> view
        -}
        \{ body, language } ->
            "(codeBlock (lang " ++ Maybe.withDefault "" language ++ ") (body " ++ body ++ "))"
    , thematicBreak =
        -- view
        "(thematicBreak)"
    , table =
        -- List view -> view
        \list -> "(table " ++ ls list ++ ")"
    , tableHeader =
        -- List view -> view
        \list -> "(tableHeader " ++ ls list ++ ")"
    , tableBody =
        -- List view -> view
        \list -> "(tableBody " ++ ls list ++ ")"
    , tableRow =
        -- List view -> view
        \list -> "(tableRow " ++ ls list ++ ")"
    , tableCell =
        -- Maybe Alignment -> List view -> view
        \_ list -> "(tableCell " ++ ls list ++ ")"
    , tableHeaderCell =
        -- Maybe Alignment -> List view -> view
        \_ list -> "(tableHeaderCell " ++ ls list ++ ")"
    }


pre : String -> Element Msg
pre str =
    html <| Html.pre [] [ Html.text str ]
