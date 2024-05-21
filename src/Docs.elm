module Docs exposing (..)

import Dict exposing (Dict)
import Html
import Html.Attributes as HAttr
import Pic
import Style exposing (..)
import Types exposing (..)
import Ui exposing (..)
import Ui.Background as Bg
import Ui.Border as Border
import Ui.Events as Ev
import Ui.Font as Font



-- Slides --


outline : Pal -> List (Element Msg)
outline m =
    let
        li : String -> String -> Element Msg
        li =
            linkItem m
    in
    [ topGroup m
        [ li "start" "Primary Driver"
        , li "driv" "Proposal Driver"
        , li "reqs" "Requirements and Responsibilities"
        , li "desc" "Description"
        , li "eval" "Evaluation Frequency and Process"
        ]
    ]


initSlides : Dict String (Pal -> Element Msg)
initSlides =
    Dict.fromList
        [ ( "start"
          , \m ->
                topGroup m
                    [ heading m "Gimbalabs Primary Driver"
                    , item m "To provide open, replicable safe spaces to learn, to explore, and to empower individuals and organizations anywhere so that we can solve meaningful problems"
                    , group m
                        "What does this mean?"
                        [ group m
                            "Openness"
                            [ item m "Allow people the freedom to see how things work"
                            , item m "Sharing information for the benefit of others"
                            ]
                        , group m
                            "Replicability"
                            [ item m "Allow people to take your code or workflows to use elsewhere" ]
                        , group m
                            "Safe spaces to learn and explore"
                            [ item m "Provide information in a way that allows people to discover ideas and how they fit together."
                            ]
                        , group m
                            "Empowering people to solve meaningful problems"
                            [ item m "What are meaningful problems?"
                            , item m "How do you empower people to solve them?"
                            ]
                        ]
                    , linkItem m "driv" "Should the website contribute towards these goals?"
                    ]
          )
        , ( "driv"
          , \m ->
                topGroup m
                    [ heading m "Proposal Driver"
                    , item m "The website doesn’t currently reflect the Gimbalabs primary driver. This can hinder newcomers from understanding what Gimbalabs is all about. We will look at how the website can be more open, replicable, a safe space to learn and explore, and more empowering to people."
                    , group m
                        "Some questions to ask"
                        [ item m "How can the website be more open?"
                        , item m "How can it be more replicable?"
                        , item m "How can it be safer?"
                        , item m "How can it promote learning and exploration?"
                        , item m "How can the website become something that empowers individuals and organizations?"
                        , item m "How can the website contribute towards finding solutions to meaningful problems?"
                        ]
                    , linkItem m "desc" "What will be done?"
                    ]
          )
        , ( "desc"
          , \m ->
                topGroup m
                    [ heading m "Description"
                    , item m "Build a Gimbalabs website that actually embodies the goals given in the primary driver!"
                    , item m "The website will be open and reusable for similar use cases in different communities, without the need to do many changes to the code itself. The code will be easy to learn about, maintain, or change for those who want to do so."
                    , item m "The website will also become a useful place to explore different ideas and connect them together. Maybe even allowing content creators to put together little interactive choose-your-own-adventure games to give people a more engaging experience with the information they produce."
                    , item m "All work on the site will be available here: https://thistent.github.io/gimbalabs/"
                    , item m "Ken can take care of the design and development work, but everyone is free to provide input to this process."
                    , item m "Code will be cleaned up to make it as easy as possible for newcomers to get acquainted with how the code-base works."
                    , item m "The site itself could introspectively allow users to explore and learn about its own source."
                    , item m "Building some interactive experiences to showcase what the site could do in the future."
                    , linkItem m "reqs" "What's needed?"
                    ]
          )
        , ( "reqs"
          , \m ->
                topGroup m
                    [ heading m "Requirements"
                    , item m "2000 Ada"
                    , item m "People can share their ideas."
                    , heading m "Responsibilities"
                    , item m "Ken can do the whole thing himself and work full time for asked fund amount"
                    ]
          )
        , ( "eval"
          , \m ->
                topGroup m
                    [ heading m "Evaluation Process"
                    , item m "All progress will be visible on github"
                    , heading m "Evaluation Frequency"
                    , item m "This proposal is for one month"
                    , item m "After a month, progress can be re-evaluated"
                    ]
          )
        ]


notFound : Element Msg
notFound =
    el
        [ centerXY
        , Font.italic
        ]
    <|
        text "Slide not found!"


topGroup : Pal -> List (Element Msg) -> Element Msg
topGroup _ =
    column
        [ spacing spcNum
        , fillSpace
        ]


heading : Pal -> String -> Element Msg
heading _ txt =
    paragraph
        [ Border.widthEach { edges | bottom = lineSize }
        , paddingEach { edges | left = spcNum // 2, bottom = spcNum // 2 }
        , width fill
        , Font.size spcNum
        , spacing <| spcNum // 2
        ]
        [ text txt
        ]


linkItem : Pal -> String -> String -> Element Msg
linkItem color msg txt =
    link [ Ev.onClick <| NextSlide msg ]
        { url = ""
        , label =
            row
                [ spacing <| spcNum // 2
                , Font.color color.link
                , Font.alignLeft
                ]
                [ el
                    [ alignTop
                    , width <| px <| spcNum // 2
                    ]
                  <|
                    Pic.loc color.link
                , paragraph [ spacing <| spcNum // 3 ]
                    [ text txt
                    ]
                ]
        }


item : Pal -> String -> Element Msg
item color txt =
    row
        [ spacing <| spcNum // 2
        ]
        [ el
            [ alignTop
            , width <| px <| spcNum // 2
            ]
          <|
            Pic.bullet color.fg
        , paragraph [ spacing <| spcNum // 3 ]
            [ text txt
            ]
        ]


group : Pal -> String -> List (Element Msg) -> Element Msg
group pal hd els =
    column
        [ Border.widthEach
            { edges
                | left = round <| lineSize * 2
            }
        , Border.roundEach
            { corners
                | topLeft = spcNum // 4
                , bottomLeft = spcNum // 4
            }
        , spacing <| spcNum // 2
        , width fill
        ]
        [ paragraph
            [ Font.bold
            , paddingXY (spcNum // 2) (spcNum // 3)
            , spacing <| spcNum // 4
            , Border.roundEach
                { corners
                    | topRight = spcNum // 4
                    , bottomRight = spcNum // 4
                }
            , moveDown <| lineSize * 3.0
            , Bg.color <| Style.mix 0.9 pal.fg pal.bg
            ]
            [ text hd
            ]

        --, hBar
        , column
            [ spacing <| spcNum // 2
            , width fill
            , paddingEach { edges | left = spcNum // 2 }
            ]
            els
        ]


vBar : Element Msg
vBar =
    el
        [ height fill
        , width <| px <| lineSize * 2
        , Border.widthEach
            { edges
                | left = lineSize
                , right = lineSize // 2
            }
        ]
        none


hBar : Element Msg
hBar =
    el
        [ width fill
        , height <| px <| lineSize * 2
        , Border.widthEach
            { edges
                | top = lineSize
                , bottom = lineSize // 2
            }
        ]
        none


iconButton : Model -> Msg -> Maybe (Color -> Element Msg) -> Element Msg -> Element Msg
iconButton model msg maybeIcon content =
    let
        isLink : Bool
        isLink =
            case msg of
                GotoPage page ->
                    if page == model.page then
                        False

                    else
                        True

                ChangeMenu menu ->
                    if menu == model.menu then
                        False

                    else
                        True

                ChangeColor pal ->
                    if pal == model.color then
                        False

                    else
                        True

                _ ->
                    True

        color : Color
        color =
            if isLink then
                case msg of
                    GetDoc str ->
                        let
                            testStr : String
                            testStr =
                                String.left 4 str
                        in
                        if testStr == "http" then
                            model.color.extLink

                        else
                            model.color.link

                    _ ->
                        model.color.link

            else
                model.color.fg

        linkStyle : Attribute Msg
        linkStyle =
            batch
                [ paddingXY 0 3
                , Font.bold
                , Font.underline
                , Font.color color
                ]

        linkContent : Element Msg
        linkContent =
            case maybeIcon of
                Just icon ->
                    row
                        []
                        [ icon color
                        , el [ width <| px <| lineSize * 3 ] none
                        , el [ linkStyle ] <| content
                        ]

                Nothing ->
                    el [ linkStyle ] <| content
    in
    if isLink then
        link
            [ Ev.onClick msg
            ]
            { url =
                case msg of
                    GetDoc str ->
                        let
                            testStr : String
                            testStr =
                                String.left 4 str
                        in
                        if testStr == "http" then
                            str

                        else
                            ""

                    _ ->
                        ""
            , label = linkContent
            }

    else
        linkContent


pre : List (Attribute Msg) -> String -> Element Msg
pre attrs str =
    el attrs <|
        html <|
            Html.pre
                [ HAttr.style "line-height" <|
                    "calc(1em + "
                        ++ String.fromInt 0
                        -- (spcNum // 4)
                        ++ "px)"
                ]
                [ Html.text str ]


dca : Attribute Msg
dca =
    batch
        [ spacing spcNum
        , paddingEach { edges | left = spcNum }
        ]
