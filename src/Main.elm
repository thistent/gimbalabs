module Main exposing (..)

import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Color exposing (Color)
import Color.Manipulate as CM
import Html exposing (Html)
import Task
import TypedSvg as Ts
import TypedSvg.Attributes as Ta
import TypedSvg.Core as Tc
import TypedSvg.Types as Tt
import Ui as El exposing (Attribute, Element, el)
import Ui.Background as Bg exposing (color)
import Ui.Border as Border
import Ui.Font as Font
import Ui.Input as Input



-- Types --


type alias Model =
    { window : Viewport
    , page : Page
    }


type Msg
    = WindowSize Viewport
    | WindowResize Float Float
    | Tick Float
    | GotoPage Page


type Page
    = GridPage
    | CalendarPage
    | BlogPage
    | SolutionPage


type alias SizeRange =
    { minWidth : Int
    , minHeight : Int
    , maxWidth : Int
    , maxHeight : Int
    }


type alias Pal =
    { black : Color
    , white : Color
    , yellow : Color
    , orange : Color
    , green : Color
    , lavender : Color
    , dark : Color
    , blue : Color
    , teal : Color
    , sky : Color
    , gray : Color
    , addColor : Color
    }



-- Main Function --


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subs
        , view = view
        }



-- Init --


init : () -> ( Model, Cmd Msg )
init _ =
    ( { window = sizeToVp 0 0
      , page = GridPage
      }
    , Task.perform WindowSize Dom.getViewport
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowSize vp ->
            ( { model | window = vp }
            , Cmd.none
            )

        WindowResize w h ->
            ( { model
                | window =
                    { scene =
                        { width = model.window.scene.width
                        , height = model.window.scene.height
                        }
                    , viewport =
                        { x = model.window.viewport.x
                        , y = model.window.viewport.y
                        , width = w
                        , height = h
                        }
                    }
              }
            , Cmd.none
            )

        Tick _ ->
            ( model
            , Task.perform WindowSize
                Dom.getViewport
            )

        GotoPage page ->
            ( { model | page = page }
            , Cmd.none
            )


sizeToVp : Float -> Float -> Viewport
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



-- Subscriptions --


subs : Model -> Sub Msg
subs _ =
    Sub.batch
        [ Events.onAnimationFrameDelta Tick
        , Events.onResize (\w h -> WindowResize (toFloat w) (toFloat h))
        ]



-- View --


view : Model -> Html Msg
view model =
    El.layout
        [ fillSpace
        , Font.color pal.black
        , El.inFront <| menu model
        ]
    <|
        el [ fillSpace ] <|
            el
                [ fillSpace
                , Bg.color pal.gray
                ]
            <|
                case model.page of
                    GridPage ->
                        grid model

                    CalendarPage ->
                        placeholderPage "Calendar"

                    BlogPage ->
                        placeholderPage "Blog"

                    SolutionPage ->
                        placeholderPage "Solutions"


menu : Model -> Element Msg
menu _ =
    El.row
        [ El.width El.fill
        , Font.color pal.white
        , Bg.color pal.dark
        , El.paddingXY 10 5
        , El.spacing 30
        , shadow
        ]
        [ menuButton [] GridPage gimbalabsPic <|
            El.row []
                [ El.text "GIMBA"
                , el [ Font.bold ] <| El.text "LABS"
                , El.text " 2024"
                ]
        , el [ El.width El.fill ] El.none
        , menuButton [ El.alignRight ] CalendarPage calendarPic <|
            El.text "Calendar"
        , menuButton [ El.alignRight ] BlogPage blogPic <|
            El.text "Blog"
        , menuButton [ El.alignRight ] SolutionPage solutionsPic <|
            El.text "Solutions"
        ]


grid : Model -> Element Msg
grid model =
    let
        w : Float
        w =
            model.window.viewport.width

        h : Float
        h =
            model.window.viewport.height

        size : SizeRange
        size =
            { minWidth = round <| w * 0.3
            , minHeight = round <| h * 0.3
            , maxWidth = round <| w * 1.5
            , maxHeight = round <| h * 1.5
            }
    in
    if model.window.viewport.width == 0 then
        El.none

    else
        el
            [ El.width El.fill
            , El.paddingEach
                { left = padVal + 5
                , right = padVal + 5
                , top = mh + padVal * 2
                , bottom = padVal + 5
                }
            , spc
            ]
        <|
            El.wrappedRow [ fillSpace, spc ] <|
                [ -- viewportView size model.window ,
                  welcome size
                , githubLink size
                , openSpaces size
                , learnNote size
                , apiNote size
                , updateNote size
                , aboutNote size
                , add size
                , add size
                ]



-- Note Stuff --


add : SizeRange -> Element Msg
add size =
    el
        [ fillSpace
        ]
    <|
        el [ centerXY ] <|
            el
                [ fillWithRange size
                , centerXY
                , Bg.color pal.gray
                , Border.width 4
                , Border.dashed
                , Border.color pal.addColor
                , El.padding 10
                ]
            <|
                svgWrap size <|
                    El.column
                        [ centerXY
                        , Font.color pal.addColor
                        ]
                        [ El.image [ El.height <| El.px <| round <| toFloat size.minHeight * 0.3, El.centerX ]
                            { src = "assets/add.svg"
                            , description = "Add"
                            }
                        , el [ Font.center, Font.bold ] <| El.text "Add Item"
                        ]


note : Color -> Float -> SizeRange -> Element Msg -> Element Msg
note color rot size elem =
    el [ fillSpace ] <|
        el [ centerXY ] <|
            el
                [ fillWithRange size
                , Bg.color color
                , shadow
                , El.rotate rot
                , El.padding 10
                ]
            <|
                svgWrap size elem


viewportView : SizeRange -> Viewport -> Element Msg
viewportView size vp =
    let
        format f =
            String.padRight 18 ' ' <| String.fromFloat <| f
    in
    note pal.blue 0.02 size <|
        El.column
            [ Font.family [ Font.monospace ]
            , Font.size 12
            , El.centerY
            ]
            [ El.text <| "{ scene ="
            , El.text <| "    { width = " ++ format vp.scene.width
            , El.text <| "    , height = " ++ format vp.scene.height
            , El.text <| "    }"
            , El.text <| ", viewport ="
            , El.text <| "    { x = " ++ format vp.viewport.x
            , El.text <| "    , y = " ++ format vp.viewport.y
            , El.text <| "    , width = " ++ format vp.viewport.width
            , El.text <| "    , height = " ++ format vp.viewport.height
            , El.text <| "    }"
            , El.text <| "}"
            ]



-- Fun --


scaleRangeXY : Float -> Float -> SizeRange -> SizeRange
scaleRangeXY sx sy size =
    { size
        | minWidth = round <| toFloat size.minHeight * sx
        , minHeight = round <| toFloat size.minHeight * sy
        , maxWidth = round <| toFloat size.minHeight * sx
        , maxHeight = round <| toFloat size.maxHeight * sy
    }



-- Items --


openSpaces : SizeRange -> Element Msg
openSpaces size =
    El.image
        [ El.width <|
            El.minimum size.minWidth <|
                El.maximum size.maxWidth El.fill
        , rotLeft
        , shadow
        ]
        { src = "assets/open-spaces-card-1.webp"
        , description =
            "Gimbalabs Open Spaces. "
                ++ "Wednesday and Thursday at 14:30 UTC. Click for more details."
        }


githubLink : SizeRange -> Element Msg
githubLink size =
    note pal.sky 0.0 size <|
        El.link
            [ fillSpace
            ]
            { url = "https://github.com/thistent/gimbalabs"
            , label =
                El.column
                    [ centerXY ]
                    [ El.image [ El.centerX ]
                        { src = "assets/github.svg"
                        , description = "GitHub"
                        }
                    , El.paragraph
                        [ Font.center
                        ]
                        [ El.text "View source on GitHub"
                        ]
                    ]
            }


welcome : SizeRange -> Element Msg
welcome size =
    note pal.yellow 0.02 size <|
        El.column
            [ fillWithRange size
            , Font.size 16
            , Font.center
            , rotRight
            , spc
            ]
            [ title "Welcome!"
            , El.paragraph [ El.centerX ]
                [ El.text <|
                    "Right now, we are building Plutus PBL 2024, "
                        ++ "running weekly live coding sessions, "
                        ++ "and hosting Gimbalabs Open Spaces."
                ]
            ]


title : String -> Element Msg
title str =
    el
        [ Border.widthEach { edges | bottom = 1 }
        , El.padding 10
        , Font.size 30
        , El.width El.fill
        ]
    <|
        El.text str


learnNote : SizeRange -> Element Msg
learnNote size =
    note pal.teal 0.02 size <|
        El.column [ spc ]
            [ title "Learn"
            , El.column [ El.centerX, spc ]
                [ el [] <| El.text "⯀ Starter Kits"
                , el [] <| El.text "⯀ Plutus"
                , el [] <| El.text "⯀ Playground"
                ]
            ]


apiNote : SizeRange -> Element Msg
apiNote size =
    note pal.orange 0.0 size <|
        El.column [ centerXY, spc ]
            [ title "APIs"
            , El.column [ El.centerX, spc ]
                [ el [] <| El.text "⯀ Dandelion"
                , el [] <| El.text "⯀ Endpoints"
                ]
            ]


updateNote : SizeRange -> Element Msg
updateNote size =
    note pal.green -0.02 size <|
        El.column [ centerXY, spc ]
            [ title "Updates"
            , El.column [ El.centerX, spc ]
                [ el [] <| El.text "⯀ Updates"
                ]
            ]


aboutNote : SizeRange -> Element Msg
aboutNote size =
    note pal.lavender 0.02 size <|
        El.column [ centerXY, spc ]
            [ title "About Us"
            , El.column [ El.centerX, spc ]
                [ el [] <| El.text "⯀ Team"
                , el [] <| El.text "⯀ Calendar"
                , el [] <| El.text "⯀ Stake Pools"
                ]
            ]


placeholderPage : String -> Element Msg
placeholderPage str =
    el
        [ centerXY
        , Font.size 40
        ]
    <|
        El.text str



-- Break --


centerXY : Attribute Msg
centerXY =
    El.batch [ El.centerX, El.centerY ]



-- Colors --


pal : Pal
pal =
    { black = Color.rgb255 0x00 0x00 0x00 -- #000000
    , yellow = Color.rgb255 0xFF 0xFF 0xA0 -- #ffffa0
    , orange = Color.rgb255 0xFF 0xDD 0xAA -- #ffddaa
    , green = Color.rgb255 0xBB 0xFF 0xCC -- #bbffcc
    , lavender = Color.rgb255 0xEE 0xDD 0xFF -- #eeddff
    , dark = Color.rgb255 0x05 0x13 0x1E -- #05131e
    , blue = Color.rgb255 0x88 0xDD 0xFF -- #88ddff
    , teal = Color.rgb255 0x88 0xFF 0xEE -- #88ffee
    , white = Color.rgb255 0x88 0xFF 0xFF -- #ffffff
    , sky = Color.rgb255 0xDB 0xF4 0xF4 -- #dbf4f4
    , gray = Color.rgb255 0xA0 0xB0 0xC0 -- #a0b0c0
    , addColor = Color.rgb255 0x88 0x99 0xAA -- #8899aa
    }


mix : Float -> Color -> Color -> Color
mix n a b =
    CM.weightedMix a b n


pal2 =
    let
        bl =
            -- #0c0e13
            El.rgb255 0x0C 0x0E 0x13

        wh =
            -- #c0d3d3
            El.rgb255 0xC0 0xD3 0xD3
    in
    { black = bl
    , darkGray = mix 0.05 bl wh
    , gray = mix 0.4 bl wh
    , white = wh
    , blue = Color.rgb255 0x22 0x9D 0xDF -- #229ddf
    , cyan = Color.rgb255 0x0E 0xEE 0xF8 -- #0eeef8
    , cursorDark = Color.rgb255 0x00 0x55 0x66 -- #005566
    , cursorLight = Color.rgb255 0x00 0xF0 0x94 -- #00f094
    , green = Color.rgb255 0x18 0xB8 0x00 -- #18b800
    , lightGreen = Color.rgb255 0xAB 0xD6 0x00 -- #abd600
    , yellow = Color.rgb255 0xFE 0xD1 0x67 -- #fed167
    , orange = Color.rgb255 0xFF 0x9D 0x47 -- #ff9d47
    , redOrange = Color.rgb255 0xDD 0x58 0x1D -- #dd581d
    , darkRed = Color.rgb255 0x88 0x20 0x40 -- #882040
    , pink = Color.rgb255 0xFF 0x68 0x73 -- #ff6873
    , berry = Color.rgb255 0xA8 0x38 0xB0 -- #a838b0
    }



-- Other Stuff --


rotLeft : Attribute Msg
rotLeft =
    El.rotate -0.02


rotRight : Attribute Msg
rotRight =
    El.rotate 0.02


edges : { left : Int, right : Int, top : Int, bottom : Int }
edges =
    { left = 0, right = 0, top = 0, bottom = 0 }


menuButton : List (Attribute Msg) -> Page -> Element Msg -> Element Msg -> Element Msg
menuButton attrs page pic elem =
    Input.button
        [ El.height El.fill
        , El.batch attrs
        ]
        { onPress = Just <| GotoPage page
        , label =
            El.row
                [ Font.size <| round <| toFloat mh * 0.5
                , El.height El.fill
                , El.spacing 8
                ]
                [ pic
                , elem
                ]
        }


mh : Int
mh =
    60


menuHeight : Attribute Msg
menuHeight =
    El.height <| El.px mh


fillSpace : Attribute Msg
fillSpace =
    El.batch
        [ El.width El.fill
        , El.height El.fill
        ]


fillWithRange : SizeRange -> Attribute Msg
fillWithRange size =
    El.batch
        [ El.width <| El.minimum size.minWidth <| El.maximum size.maxWidth El.fill
        , El.height <| El.minimum size.minHeight <| El.maximum size.maxHeight El.fill
        ]


padVal : number
padVal =
    20


spc : Attribute Msg
spc =
    El.spacingXY padVal <| round <| toFloat padVal * 1.5


shadow : Attribute Msg
shadow =
    Border.shadow
        { offset = ( 0, 3.0 )
        , size = 0
        , blur = 5.0
        , color = Color.rgba 0 0 0 0.5
        }



-- Svg Stuff --


svgIcon : Float -> String -> Color -> Element Msg
svgIcon f pathData color =
    el [ El.centerX, El.centerY ] <|
        El.html <|
            Ts.svg
                [ Ta.viewBox 0 0 35 35
                , Ta.width <| Tt.px <| f
                , Ta.height <| Tt.px <| f * 1
                , Ta.preserveAspectRatio
                    (Tt.Align Tt.ScaleMid Tt.ScaleMid)
                    Tt.Slice
                ]
                [ Ts.path
                    [ Ta.d pathData
                    , Ta.fill <|
                        Tt.Paint color
                    ]
                    []
                ]


svgWrap : SizeRange -> Element Msg -> Element Msg
svgWrap size elem =
    El.html <|
        Ts.svg
            [ Ta.viewBox 0
                0
                (toFloat size.minWidth)
                (toFloat size.minHeight)
            ]
            [ Tc.foreignObject
                [ Ta.width <| Tt.percent 100.0
                , Ta.height <| Tt.percent 100.0
                , Ta.preserveAspectRatio
                    (Tt.Align Tt.ScaleMin Tt.ScaleMin)
                    Tt.Meet
                ]
              <|
                [ El.layoutWith { options = [ El.noStaticStyleSheet ] }
                    [ fillSpace
                    ]
                  <|
                    el
                        [ centerXY
                        , El.padding 10
                        ]
                    <|
                        elem
                ]
            ]


gimbalabsPic : Element Msg
gimbalabsPic =
    El.image [ El.height <| El.px <| round <| toFloat mh * 0.65 ]
        { src = "assets/gimbalabs-logo.svg"
        , description = "Gimbalabs"
        }



-- 󰃭 Calendar --


calendarPic : Element Msg
calendarPic =
    El.image
        [ El.height <| El.px <| round <| toFloat mh * 0.5 ]
        { src = "assets/calendar.svg"
        , description = "Calendar"
        }



-- 󰎕 Blog --


blogPic : Element Msg
blogPic =
    El.image [ El.height <| El.px <| round <| toFloat mh * 0.5 ]
        { src = "assets/blog.svg"
        , description = "Blog"
        }



-- 󱠃 Solutions --


solutionsPic : Element Msg
solutionsPic =
    El.image [ El.height <| El.px <| round <| toFloat mh * 0.5 ]
        { src = "assets/solutions.svg"
        , description = "Solutions"
        }
