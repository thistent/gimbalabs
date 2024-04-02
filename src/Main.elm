module Main exposing (..)

import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Color exposing (Color)
import Element.Region exposing (description)
import Html exposing (Html)
import Task
import Ui as El exposing (Element, el)
import Ui.Background as Bg
import Ui.Border as Border
import Ui.Font as Font



-- Types --


type alias Model =
    { window : Viewport }


type Msg
    = WindowSize Viewport
    | WindowResize Float Float
    | Tick Float



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
    ( { window = sizeToVp 0 0 }
    , Cmd.batch
        [ Task.perform WindowSize
            Dom.getViewport

        --, Http.get
        --    { url = "notes/" ++ startDoc
        --    , expect =
        --        Http.expectString <| GotDoc startDoc
        --    }
        ]
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
            let
                win =
                    model.window

                vp =
                    win.viewport

                nvp =
                    { vp | width = w, height = h }

                newWin =
                    { win | viewport = nvp }
            in
            ( { model | window = newWin }
            , Cmd.none
            )

        Tick _ ->
            ( model
            , Task.perform WindowSize
                Dom.getViewport
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
        , siteBg
        , Font.color black
        , El.inFront <|
            El.row
                [ El.width El.fill
                , menuHeight
                , Font.color white
                , El.paddingEach { edges | right = padVal, bottom = 5 }
                , Bg.color dark
                , spc
                , shadow
                ]
                [ El.row [ Font.size <| round <| toFloat mh * 0.6 ]
                    [ el
                        [ El.height El.fill
                        ]
                        gibmalabsPic
                    , El.text "GIMBA"
                    , el [ Font.bold ] <| El.text "LABS"
                    , El.text "  2024"
                    ]
                , el [ El.width El.fill ] El.none
                , menuButton [ El.alignRight ] calendarPic "Calendar"
                , menuButton [ El.alignRight ] blogPic "Blog"
                , menuButton [ El.alignRight ] solutionsPic "Solutions"
                ]
        ]
    <|
        el [ fillSpace, siteBg ] <|
            El.column
                [ fillSpace
                , El.paddingEach
                    { left = padVal + 5
                    , right = padVal + 5
                    , top = 0 -- padVal
                    , bottom = padVal + 5
                    }
                , spc
                , siteBg
                ]
                [ el
                    [ El.width El.fill
                    , menuHeight
                    ]
                    El.none
                , El.row [ El.width El.fill, El.height <| El.px 400, spc ]
                    [ El.column
                        [ El.width <| El.px 200
                        , El.height El.fill
                        , siteBg
                        , spc
                        ]
                        [ el
                            [ fillSpace
                            , Bg.color blue
                            , shadow
                            , El.rotate -0.01
                            ]
                          <|
                            displayViewport model.window
                        , el
                            [ fillSpace
                            , cellBg
                            , shadow
                            , rotRight
                            , El.padding 5
                            ]
                          <|
                            El.column
                                [ centerXY
                                , El.moveUp 5
                                ]
                                [ El.image [ El.centerX ]
                                    { src = "../assets/github.svg"
                                    , description = "GitHub"
                                    }
                                , El.paragraph
                                    [ Font.center
                                    ]
                                    [ El.text "View source on GitHub"
                                    ]
                                ]
                        ]
                    , El.image
                        [ El.height <| El.px 400
                        , rotLeft
                        , shadow
                        ]
                        { src = "../assets/open-spaces-card-1.webp"
                        , description = "Gimbalabs Open Spaces. Wednesday and Thursday at 14:30 UTC. Click for more details."
                        }
                    , add
                    , El.column [ fillSpace, spc ]
                        [ add
                        , El.column
                            [ fillSpace
                            , Bg.color yellow
                            , Font.size 16
                            , Font.center
                            , El.padding <| round <| padVal * 1.5
                            , rotRight
                            , shadow
                            , spc
                            ]
                            [ el
                                [ Font.size 30
                                , Font.center
                                , centerXY
                                ]
                              <|
                                El.text "Welcome!"
                            , El.paragraph [ centerXY ]
                                [ El.text "Right now, we are building Plutus PBL 2024, running weekly live coding sessions, and hosting Gimbalabs Open Spaces."
                                ]
                            ]
                        ]
                    ]
                , El.row [ fillSpace, spc ]
                    [ el
                        [ fillSpace
                        , Bg.color teal
                        , El.padding padVal
                        , rotRight
                        , shadow
                        ]
                      <|
                        El.column [ centerXY, spc ]
                            [ el
                                [ Border.widthEach { edges | bottom = 1 }
                                , El.padding 10
                                , Font.size 30
                                ]
                              <|
                                El.text "Learn"
                            , El.column [ El.centerX, spc ]
                                [ el [] <| El.text "⯀ Starter Kits"
                                , el [] <| El.text "⯀ Plutus"
                                , el [] <| El.text "⯀ Playground"
                                ]
                            ]
                    , el
                        [ fillSpace
                        , Bg.color orange
                        , El.padding padVal
                        , shadow
                        ]
                      <|
                        El.column [ centerXY, spc ]
                            [ el
                                [ Border.widthEach { edges | bottom = 1 }
                                , El.padding 10
                                , Font.size 30
                                ]
                              <|
                                El.text "APIs"
                            , El.column [ El.centerX, spc ]
                                [ el [] <| El.text "⯀ Dandelion"
                                , el [] <| El.text "⯀ Endpoints"
                                ]
                            ]
                    , el
                        [ fillSpace
                        , Bg.color green
                        , El.padding padVal
                        , rotLeft
                        , shadow
                        ]
                      <|
                        El.column [ centerXY, spc ]
                            [ el
                                [ Border.widthEach { edges | bottom = 1 }
                                , El.padding 10
                                , Font.size 30
                                ]
                              <|
                                El.text "Updates"
                            , El.column [ El.centerX, spc ]
                                [ el [] <| El.text "⯀ Updates"
                                ]
                            ]
                    , el
                        [ fillSpace
                        , Bg.color lavender
                        , El.padding padVal
                        , rotRight
                        , shadow
                        ]
                      <|
                        El.column [ centerXY, spc ]
                            [ el
                                [ Border.widthEach { edges | bottom = 1 }
                                , El.padding 10
                                , Font.size 30
                                ]
                              <|
                                El.text "About Us"
                            , El.column [ El.centerX, spc ]
                                [ el [] <| El.text "⯀ Team"
                                , el [] <| El.text "⯀ Calendar"
                                , el [] <| El.text "⯀ Stake Pools"
                                ]
                            ]
                    ]
                ]


centerXY : El.Attribute Msg
centerXY =
    El.batch [ El.centerX, El.centerY ]


displayViewport : Viewport -> Element Msg
displayViewport vp =
    let
        sw =
            String.fromFloat vp.scene.width

        sh =
            String.fromFloat vp.scene.height

        vx =
            String.fromFloat vp.viewport.x

        vy =
            String.fromFloat vp.viewport.y

        vw =
            String.fromFloat vp.viewport.width

        vh =
            String.fromFloat vp.viewport.height
    in
    el [ fillSpace ] <|
        El.column
            [ centerXY
            , Font.size 12
            , El.spacing 3
            ]
            [ El.text <| "{ scene ="
            , El.text <| "    { width = " ++ sw
            , El.text <| "    , height = " ++ sh
            , El.text <| "    }"
            , El.text <| ", viewport ="
            , El.text <| "    { x = " ++ vx
            , El.text <| "    , y = " ++ vy
            , El.text <| "    , width = " ++ vw
            , El.text <| "    , height = " ++ vh
            , El.text <| "    }"
            , El.text <| "}"
            ]


add : Element Msg
add =
    el
        [ fillSpace
        , Border.width 4
        , Border.dashed
        , Border.color addColor
        ]
    <|
        El.column [ centerXY, Font.color addColor ]
            [ El.image [ El.height <| El.px 60, El.centerX ]
                { src = "../assets/add.svg"
                , description = "Add"
                }
            , el [ Font.center, Font.bold ] <| El.text "Add Item"
            ]



-- Colors --


black : Color
black =
    Color.rgb 0 0 0


white : Color
white =
    Color.rgb 1 1 1


yellow : Color
yellow =
    -- #F0D080 #Fff0a0
    Color.rgb255 0xFF 0xFF 0xA0


orange : Color
orange =
    -- #FFeeaa
    Color.rgb255 0xFF 0xDD 0xAA


green : Color
green =
    -- #AAFF88#bbffcc
    Color.rgb255 0xBB 0xFF 0xCC


lavender : Color
lavender =
    -- #EEddFF
    Color.rgb255 0xEE 0xDD 0xFF


dark : Color
dark =
    -- #05131e
    Color.rgb255 0x05 0x13 0x1E


blue : Color
blue =
    -- #70B8F0 #88ddff
    Color.rgb255 0x88 0xDD 0xFF


teal : Color
teal =
    -- #65e3dd #88ffee
    Color.rgb255 0x88 0xFF 0xEE


sky : Color
sky =
    -- #dbf4f4
    Color.rgb255 0xDB 0xF4 0xF4



-- Other Stuff --


rotLeft : El.Attribute Msg
rotLeft =
    El.rotate -0.02


rotRight : El.Attribute Msg
rotRight =
    El.rotate 0.04


edges : { left : Int, right : Int, top : Int, bottom : Int }
edges =
    { left = 0, right = 0, top = 0, bottom = 0 }


menuButton : List (El.Attribute Msg) -> Element Msg -> String -> Element Msg
menuButton attrs pic str =
    el
        [ El.height El.fill
        , El.batch attrs
        ]
    <|
        el
            [ centerXY
            , El.padding 10
            ]
        <|
            El.row
                [ Font.size <| round <| toFloat mh * 0.5
                , Font.center
                , El.height El.fill
                ]
                [ pic
                , el [ El.centerY ] <| El.text str
                ]


gibmalabsPic : Element Msg
gibmalabsPic =
    El.image [ El.height <| El.px <| mh ]
        { src = "../assets/gimbalabs-logo.svg"
        , description = "Gimbalabs"
        }



-- 󰃭 Calendar --


calendarPic : Element Msg
calendarPic =
    El.image [ El.height <| El.px <| mh - 20 ]
        { src = "../assets/calendar.svg"
        , description = "Calendar"
        }



-- 󰎕 Blog --


blogPic : Element Msg
blogPic =
    El.image [ El.height <| El.px <| mh - 20 ]
        { src = "../assets/blog.svg"
        , description = "Blog"
        }



-- 󱠃 Solutions --


solutionsPic : Element Msg
solutionsPic =
    El.image [ El.height <| El.px <| mh - 20 ]
        { src = "../assets/solutions.svg"
        , description = "Solutions"
        }


mh : Int
mh =
    60


menuHeight : El.Attribute Msg
menuHeight =
    El.height <| El.px mh


siteBg : El.Attribute Msg
siteBg =
    -- #A0B0C0
    Bg.color <| Color.rgb255 0xA0 0xB0 0xC0


addColor : Color
addColor =
    -- #8899AA
    Color.rgb255 0x88 0x99 0xAA


cellBg : El.Attribute Msg
cellBg =
    -- #dbf4f4
    Bg.color <| Color.rgb255 0xDB 0xF4 0xF4


fillSpace : El.Attribute Msg
fillSpace =
    El.batch
        [ El.width El.fill
        , El.height El.fill
        ]


padVal : number
padVal =
    20


spc : El.Attribute Msg
spc =
    El.spacing padVal


pad : El.Attribute Msg
pad =
    El.padding padVal


shadow : El.Attribute Msg
shadow =
    Border.shadow
        { offset = ( 0, 3.0 )
        , size = 0
        , blur = 5.0
        , color = Color.rgba 0 0 0 0.5
        }
