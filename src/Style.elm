module Style exposing (..)

import Html.Attributes as Attr
import Pic exposing (addAlpha)
import Types exposing (..)
import Ui exposing (..)
import Ui.Border as Border



-- Size Values --


spcNum : number
spcNum =
    40


lineSize : number
lineSize =
    2


fontSize : number
fontSize =
    20



--Color Schemes --


black : Color
black =
    rgb 0 0 0


newspaper : Pal
newspaper =
    { name = "Newspaper"
    , fg = rgb255 0x2B 0x16 0x00
    , bg = rgb255 0x9B 0x99 0x94
    , link = rgb255 0x00 0x4D 0x1F

    -- #9b9994#918f8b
    -- #003370 #004d1f #961d00
    }


blueprint : Pal
blueprint =
    { name = "Blueprint"
    , fg = rgb255 0x70 0xB1 0xD8
    , bg = rgb255 0x02 0x25 0x49
    , link = rgb255 0xDD 0xE8 0xEC
    }


term : Pal
term =
    { name = "Terminal"
    , fg = rgb255 0x54 0xAE 0x10
    , bg = rgb255 0x00 0x36 0x18
    , link = rgb255 0xEB 0xFF 0x5F
    }


dark : Pal
dark =
    { name = "Dark"
    , fg = rgb255 0xA1 0xA0 0x9F
    , bg = rgb255 0x1A 0x1C 0x1F
    , link = rgb255 0xC9 0x99 0x23
    }


blueNote : Pal
blueNote =
    { name = "Blue-Note"
    , fg = rgb255 0x28 0x36 0x40
    , bg = rgb255 0x7E 0xB3 0xC2
    , link = rgb255 0x44 0x89 0xBF
    }


greenNote : Pal
greenNote =
    { name = "Green-Note"
    , fg = rgb255 0x2C 0x3A 0x16
    , bg = rgb255 0x86 0xB9 0x61
    , link = rgb255 0x4F 0x93 0x43
    }


yellowNote : Pal
yellowNote =
    { name = "Yellow-Note"
    , fg = rgb255 0x48 0x33 0x0C
    , bg = rgb255 0xBE 0xAA 0x52
    , link = rgb255 0xA3 0x7E 0x24
    }


orangeNote : Pal
orangeNote =
    { name = "Orange-Note"
    , fg = rgb255 0x53 0x2D 0x0F
    , bg = rgb255 0xDB 0x9F 0x61
    , link = rgb255 0xC6 0x6C 0x2C
    }


redNote : Pal
redNote =
    { name = "Red-Note"
    , fg = rgb255 0x57 0x29 0x23
    , bg = rgb255 0xD8 0x9C 0x9B
    , link = rgb255 0xD0 0x61 0x6A
    }


purpleNote : Pal
purpleNote =
    { name = "Purple-Note"
    , fg = rgb255 0x42 0x30 0x40
    , bg = rgb255 0xB4 0xA5 0xC8
    , link = rgb255 0x93 0x76 0xC1
    }



-- Attributes --


otherSide : Element Msg -> Attribute Msg
otherSide content =
    behindContent <|
        el
            [ fillSpace
            , style "-moz-transform" "scale(1, -1)"
            , style "-webkit-transform" "scale(1, -1)"
            , style "-o-transform" "scale(1, -1)"
            , style "-ms-transform" "scale(1, -1)"
            , style "transform" "scale(1, -1)"
            , style "pointer-events" "none"
            ]
        <|
            content


shadow : Attribute Msg
shadow =
    Border.shadow
        { offset = ( 1.0, 2.0 )
        , size = 0
        , blur = 5.0
        , color = addAlpha 0.5 black
        }


fillSpace : Attribute Msg
fillSpace =
    batch
        [ width fill
        , height fill
        ]


centerXY : Attribute Msg
centerXY =
    batch
        [ centerX
        , centerY
        ]


colNumGap : Int -> Int -> Attribute Msg
colNumGap count width =
    batch
        [ style "columns" <|
            "300px "
                ++ String.fromInt count
        , style "column-gap" <|
            String.fromInt width
                ++ "px"
        ]


selectable : Bool -> Attribute Msg
selectable b =
    let
        none =
            if b then
                "auto"

            else
                "none"
    in
    batch
        [ style "-webkit-user-select" none
        , style "-khtml-user-select" none
        , style "-webkit-touch-callouT" none
        , style "-moz-user-select" none
        , style "-o-user-select" none
        , style "user-select" none
        ]


style : String -> String -> Attribute Msg
style s t =
    htmlAttribute <| Attr.style s t