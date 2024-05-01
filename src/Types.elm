module Types exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Delay exposing (Delay)
import Dict exposing (Dict)
import Http
import Time
import Ui exposing (..)
import Url



-- Model --


type alias Model =
    { navKey : Nav.Key
    , url : Url.Url
    , page : Page
    , menu : Menu
    , color : Pal
    , size : Delay Dom.Viewport
    , zone : Time.Zone
    , time : Maybe Time.Posix
    , currentSlide : String
    , slides : Dict String (Pal -> Element Msg)
    , mdText : String
    }


type Msg
    = GotoPage Page
    | ChangeMenu Menu
    | ChangeColor Pal
    | ResetView
    | WindowResize Int Int
    | SceneInfo Dom.Viewport
    | NextSlide String
    | Tick Time.Posix
    | FrameDelta Float
    | AdjustTimeZone Time.Zone
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GetDoc String
    | ReceiveDoc String (Result Http.Error String)


type GameState
    = Stuff


type alias Form =
    Dict String FormMsg


type Page
    = Home
    | Calendar
    | Blog
    | Solutions


type Menu
    = MenuClosed
    | MainMenu
    | Settings
    | ThemePicker


type FormMsg
    = Field String
    | Timestamp Time.Posix


type alias Pal =
    { name : String
    , fg : Color
    , bg : Color
    , link : Color
    }


emptyViewport : Dom.Viewport
emptyViewport =
    { scene = { width = 0, height = 0 }
    , viewport =
        { x = 0
        , y = 0
        , width = 0
        , height = 0
        }
    }


corners :
    { topLeft : number
    , topRight : number
    , bottomLeft : number
    , bottomRight : number
    }
corners =
    { topLeft = 0
    , topRight = 0
    , bottomLeft = 0
    , bottomRight = 0
    }


edges : { left : number, right : number, top : number, bottom : number }
edges =
    { left = 0, right = 0, top = 0, bottom = 0 }
