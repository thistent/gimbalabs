module Types exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Date exposing (Date)
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
    , pal : Pal
    , size : Delay Dom.Viewport
    , zone : Time.Zone
    , time : Maybe Time.Posix
    , currentSlide : String
    , slides : Dict String (Pal -> Element Msg)
    , docText : String
    , docName : String
    , clock : Orientation
    , selectDate : Maybe Date
    , dpi : Float
    , fontSize : Float
    }


type Orientation
    = North
    | South


type Msg
    = GotoPage Page
    | ChangeMenu Menu
    | ChangeColor Pal
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
    | ToggleClockOrientation
    | SelectDate (Maybe Date)


type Page
    = Home
    | Calendar
    | Blog
    | Solutions
    | Settings


type Menu
    = MenuClosed
    | MainMenu


type alias Pal =
    { name : String
    , fg : Color
    , bg : Color
    , error : Color
    , link : Color
    , extLink : Color
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
