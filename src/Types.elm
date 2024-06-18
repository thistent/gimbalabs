module Types exposing (..)

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Date exposing (Date)
import Delay exposing (Delay)
import Dict exposing (Dict)
import Http
import Time
import Ui exposing (..)
import Ui.Region exposing (description)
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
    , hemisphere : Orientation
    , selectDate : Maybe Date
    , events : Array WeeklyEvent
    , fontSize : Float
    , dpi : Float
    }


type alias Flags =
    { dpi : Float }


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
    | Graph


type Menu
    = MenuClosed
    | MenuOpen


type alias Pal =
    { name : String
    , fg : Color
    , bg : Color
    , error : Color
    , link : Color
    , extLink : Color
    }



-- Events --


{-| every Wednesday 14:30-16:00 UTC
-}
type alias WeeklyEvent =
    { firstDate : Date
    , lastDate : Date
    , startTime : Int -- Time of the day UTC in minutes
    , duration : Int -- Length of event in minutes
    , exceptions : List Date
    , title : String
    , description : String
    , color : Color
    }


eventTimeString : Int -> Int -> String
eventTimeString startTime duration =
    let
        h1 =
            startTime
                // 60
                |> String.fromInt
                |> String.padLeft 2 '0'

        m1 =
            modBy 60 startTime
                |> String.fromInt
                |> String.padLeft 2 '0'

        h2 =
            (startTime + duration)
                // 60
                |> String.fromInt
                |> String.padLeft 2 '0'

        m2 =
            modBy 60 (startTime + duration)
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    h1 ++ ":" ++ m1 ++ "–" ++ h2 ++ ":" ++ m2 ++ " UTC"


eventsOfDay : Date -> Array WeeklyEvent -> List WeeklyEvent
eventsOfDay date allEvents =
    allEvents
        |> Array.filter
            (\ev ->
                (Date.weekdayNumber ev.firstDate == Date.weekdayNumber date)
                    && Date.isBetween ev.firstDate (Date.add Date.Days 1 ev.lastDate) date
                    && not (List.member date ev.exceptions)
            )
        |> Array.toList


dateRange : Date -> Date -> List Date
dateRange first last =
    Date.range Date.Day 7 first (Date.add Date.Days 1 last)


rangeToStringList : List Date -> List String
rangeToStringList range =
    range |> List.map (Date.format "y-MMMM-dd")



-- Misc --


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
