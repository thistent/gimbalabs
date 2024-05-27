module Pic exposing (..)

--import TypedSvg.Filters as Tf

import Color
import Style exposing (fontSize)
import TypedSvg as Ts
import TypedSvg.Attributes as Ta
import TypedSvg.Core as Tc
import TypedSvg.Types as Tt
import Types exposing (..)
import Ui exposing (..)
import Ui.Background as Bg
import Ui.Events as Ev


gimbalogo : Color -> Color -> Element msg
gimbalogo link bg =
    el
        [ Bg.color link
        , height <| px <| fontSize * 3
        , paddingXY 0 5
        ]
    <|
        svg
            [ Ta.viewBox 0 0 15.236 26.458
            , Ta.width <| Tt.px 57.587
            , Ta.height <| Tt.px 100
            ]
            [ Ts.path
                [ Ta.d gimbalogoPath
                , Ta.fill <| Tt.Paint bg
                ]
                []
            ]


menuClosed : Color -> msg -> Element msg
menuClosed color msg =
    link
        [ Ev.onClick msg
        , alignRight
        ]
        { url = ""
        , label =
            svg
                [ Ta.viewBox 0 0 26.458 26.458
                , Ta.height <| Tt.px <| fontSize * 1.7
                ]
                [ Ts.path
                    [ Ta.d menuClosedPath
                    , Ta.fill <| Tt.Paint color
                    ]
                    []
                ]
        }


menuOpen : Color -> msg -> Element msg
menuOpen color msg =
    link
        [ Ev.onClick msg
        , alignRight
        ]
        { url = ""
        , label =
            svg
                [ Ta.viewBox 0 0 26.458 26.458
                , Ta.height <| Tt.px <| fontSize * 1.7
                ]
                [ Ts.path
                    [ Ta.d menuOpenPath
                    , Ta.fill <| Tt.Paint color
                    ]
                    []
                ]
        }


pageCurl : Pal -> Element msg
pageCurl c =
    let
        fg : Color
        fg =
            c.fg

        bg : Color
        bg =
            c.bg

        gid : String
        gid =
            c.name ++ "-grad"
    in
    svg
        [ Ta.viewBox 0 0 26.458 26.458
        , Ta.width <| Tt.px <| fontSize * 2
        , Ta.height <| Tt.px <| fontSize * 2
        ]
        [ Ts.defs []
            [ Ts.linearGradient
                [ Ta.id gid
                , Ta.x1 <| Tt.percent 0
                , Ta.y1 <| Tt.percent 0
                , Ta.x2 <| Tt.percent 100
                , Ta.y2 <| Tt.percent 100
                ]
                [ Ts.stop
                    [ Ta.offset "55%"
                    , Ta.stopColor <|
                        Color.toCssString <|
                            Style.mix 0.9 fg bg
                    ]
                    []
                , Ts.stop
                    [ Ta.offset "100%"
                    , Ta.stopColor <|
                        Color.toCssString <|
                            Style.mix 0.1 fg bg
                    ]
                    []
                ]
            ]
        , Ts.path
            [ Ta.d curlFront
            , Ta.fill <| Tt.Paint bg
            ]
            []
        , Ts.path
            [ Ta.d curlShadow
            , Ta.fill <|
                Tt.Paint <|
                    rgba 0 0 0 0.35
            ]
            []
        , Ts.path
            [ Ta.d curlBack
            , Ta.fill <|
                Tt.Reference gid
            ]
            []
        ]


home : Color -> Element msg
home color =
    svg
        [ Ta.viewBox 0 0 26.213 26.458
        , Ta.height <| Tt.px <| fontSize * 1.7
        ]
        [ Ts.path
            [ Ta.d homePath
            , Ta.fill <| Tt.Paint color
            ]
            []
        ]


calendar : Color -> Element msg
calendar color =
    svg
        [ Ta.viewBox 0 0 23.64 26.458
        , Ta.height <| Tt.px <| fontSize * 1.7
        ]
        [ Ts.path
            [ Ta.d calendarPath
            , Ta.fill <| Tt.Paint color
            ]
            []
        ]


blog : Color -> Element msg
blog color =
    svg
        [ Ta.viewBox 0 0 23.657 26.458
        , Ta.height <| Tt.px <| fontSize * 1.7
        ]
        [ Ts.path
            [ Ta.d blogPath
            , Ta.fill <| Tt.Paint color
            ]
            []
        ]


solutions : Color -> Element msg
solutions color =
    svg
        [ Ta.viewBox 0 0 19.59 26.458
        , Ta.height <| Tt.px <| fontSize * 1.7
        ]
        [ Ts.path
            [ Ta.d solutionsPath
            , Ta.fill <| Tt.Paint color
            ]
            []
        ]


pal : Color -> Element msg
pal color =
    svg
        [ Ta.viewBox 0 0 26.458 26.458
        , Ta.height <| Tt.px <| fontSize * 1.7
        ]
        [ Ts.path
            [ Ta.d palPath
            , Ta.fill <| Tt.Paint color
            ]
            []
        ]


loc : Color -> Element msg
loc color =
    svg
        [ Ta.viewBox 0 0 22.933 26.458
        , Ta.height <| Tt.px <| fontSize * 1.7
        ]
        [ Ts.path
            [ Ta.d locPath
            , Ta.fill <| Tt.Paint color
            ]
            []
        ]


bullet : Color -> Element msg
bullet color =
    svg
        [ Ta.viewBox 0 0 26.458 26.458
        , Ta.height <| Tt.px <| fontSize * 1.7
        ]
        [ Ts.path
            [ Ta.d bulletPath
            , Ta.fill <| Tt.Paint color
            ]
            []
        ]


settings : Color -> Element msg
settings color =
    svg
        [ Ta.viewBox 0 0 26.458 26.458
        , Ta.height <| Tt.px <| fontSize * 1.7
        ]
        [ Ts.path
            [ Ta.d settingsPath
            , Ta.fill <| Tt.Paint color
            ]
            []
        ]


dark : Color -> Element msg
dark color =
    svg
        [ Ta.viewBox 0 0 26.458 26.458
        , Ta.height <| Tt.px <| fontSize * 1.7
        ]
        [ Ts.path
            [ Ta.d darkPath
            , Ta.fill <| Tt.Paint color
            ]
            []
        ]


light : Color -> Element msg
light color =
    svg
        [ Ta.viewBox 0 0 26.458 26.458
        , Ta.height <| Tt.px <| fontSize * 1.7
        ]
        [ Ts.path
            [ Ta.d lightPath
            , Ta.fill <| Tt.Paint color
            ]
            []
        ]



{-
   dims : Color -> Color -> Element msg
   dims link bg =
       el
           [ Bg.color link
           , height <| px <| fontSize * 3
           , paddingXY 0 5
           , centerX
           ]
       <|
           svg
               [ Ta.viewBox 0 0 23.492 26.458
               , Ta.width <| Tt.px 57.587
               , Ta.height <| Tt.px 100
               ]
               [ Ts.path
                   [ Ta.d dimsPath
                   , Ta.fill <| Tt.Paint bg
                   ]
                   []
               ]
-}
-- Misc Functions --


svg : List (Tc.Attribute msg) -> List (Tc.Svg msg) -> Element msg
svg svgAttrs svgElems =
    html <|
        Ts.svg svgAttrs svgElems



-- Svg Paths --


gimbalogoPath : String
gimbalogoPath =
    "m12.506-5.16e-5c-1.0293 0.49442-1.472 1.835-2.5571 2.1537-1.9544-0.039688-3.9131-0.027946-5.8692-0.046709-1.1367 2.267-2.872 4.1608-4.0793 6.386 1.5446 2.1403 2.68 4.5378 4.15 6.7263 2.2414 0.013 4.4827 0.026 6.7242 0.03894 0.86787 1.5851 1.7358 3.1704 2.6035 4.7555-0.89363 1.7207-1.7873 3.4413-2.681 5.162-1.9152-0.01116-3.8303-0.02219-5.7455-0.03327-0.75505-0.51591-1.8186-1.8333-2.4001-1.8254 0.36292 1.0212 0.93988 1.9644 1.4243 2.9251 0.45654 0.37361 1.1023 0.09618 1.6545 0.18136 1.986 0.01152 3.9723 0.02306 5.9585 0.03449 1.1772-2.1801 2.6605-4.2056 3.5479-6.5341-1.0056-2.0616-2.0515-4.103-3.0729-6.1567-0.13609-0.80284 0.44097-2.5666-0.13232-2.7775-0.73114 0.70718-1.0814 2.1016-2.2086 2.2341-1.6392-0.0095-3.2785-0.01896-4.9175-0.02843-0.93021-1.6182-1.8604-3.2363-2.7907-4.8547 0.96335-1.7259 1.9267-3.4522 2.8901-5.1781 1.9561 0.011321 3.9121 0.022662 5.8683 0.033973 0.94943 1.5578 1.8988 3.1152 2.8483 4.6729-0.26047 0.61027-1.3392 1.8534-1.0077 2.0883 0.74242-0.69001 2.8028-1.4062 1.8361-2.6342-0.83496-1.5881-1.6696-3.1762-2.5046-4.7644 0.06555-0.79786 1.1367-1.8864 0.46097-2.5592z"


menuClosedPath : String
menuClosedPath =
    "m-1.2495e-4 0v3.3329h26.458v-3.3329zm2.499e-4 11.601h26.457v3.2559h-26.457zm0 11.524h26.457v3.3334h-26.457z"


menuOpenPath : String
menuOpenPath =
    "m26.458-1.67e-4 -12.314 13.229 12.314 13.23v-6.141l-6.5973-7.0882 6.5973-7.0882v-6.14zm-26.458 1.67e-4v3.3326h17.667l3.1434-3.3326zm0 11.601v3.2561h9.8693l-1.5353-1.6278 1.5358-1.6283zm0 11.524v3.3331h20.81l-3.1434-3.3331z"


curlFront : String
curlFront =
    "m0 0h26.458c-0.41842 13.945-12.127 26.009-26.458 26.458z"


curlShadow : String
curlShadow =
    "m26.458 0c-2.303 14.427-14.426 9.03-19.001 10.532 7.0451 3.973 1.0454 15.07-6.6212 15.879 13.303-0.84084 25.622-15.264 25.622-26.41zm-26.447 26.458c-0.0036 5e-6 -0.0072 5.17e-4 -0.01085 5.17e-4 0.01284 0 0.02541-5.01e-4 0.03824-5.17e-4z"


curlBack : String
curlBack =
    "m26.458 0c-1.0917 12.278-9.6479 8.1027-16.072 6.1065 6.2199 12.299-1.9433 19.672-9.5574 20.304 12.905 0 25.629-9.8477 25.629-26.41zm-26.449 26.458c-0.0032 8.5e-5 -0.0066 4.35e-4 -0.0098 5.17e-4 0.01284 0 0.02541-5.01e-4 0.03824-5.17e-4z"


homePath : String
homePath =
    "m13.123 3.8715e-7 -0.0021 0.0022-0.0021-0.0022-13.119 11.992v1.319h2.2502l0.00257 13.147h9.0395v-9.4503h3.6572v9.4503h9.0395l0.0029-13.147h2.2211v-1.319l-3.9856-3.696v-3.8915h-2.0845v1.9659zm-0.0021 2.8345 8.8665 8.0728v13.632h-4.9329v-9.7701h-7.9062v9.7701h-4.894v-13.632z"


calendarPath : String
calendarPath =
    "m3.8891 9.785e-5v2.6646h-1.3012c-0.74354 0-1.3737 0.25818-1.89 0.77451-0.51636 0.51634-0.69794 1.1458-0.69793 1.8893v18.465c0 0.74354 0.18159 1.3737 0.69793 1.89 0.51634 0.51637 1.2231 0.77452 1.9666 0.77451h18.311c0.74354 0 1.3737-0.26373 1.89-0.7904 0.51633-0.51634 0.77451-1.1409 0.77451-1.8742v-18.465c0-0.74354-0.1816-1.373-0.69793-1.8893-0.51634-0.51634-1.1465-0.77451-1.89-0.77451h-1.3012v-2.6646h-2.3936v2.6646h-11.076v-2.6646h-2.3929zm-2.192 6.2583h20.246v16.415c0 1.2175-0.08533 1.9248-1.1842 1.9247h-17.878c-1.0988 2.5e-5 -1.1849-0.70722-1.1849-1.9247v-16.415zm9.9315 7.4699v8.6373h8.068v-8.6373z"


blogPath : String
blogPath =
    "m22.042 26.458c1.1524 0 1.6157-1.7703 1.6157-3.1285v-22.639c-0.49399 0.64093-0.98859 1.2811-1.482 1.9229-0.70743-0.86788-1.411-1.7407-2.1168-2.6107-0.67272 0.87194-1.3466 1.7425-2.0183 2.6156-0.69437-0.86978-1.3853-1.7439-2.0781-2.6156-0.68494 0.87033-1.3708 1.7396-2.055 2.6107-0.69144-0.87106-1.3829-1.7421-2.0744-2.6131-0.56794 0.71548-1.1359 1.431-1.7039 2.1465l-6.16e-4 1.4e-4c-0.4128 0.98165-0.81853-0.27184-1.2283-0.62058-0.42376-0.40916-0.84672-1.3243-1.271-1.4355-0.6694 0.84258-1.3392 1.6847-2.0083 2.5277-0.67148-0.87268-1.3443-1.7437-2.0164-2.6156-0.70572 0.87009-1.4093 1.7428-2.1168 2.6107-0.4935-0.64231-0.98921-1.2819-1.4838-1.9229 0.016199 7.8939-0.03251 15.791 0.024403 23.684 0.13698 1.427 1.3248 2.0839 2.4069 2.0839m-0.021337-20.232h19.549v5.992h-20.209v-5.992zm0 7.7423h9.392v10.667h-10.051v-10.667zm11.801 0h7.7483v4.4583h-8.4072v-4.4583zm0 6.2813h7.7483v4.3853c-4.0912 0-4.316 1.3e-5 -8.4072 0v-4.3853z"


solutionsPath : String
solutionsPath =
    "m9.8581 1.8845c4.2437-0.053247 8.0943 3.8087 7.8697 8.0823 0.0077 2.6696-1.5024 5.1269-3.6132 6.6799-1.4561 0.76112-1.4486 3.1146-2.783 3.5365-1.5545-0.18354-2.079 0.17442-3.6258-0.14291-0.23694-2.2657-2.5672-3.6644-4.0521-5.1761-2.7918-3.201-2.2281-8.6378 1.174-11.197 1.4065-1.1525 3.2074-1.8124 5.0304-1.7829zm-0.38564-1.8607c-5.3155-0.067914-9.972 5.0579-9.4293 10.344 0.12472 3.0824 1.9241 5.9031 4.3886 7.6791 1.622 1.5204 0.86975 3.879 1.2233 5.8003 0.63864 1.1201 2.3221 0.92469 2.9241 2.2799 1.2116 0.77183 2.9467 0.11757 3.5021-1.1714 1.7546-0.14172 2.3661-1.4707 1.9908-3.0465-0.4007-2.1117 0.95018-3.8085 2.469-5.0803 2.2752-2.0179 3.3433-5.1314 2.9793-8.1244-0.32898-4.9207-5.1307-9.0588-10.048-8.6807zm-1.4227 5.0268c-0.97743 0.48855-0.29402 2.6587-2.1078 2.246-2.0944 0.47823 1.098 1.9028-0.246 3.1912-1.421 1.1406 0.43187 1.5576 1.4053 1.7432 0.43369 1.0487 0.52218 2.9053 1.9543 1.4087 1.1689-1.1994 2.6519 1.8481 3.1084-0.20359-0.75694-1.9652 3.8488-1.0826 1.7085-3.0801-0.97455-1.1205 1.9187-3.3182-0.78562-3.0521-1.4285-0.28787-0.53286-3.3928-2.3383-1.8852-1.0392 1.3614-1.8127-0.51766-2.6987-0.36809zm1.6647 2.7825c1.8153-0.20883 2.6255 2.5443 0.99169 3.3774-1.6058 0.99594-3.5475-1.228-2.3468-2.6864 0.3094-0.42572 0.82541-0.7058 1.3551-0.69108z"


palPath : String
palPath =
    "m13.185 0.058993c-6.298-0.087258-12.225 4.9647-13.035 11.231-0.1585 1.3411-0.35287 2.7966 0.17566 4.0756 1.0256 1.6996 3.5858 1.5421 4.9555 0.36213 1.0334-1.0389 2.635-1.5975 4.0241-0.94473 1.9435 0.79639 2.9932 3.5302 1.5482 5.2339-0.89391 0.97327-1.6282 2.208-1.4609 3.5791 0.070066 1.5108 1.3205 2.9548 2.9092 2.8474 5.5019 0.43464 11.03-2.9835 13.113-8.093 2.054-4.6581 1.0465-10.468-2.5257-14.115-2.4449-2.6284-5.9763-4.246-9.5664-4.2606zm0 24.586c-1.6207 0.55467-2.5701-1.2912-1.5886-2.4911 0.68508-1.0037 1.6044-1.9228 1.7221-3.2005 0.67353-3.188-2.122-6.5852-5.4078-6.3923-1.5473 0.034529-3.0221 0.79716-4.0235 1.963-1.3255 1.0452-2.495-0.61746-1.9481-1.8991 0.31238-5.9952 6.0632-11.153 12.084-10.615 5.6868 0.34252 10.644 5.5301 10.546 11.263 0.08485 5.0631-3.6209 9.9419-8.5942 11.052-0.912 0.22245-1.8519 0.32884-2.7905 0.32052zm1.9003-18.907c0.13483 1.9206-2.895 2.6099-3.6258 0.82438-0.82134-1.565 1.0369-3.4567 2.5912-2.5586 0.63731 0.31905 1.0819 1.0129 1.0346 1.7342zm5.6422 13.171c0.16042 1.7787-2.4934 2.6554-3.4587 1.1636-1.0404-1.3857 0.60688-3.6217 2.2278-2.8526 0.67873 0.27139 1.2184 0.94314 1.2309 1.689zm-13.185-9.3654c1.6128 0.12868 2.4868-2.025 1.4176-3.164-1.1399-1.4331-3.7286-0.24546-3.2942 1.5691 0.14141 0.88858 0.94354 1.6678 1.8766 1.5949zm13.185-1.9321c0.1604 1.771-2.4851 2.6558-3.4591 1.1838-0.9969-1.3364 0.4365-3.5866 2.0832-2.9886 0.78271 0.21693 1.4043 0.97949 1.3759 1.8048zm1.9003 5.6782c0.13482 1.9206-2.8949 2.6099-3.6258 0.82438-0.82133-1.5649 1.0369-3.4567 2.5912-2.5586 0.6373 0.31905 1.0819 1.0129 1.0346 1.7342z"


settingsPath : String
settingsPath =
    "m3.8787 16.017c1.3875-0.0041 3.8165-0.21631 3.8137-1.5986-0.0027881-1.3823-2.4285-1.5764-3.8137-1.5805-1.3852-0.0041-3.8759 0.17804-3.8787 1.5804-0.0027881 1.4024 2.4912 1.6028 3.8787 1.5987zm0.9168-16.017h-1.8986v10.547h1.8986zm9.3313 0h-1.7956v2.8911h1.7956zm-1.7956 10.652h1.7956v15.806h-1.7956zm-7.5357 7.6556h-1.8986v8.1503h1.8986zm8.466-9.9466c1.3875-0.0041 3.8165-0.21631 3.8137-1.5986-0.0028-1.3823-2.4285-1.5764-3.8137-1.5805-1.3852-0.0041-3.8759 0.17804-3.8787 1.5804-0.00279 1.4024 2.4912 1.6028 3.8787 1.5987zm10.3-8.3611h-1.8986v14.885h1.8986zm-1.8986 22.646h1.8986v3.8116h-1.8986zm-16.867-22.646h-1.8986v10.547h1.8986zm9.3313 0h-1.7956v2.8911h1.7956zm-1.7956 10.652h1.7956v15.806h-1.7956zm-7.5357 7.6556h-1.8986v8.1503h1.8986zm17.849 2.0477c1.3875-0.0041 3.8165-0.21631 3.8137-1.5986-0.0028-1.3823-2.4285-1.5764-3.8137-1.5805-1.3852-0.0041-3.8759 0.17804-3.8787 1.5804-0.0028 1.4024 2.4912 1.6028 3.8787 1.5987zm0.9168-20.355h-1.8986v14.885h1.8986zm-1.8986 22.646h1.8986v3.8116h-1.8986z"



{-
   dimsPath : String
   dimsPath =
       "m5.5428 10.345 5.942e-4 -3e-6c-1.507e-4 0.03436 7.897e-4 0.06884 0.00269 0.10329 6.94e-4 0.01403 0.00242 0.02821 0.00369 0.04241-7.298e-4 0.0575-2.723e-4 0.11482 0.00113 0.17195 0.00692 1.0715 1.1832 1.8887 2.1916 1.6422 0.059303-0.01287 0.11738-0.02915 0.17418-0.04833 0.19599 0.12367 0.36714 0.28644 0.55243 0.42754 0.11345 0.0913 0.23436 0.17602 0.36319 0.24485 0.13239 0.07695 0.27364 0.13661 0.42622 0.16344 0.05945 0.01327 0.12165 0.02525 0.18283 0.03942 0.02931 0.0075 0.0585 0.01536 0.08782 0.02302 0.08773 0.02655 0.17034 0.06456 0.23804 0.1294 0.04094 0.04338 0.06631 0.09541 0.0787 0.1514 0.0012 0.0061 0.0023 0.01192 0.0036 0.01791 0.02167 0.12764-0.02386 0.2698-0.11709 0.35975-0.03268 0.02352-0.06523 0.047-0.09769 0.07076-0.075 0.05188-0.15094 0.10311-0.22643 0.15417-0.1639 0.10984-0.34633 0.20887-0.54014 0.25248-0.06661 0.01335-0.13461 0.02029-0.20413 0.01854-0.0546-9.77e-4 -0.10864-0.0073-0.16186-0.01739-0.26886-0.23091-0.60221-0.38316-0.9667-0.4071-0.97498-0.0979-1.9974 0.71351-1.9685 1.7263-0.00345 0.06715-1.815e-4 0.13404 6.248e-4 0.2012 0.00741 0.93164 0.88995 1.6896 1.8 1.6726 0.96811 0.0391 1.8349-0.85028 1.7938-1.8135-2e-3 -0.15104-0.02489-0.30087-0.06529-0.44579 0.0055-0.01914 0.01163-0.03836 0.01891-0.05797 0.035-0.08316 0.08586-0.15597 0.14618-0.22198 0.03289-0.02805 0.06761-0.05369 0.10426-0.07497 0.16071-0.10919 0.32127-0.22715 0.49156-0.32218 0.08556-0.04126 0.17255-0.08046 0.25933-0.11906 0.16734-0.04325 0.34125 0.07 0.40373 0.2244 0.02793 0.06114 0.03823 0.1287 0.03285 0.19515-0.0056 0.02989-0.01176 0.06025-0.01763 0.09022-0.0032 0.01086-0.0063 0.02136-0.01073 0.03168-0.08023 0.4259-0.14957 0.86602-0.09082 1.2982 6.64e-4 4e-3 5.51e-4 0.0078 0.0012 0.01201 0.0044 0.03133 0.0092 0.06247 0.01521 0.09369l9e-6 6.08e-4c0.03711 0.20418 0.0897 0.40738 0.05453 0.61533-0.02206 0.11397-0.05677 0.22451-0.09566 0.33406-0.15382 0.22701-0.25315 0.48884-0.28298 0.76275-0.0096 0.03697-0.01838 0.074-0.02534 0.11172-0.01368 0.07831-0.01594 0.15659-0.01098 0.23467 8.88e-4 0.08935 0.01137 0.17849 0.03308 0.26559 0.02656 0.10822 0.062 0.21492 0.10488 0.31791 0.04242 0.11418 0.09843 0.22217 0.16581 0.32308 0.17068 0.26135 0.41761 0.4668 0.69925 0.60022 8.88e-4 5.16e-4 0.0023 0.0011 0.0036 0.0018 0.02971 0.01431 0.05977 0.02772 0.09028 0.04032 0.01525 0.0061 0.03044 0.01201 0.04603 0.01776 0.05711 0.02165 0.11545 0.04059 0.17451 0.05616 0.18618 0.0579 0.38335 0.08297 0.58474 0.06741 0.86085-0.02458 1.7105-0.73162 1.7085-1.6283 0.06401-0.54779-0.10538-1.1311-0.53432-1.4957-0.33331-0.31509-0.78711-0.48057-1.2398-0.48268-0.0023-2.54e-4 -0.0044-2.75e-4 -0.0067-5.9e-4 -0.14756-0.02617-0.29541-0.0531-0.43974-0.09474-0.08164-0.03102-0.1439-0.09329-0.18919-0.16599-0.02036-0.04123-0.03941-0.08299-0.05476-0.1258-0.01391-0.04024-0.02426-0.08188-0.03321-0.12349-0.03078-0.17059-0.02874-0.34919 0.0092-0.51887l5.97e-4 -5.98e-4 -4e-6 -5.98e-4c0.0023-0.0093 0.0049-0.01826 7e-3 -0.02749 0.04323-0.15597 0.11912-0.30181 0.22614-0.42282 0.13116 0.02621 0.2646 0.03938 0.39835 0.03936 0.87284-0.0033 1.6459-0.56386 1.9204-1.3925 0.10985-0.0169 0.23039-0.01451 0.34036 9e-3 0.0044 6.28e-4 0.0089 0.0023 0.01314 0.0036 0.12098 0.03483 0.23982 0.08007 0.35091 0.1398 0.08708 0.05354 0.16575 0.11987 0.22869 0.2011 0.10028 0.13152 0.14468 0.29335 0.14355 0.4563-0.0044 0.0204-0.0066 0.04096-0.013 0.06094-0.02283 0.10796-0.05969 0.21138-0.0999 0.31376-0.04328 0.0873-0.08021 0.17738-0.10962 0.27023-0.06292 0.13228-0.11157 0.26991-0.10911 0.41889-0.0012 0.01457-0.0012 0.02948-0.0021 0.04417-0.02263 0.23782 0.01192 0.47952 0.09061 0.70604 0.06882 0.21263 0.17503 0.41304 0.31948 0.58529 0.17709 0.21712 0.41056 0.38734 0.67143 0.48865 0.08733 0.03523 0.17735 0.06343 0.27074 0.08154 0.23439 0.04835 0.47648 0.04345 0.70928-0.0064 0.0044-7.04e-4 0.01142-0.0011 0.0155-0.0018 0.74592-0.09052 1.347-0.72844 1.5424-1.4307 0.09053-0.65192-0.11904-1.3856-0.67315-1.7814-0.40184-0.33181-0.94786-0.44969-1.4526-0.35723-0.09503-0.01649-0.18626-0.04909-0.27255-0.09287-0.22024-0.12666-0.40636-0.30748-0.60927-0.46199-0.11352-0.09134-0.23422-0.176-0.36319-0.24485-0.1323-0.07683-0.27318-0.13664-0.42562-0.16344-0.05977-0.01365-0.12198-0.02507-0.18343-0.03941-0.02887-0.0075-0.05778-0.01551-0.08663-0.023-0.07711-0.02332-0.15032-0.05493-0.21288-0.10621-0.01913-0.12962-0.05071-0.25709-0.09433-0.38062 0.02003-0.06508 0.05516-0.12526 0.10335-0.17168 0.03229-0.02317 0.06443-0.04665 0.09652-0.07017 0.07532-0.05212 0.15177-0.10285 0.22758-0.15415 0.16389-0.10988 0.34632-0.20947 0.54014-0.25307 0.06658-0.01341 0.13459-0.02028 0.20413-0.01858 0.01932 4.64e-4 0.03955 0.0015 0.0591 0.0035 0.37844 0.4238 0.94823 0.67862 1.5238 0.59581 0.91473-0.09893 1.7508-0.97706 1.6102-1.9246-0.02874-0.45722-0.23236-0.89771-0.59185-1.1875-0.04094-0.042531-0.08432-0.081137-0.12933-0.11841-0.24194-0.23155-0.54696-0.39665-0.87922-0.45159-0.59013-0.098051-1.2262 0.1489-1.5868 0.62834-0.29014 0.36896-0.44464 0.84525-0.40896 1.3147 0.0015 0.09944-0.01217 0.18557-0.04804 0.28077-0.03504 0.0832-0.0864 0.15655-0.14677 0.22257-0.03288 0.02805-0.06764 0.05369-0.10428 0.07499-0.16056 0.10912-0.32082 0.226-0.49095 0.32098-0.08566 0.04126-0.17243 0.08103-0.25933 0.11964-0.16737 0.04333-0.34124-0.06998-0.4037-0.22441-0.02792-0.06112-0.03824-0.1287-0.03284-0.19515 0.0057-0.02992 0.01129-0.05963 0.01705-0.08959 0.0032-0.01078 7e-3 -0.02137 0.01116-0.03171 0.08024-0.42594 0.1502-0.86661 0.09142-1.2989-6.51e-4 -0.00391-0.0012-0.00764-0.0019-0.011285-5e-3 -0.033572-0.01078-0.067366-0.01697-0.10084-0.02406-0.13173-0.05327-0.26314-0.06156-0.39566 0.36729-0.31205 0.61126-0.76112 0.59845-1.2587 0.05255-0.578-0.17284-1.1797-0.64488-1.5306-0.01384-0.010397-0.02798-0.019774-0.04185-0.029754-0.14357-0.1439-0.31323-0.26179-0.49785-0.3483-0.02844-0.013674-0.05752-0.026486-0.0867-0.038535-0.01562-0.00622-0.03091-0.012173-0.04662-0.017767-0.34959-0.13239-0.74277-0.15343-1.0986-0.034628-0.52993 0.16833-0.98858 0.57444-1.1811 1.1005-0.17988 0.47319-0.1696 1.0269 0.06498 1.4787l0.0036 0.0063c0.0089 0.017172 0.01846 0.033776 0.02823 0.050664 0.01086 0.018841 0.02153 0.038038 0.03302 0.056622 9.77e-4 0.00142 0.0023 0.0032 0.0032 0.00568 9.77e-4 0.00142 0.0024 0.00346 0.0037 0.00506 0.08522 0.13563 0.19062 0.25827 0.31102 0.36383 0.3476 0.4003 0.86174 0.65697 1.4028 0.63561 0.09464-0.00124 0.18868-0.011161 0.28173-0.028289 0.03544 0.030002 0.06576 0.066362 0.09048 0.106 0.02028 0.041261 0.03881 0.08293 0.05414 0.1258 0.0139 0.039991 0.02487 0.080924 0.03381 0.1223 0.03087 0.17088 0.02823 0.34949-0.0098 0.51948-5.2e-5 2.55e-4 3e-6 0.0012 3e-6 0.0012-0.0021 0.0093-0.0049 0.01825-0.0069 0.02749-0.04981 0.17965-0.14337 0.34543-0.27737 0.47558-0.08653-0.01091-0.17371-0.01668-0.26097-0.01652-0.83288 0.0026-1.5798 0.51321-1.8844 1.2885-0.13695 0.04386-0.30955 0.05277-0.46265 0.02012-0.0044-5.75e-4 -0.0088-2e-3 -0.01314-0.0036-0.12105-0.03483-0.24036-0.08119-0.35151-0.14099-0.08694-0.05357-0.1652-0.11937-0.22806-0.20053-0.07562-0.0991-0.11961-0.21532-0.136-0.33631 0.32882-0.43238 0.47743-1.0049 0.33844-1.5396-0.0088-0.03774-0.01897-0.07533-0.03021-0.11214-0.01664-0.096084-0.04107-0.19105-0.07313-0.28337-0.068896-0.21222-0.17533-0.41096-0.31955-0.58292-0.17707-0.21713-0.40997-0.38856-0.67086-0.48984-0.087353-0.035223-0.17802-0.062837-0.27132-0.080968-0.54706-0.11302-1.1368 0.061222-1.5504 0.43282-0.27127 0.23908-0.47283 0.55731-0.57031 0.90455-0.022988 0.080542-0.039894 0.16268-0.050576 0.24615-1.597e-4 0.0014-1.788e-4 2e-3 -5.034e-4 0.0029-0.00242 0.01932-0.00383 0.0391-0.00574 0.05852-0.00204 0.01888-0.00421 0.03766-0.00524 0.0567-7.78e-5 0.0021-6.375e-4 0.0042-6.272e-4 0.0066-2.057e-4 0.0057 2.951e-4 0.01142 1.9e-5 0.01673-8.459e-4 0.02227-0.00255 0.04442-0.00294 0.06687zm2.8528 5.1238c0.23474 1.3154-1.2233 1.7518-1.96 0.79552-0.096298-0.12501-0.1334-0.29331-0.12436-0.5441 0.04539-1.2597 1.8504-1.5626 2.0844-0.25143zm-2.0805-4.9997c-4.14e-4 -0.03178 3.987e-4 -0.06328 0.00294-0.09492 0.00972-0.14228 0.041363-0.2707 0.10023-0.40273 0.087263-0.19478 0.23516-0.36257 0.41523-0.46765 0.13072-0.073288 0.27666-0.1173 0.42652-0.12073 0.23012-0.012555 0.46624 0.059072 0.65559 0.19555 0.23914 0.19505 0.38633 0.50003 0.42348 0.80946 5.43e-4 0.0044 4.85e-4 0.0088 0.0012 0.01314 0.01584 0.23121-0.033616 0.46638-0.16507 0.65791-0.16877 0.25009-0.45316 0.40389-0.74559 0.43581-0.28934 0.018149-0.58909-0.093547-0.79974-0.29241-0.11412-0.12214-0.20044-0.27288-0.25153-0.43326-0.03275-0.099407-0.053414-0.1992-0.063036-0.30011zm4.4432 2.6292c-3.93e-4 -0.03433 7.18e-4 -0.06793 0.0033-0.10208 0.01035-0.1536 0.04437-0.29246 0.10789-0.435 0.0942-0.21028 0.25417-0.39134 0.44858-0.50474 0.14113-0.07912 0.29816-0.12665 0.45991-0.13041 0.24841-0.01314 0.50377 0.06356 0.70817 0.21092 0.25815 0.21055 0.41644 0.5404 0.45654 0.87443 3.4e-4 5e-3 8.85e-4 0.0098 0.0018 0.01421 0.0172 0.24958-0.0362 0.50254-0.17811 0.7093-0.1821 0.27-0.48939 0.4362-0.80509 0.47066-0.31239 0.01967-0.63574-0.10082-0.86314-0.3155-0.12318-0.13189-0.21679-0.2941-0.27195-0.46724-0.03538-0.10736-0.0575-0.21561-0.06789-0.32458zm0.09756-5.3995c0.0026-0.20054 0.05738-0.39941 0.17256-0.56358 0.25224-0.35013 0.71824-0.50975 1.1271-0.40478 0.02202 0.00506 0.04355 0.012733 0.06516 0.019525 0.19279 0.075286 0.36536 0.20716 0.4854 0.3794l1e-6 5.975e-4c0.0118 0.019348 0.02327 0.039245 0.03366 0.059561 0.10164 0.19004 0.14579 0.40716 0.15063 0.62285-0.01217 0.17109-0.06325 0.34002-0.15072 0.48648-0.10574 0.18546-0.27063 0.34143-0.46076 0.43312-0.09267 0.038473-0.19066 0.062784-0.29114 0.067756-0.2244 0.020697-0.45274-0.039254-0.64296-0.16156-0.1699-0.1184-0.30833-0.28634-0.38546-0.48178-0.0631-0.14384-0.09524-0.30042-0.10353-0.4576zm0.06975 10.548v-5.97e-4c0.01214-0.17115 0.0632-0.33937 0.15072-0.48589 0.10582-0.18552 0.27053-0.34204 0.46075-0.43373 0.09258-0.03845 0.19073-0.06278 0.29114-0.06779 0.2245-0.02071 0.4527 0.03972 0.64296 0.16216 0.16979 0.11839 0.30827 0.28592 0.38543 0.48119 0.06316 0.14389 0.09525 0.30035 0.10352 0.4576-2e-3 0.20046-0.05739 0.39947-0.17253 0.56358-0.25224 0.35011-0.71824 0.50976-1.1271 0.40478-0.02193-0.0051-0.04362-0.01264-0.06514-0.0195-0.19285-0.07528-0.36473-0.2065-0.48482-0.37878-0.01189-0.01973-0.02374-0.03954-0.03422-0.0602-0.10158-0.19001-0.14577-0.40721-0.15065-0.62285zm4.3751-7.8209 6.02e-4 -2e-6 -3e-6 -5.98e-4c0.0054-0.27198 0.10647-0.54189 0.28243-0.7484 0.03595-0.042433 0.07585-0.081625 0.11781-0.11742 0.17318-0.13958 0.38711-0.22498 0.60764-0.23597 0.13685-0.00939 0.274 0.012999 0.40321 0.059614 0.23957 0.092545 0.44674 0.27278 0.57418 0.50387 0.07819 0.13709 0.11625 0.2933 0.13159 0.45037 0.01808 0.28684-0.07156 0.57979-0.2512 0.80383-0.0086 0.01015-0.01701 0.01956-0.02558 0.02929-0.07044 0.07814-0.15021 0.14715-0.23817 0.20316-0.0015 7.31e-4 -0.0021 0.0021-0.0044 0.0029-0.01929 0.01273-0.04095 0.02459-0.06198 0.03605-0.0085 0.0044-0.01763 0.0091-0.02682 0.01383-0.0085 0.0051-0.01923 0.0091-0.02864 0.01382-0.08368 0.03872-0.17182 0.06704-0.26183 0.0832-0.0063 0.0015-0.013 0.0021-0.02146 0.0036-0.01717 0.0029-0.03526 0.0056-0.05313 0.0076-0.0109 0.0015-0.02178 0.0011-0.03161 0.0018-0.01716 0.0014-0.03377 0.0023-0.05075 0.0033-0.14288 0.0026-0.28442-0.02867-0.41522-0.08704-0.01506-0.0071-0.03064-0.01394-0.04547-0.02196-0.0042-0.0023-0.0088-0.0051-0.01314-0.0071-0.27692-0.15196-0.48335-0.42909-0.56358-0.74096-0.01746-0.08456-0.0266-0.17111-0.02457-0.25725zm0.03674 5.2026c-0.0029-0.21075 0.04847-0.42018 0.16827-0.59462 0.16874-0.25019 0.45297-0.40398 0.74554-0.43582 0.28934-0.0181 0.58858 0.09295 0.79919 0.29183 0.11406 0.12214 0.20112 0.27294 0.25214 0.43326 0.03276 0.09943 0.05288 0.1992 0.06247 0.30012 3.34e-4 0.03188 1.82e-4 0.06387-0.0018 0.09553-0.01053 0.14228-0.04194 0.2707-0.10081 0.40273-0.08728 0.19481-0.23509 0.36257-0.41526 0.46762-0.13061 0.07324-0.27673 0.11729-0.42652 0.12076-0.23014 0.01252-0.46623-0.05966-0.6556-0.19615-0.23912-0.19505-0.38572-0.50007-0.42288-0.80946-5.84e-4 -0.0044-0.0012-0.0087-0.0018-0.01312-0.0017-0.0206-0.0019-0.04199-0.0021-0.06269zm-14.063 2.979c-0.03509-0.79027 1.2143-1.1549 1.5414-0.34796 0.42457 0.82864-0.99813 1.5774-1.4272 0.72007-0.068786-0.13746-0.10954-0.2664-0.11421-0.37211zm-0.55092-11.482c-0.0039609-0.15088 0.050052-0.31626 0.14635-0.41747 0.41229-0.69438 1.5519-0.28216 1.3915 0.5257-0.061221 0.88817-1.3822 0.85914-1.5136 0.034895-0.014631-0.04404-0.022809-0.092849-0.024088-0.14314zm1.1571 5.7023c-4.86e-5 -0.1573 0.048213-0.3202 0.11628-0.45098 0.38138-0.88373 1.8465-0.72225 1.8933 0.29518 0.18163 1.1288-1.6349 1.5222-1.9507 0.42405-0.040993-0.08163-0.058677-0.17385-0.05878-0.26825zm4.1115-7.9296c-0.013448-0.22385 0.046436-0.45959 0.18363-0.63738 0.57275-0.92267 2.1477-0.17668 1.7567 0.84561-0.24483 0.99984-1.8386 0.92955-1.9301-0.11333-0.00498-0.03129-0.00831-0.062917-0.010223-0.09493zm-5.4441 13.78c0.00123 0.01865 0.0032 0.03739 0.00536 0.05599 0.084261 0.94878 1.1871 1.6259 2.0728 1.2799 0.27387-0.11774 0.52922-0.2955 0.72326-0.5226 0.36391-0.45467 0.35774-1.0941 0.10013-1.5998l0.015526 0.01342c4.729e-4 -0.0018 0.00122-4e-3 0.00178-6e-3 0.11219-0.42054 0.11964-1.0219 0.051868-1.453-0.098292-0.61407-0.27374-1.2164-0.3738-1.8301 0.00882-0.04281 0.020343-0.08831 0.033313-0.13487 0.47314-0.06654 0.91757-0.3022 1.1892-0.70828 0.34216-0.46651 0.39727-1.1102 0.15344-1.6327-0.078652-0.1867-0.19256-0.35855-0.33139-0.50846-0.0016604-0.081039-0.013571-0.15392-0.039281-0.21595 0.15044-0.58744 0.21283-1.1991 0.30885-1.7964 0.13506-0.75053 0.44376-1.4668 0.88993-2.0849 0.26106-0.37494 0.64981-0.74871 0.97633-1.085 0.67581 0.25163 1.507 0.12687 1.9874-0.43342 0.30702-0.38756 0.51838-0.89063 0.4216-1.3921-0.14702-0.90822-1.0741-1.6444-1.9994-1.4808-0.8823 0.089691-1.5377 0.98126-1.4685 1.8442 0.002169 0.40149 0.15994 0.80107 0.44489 1.0822-0.1529 0.16773-0.30265 0.33739-0.4437 0.51439-0.49743 0.60843-0.93097 1.2741-1.2073 2.011-0.16736 0.45646-0.25365 0.94845-0.32397 1.4305-0.05767 0.38323-0.11866 0.77018-0.21665 1.1464-0.23229-0.08907-0.48317-0.12433-0.73459-0.089869-0.25853-0.78941-0.56707-1.6808-0.76156-2.4954 0.31886-0.093674 0.60375-0.28811 0.77707-0.58414 0.30865-0.4646 0.31952-1.0934 0.037479-1.5726-0.29253-0.54982-0.9446-0.90379-1.5655-0.7705-0.7412 0.10817-1.3367 0.85038-1.2674 1.5987-0.0037 0.61886 0.47921 1.1527 1.0586 1.3152 0.089845 0.029718 0.18334 0.049314 0.27796 0.062358 0.16733 0.74488 0.4651 1.4617 0.70971 2.1844 0.048161 0.13982 0.092133 0.28097 0.13438 0.42264-0.55479 0.26006-0.96407 0.82278-1.02 1.4383-0.010349 0.09731-0.010222 0.19552-0.00281 0.29308 0.029032 0.53068 0.32735 1.0423 0.80329 1.2911 0.14809 0.08379 0.30889 0.14493 0.4755 0.18437 0.00369 0.01978 0.00741 0.03996 0.012646 0.0595 0.13633 0.91661 0.45135 1.814 0.41546 2.7507-7.69e-5 0.0011-3.912e-4 0.0023-4.818e-4 0.0031-0.091699-0.05349-0.19024-0.09746-0.29481-0.12747-0.68391-0.17564-1.4676 0.12559-1.8284 0.73968-0.13584 0.22355-0.20801 0.48523-0.20115 0.74663l6.441e-4 -3.2e-5c5.341e-4 0.01865 9.501e-4 0.03748 0.00218 0.05615zm11.732-17.861c0.69597 0.37602 0.36678 1.6351-0.4936 1.5012-0.92904-0.061585-0.84036-1.6669 0.11568-1.5944 0.15328 0.011667 0.28483 0.042912 0.37792 0.093217zm10.133 5.4276c0.13149 0.074078 0.24559 0.20545 0.28292 0.34007 0.38382 0.71051-0.55544 1.4763-1.166 0.92352-0.73048-0.50891-0.02673-1.6273 0.74788-1.3165 0.04532 0.010149 0.09135 0.028147 0.13518 0.052884zm-5.4864-1.9375c0.135 0.080737 0.24991 0.2059 0.32713 0.33151 0.56214 0.78131-0.32924 1.9552-1.2261 1.4725-1.0617-0.42427-0.46578-2.1848 0.63864-1.8914 0.09108 0.00684 0.17929 0.039015 0.26032 0.087432zm4.6898 7.6019c0.19898 0.10344 0.37041 0.27601 0.45242 0.48506 0.49721 0.96547-0.95206 1.9332-1.6282 1.0724-0.73193-0.72383 0.14738-2.055 1.089-1.5975 0.0294 0.0118 0.05822 0.02523 0.08671 0.03997zm-9.0243-11.752c-0.01665-0.00861-0.03369-0.016524-0.0508-0.024151-0.85722-0.41526-2.0048 0.18287-2.1631 1.1205-0.03972 0.29544-0.01845 0.60584 0.07666 0.889 0.20304 0.54583 0.75478 0.86912 1.3208 0.90793l-0.01949 0.00648c0.0012 0.00142 0.0028 0.00293 0.0042 0.00444 0.30311 0.31234 0.81518 0.62776 1.2198 0.79112 0.5773 0.23122 1.1841 0.39021 1.762 0.61973 0.03217 0.029576 0.0653 0.062828 0.09857 0.097911-0.18604 0.44008-0.21225 0.94245-0.0035 1.3841 0.22437 0.53325 0.74826 0.91129 1.3218 0.97066 0.20058 0.028475 0.40655 0.019037 0.60649-0.022988 0.07041 0.040169 0.139 0.067481 0.20545 0.077275 0.42664 0.43091 0.9193 0.79876 1.3823 1.1881 0.57445 0.50155 1.0303 1.1345 1.3312 1.8348 0.1875 0.41662 0.30838 0.94218 0.42905 1.395-0.56313 0.45042-0.88328 1.2276-0.64945 1.9276 0.1747 0.46255 0.49766 0.90238 0.97758 1.077 0.85468 0.34057 1.9626-0.07647 2.2978-0.95435 0.37646-0.80299-0.05156-1.8234-0.82746-2.2075-0.34552-0.20448-0.76939-0.27444-1.157-0.17444-0.06534-0.21732-0.13393-0.43298-0.2133-0.64494-0.26634-0.73937-0.61466-1.4534-1.1048-2.0691-0.30559-0.37813-0.6833-0.70497-1.0607-1.013-0.29912-0.24641-0.59974-0.49756-0.87214-0.77496 0.19577-0.15351 0.35495-0.35061 0.45458-0.584 0.81007 0.18387 1.7333 0.37721 2.532 0.62899-0.08352 0.32169-0.06307 0.66599 0.10182 0.96679 0.23996 0.50352 0.77382 0.83597 1.3298 0.84026 0.62199 0.031574 1.2607-0.34593 1.4654-0.94701 0.28807-0.69145-0.042646-1.5836-0.72028-1.9089-0.52898-0.32122-1.2351-0.18123-1.6723 0.23234-0.071663 0.061798-0.13651 0.13196-0.19633 0.20641-0.725-0.23923-1.4929-0.35216-2.2387-0.51364-0.1447-0.030535-0.28838-0.06535-0.43162-0.10192 0.06203-0.60961-0.21041-1.2499-0.7097-1.6141-0.078171-0.058877-0.16246-0.10925-0.25001-0.15296-0.47016-0.2478-1.0624-0.2548-1.5205 0.025634-0.14799 0.083987-0.28306 0.19052-0.4025 0.31318-0.01892-0.00684-0.03809-0.014144-0.05754-0.019712-0.85638-0.35406-1.788-0.54492-2.5733-1.0571-8.37e-4 -5.922e-4 -0.0018-0.00169-0.0023-0.00186 0.09301-0.05117 0.18136-0.11312 0.26084-0.1874 0.50208-0.49648 0.64636-1.3235 0.30501-1.9487-0.12196-0.2314-0.30937-0.42778-0.53714-0.55622l-3.04e-4 5.709e-4c-0.01625-0.00914-0.03265-0.018442-0.04927-0.02701zm9.7889 19.756c-0.6863 0.39339-1.583-0.54984-1.0158-1.2105 0.54186-0.75717 1.8595 0.16405 1.2942 0.93841-0.09066 0.12414-0.18655 0.21946-0.27836 0.27208zm-9.9523 5.7526c-0.13225 0.07272-0.30402 0.10049-0.4381 0.06128-0.80612-0.04815-0.96213-1.25-0.17055-1.4776 0.81743-0.3527 1.3972 0.83471 0.72484 1.3293-0.03241 0.03323-0.07205 0.06282-0.11618 0.08702zm4.5374-3.6421c-0.13976 0.0722-0.3067 0.10391-0.45412 0.10336-0.96024 0.06611-1.4883-1.31-0.60548-1.8179 0.92002-0.67884 2.1023 0.75528 1.2711 1.5394-0.05381 0.0738-0.12763 0.13183-0.21147 0.1752zm-8.9321-0.01946c-0.19275 0.11461-0.42976 0.16939-0.65066 0.12895-1.0826-0.08612-1.1415-1.8278-0.053618-1.9488 1.0009-0.24069 1.6689 1.208 0.78394 1.7673-0.025554 0.01876-0.05212 0.0362-0.079672 0.05264zm14.743-1.4779c0.01599-0.0097 0.03178-0.01993 0.04731-0.03045 0.80462-0.50977 0.90092-1.8003 0.18738-2.4288-0.23018-0.18943-0.5052-0.3349-0.79598-0.40326-0.5709-0.11503-1.1364 0.18357-1.4677 0.64425l0.0047-0.01999c-0.0018 4.28e-4 -0.0041 7.96e-4 -0.0059 0.0012-0.42524 0.09306-0.96314 0.36208-1.3151 0.6199-0.50072 0.36884-0.95558 0.80083-1.4552 1.171-0.04209 0.01176-0.08781 0.02241-0.13515 0.0322-0.27601-0.39-0.68918-0.67698-1.1746-0.73229-0.57139-0.09027-1.1688 0.15579-1.5215 0.61203-0.12995 0.15548-0.23045 0.3355-0.30005 0.52761-0.07124 0.03867-0.1306 0.08258-0.17393 0.1339-0.59104 0.13556-1.1633 0.36046-1.7381 0.54892-0.72896 0.22398-1.507 0.27795-2.2609 0.16469-0.45288-0.06016-0.96326-0.23434-1.4118-0.37042-0.08613-0.71597-0.57803-1.3976-1.2962-1.5677-0.48518-0.09524-1.0292-0.05249-1.4305 0.26338-0.7398 0.54697-0.96914 1.7083-0.39957 2.4557 0.48412 0.74305 1.5769 0.91693 2.3122 0.45983 0.35585-0.18591 0.63864-0.50933 0.75788-0.89143 0.21912 0.05898 0.43857 0.1143 0.66052 0.15853 0.76875 0.16321 1.5592 0.24339 2.3406 0.15121 0.4824-0.06049 0.9592-0.2093 1.4199-0.36777 0.36704-0.1244 0.7389-0.24756 1.1182-0.33293 0.02732 0.24727 0.11096 0.48641 0.25683 0.69408-0.5831 0.59161-1.2339 1.2744-1.8688 1.8207-0.22942-0.24046-0.53281-0.40453-0.87535-0.42287-0.55438-0.06135-1.1183 0.21721-1.4148 0.68753-0.35456 0.512-0.37028 1.2538 0.03275 1.7445 0.43588 0.60916 1.3685 0.7982 2.0018 0.39358 0.55173-0.28035 0.80482-0.95427 0.68366-1.5437-0.01477-0.09346-0.0402-0.18555-0.07199-0.27562 0.58533-0.49015 1.086-1.0834 1.6161-1.632 0.10217-0.10689 0.20749-0.21066 0.31404-0.31315 0.48544 0.37392 1.1732 0.47975 1.7459 0.24731 0.09123-0.03542 0.17846-0.08051 0.26174-0.13189 0.45835-0.26902 0.77635-0.76871 0.77934-1.3057 0.0065-0.17002-0.01273-0.34097-0.05407-0.50711 0.01586-0.01239 0.03209-0.02492 0.04706-0.03855 0.75216-0.5413 1.4053-1.2326 2.2543-1.6301 9.76e-4 -3.98e-4 0.0023-7.18e-4 0.0028-9.76e-4 -0.0054 0.10602 5.77e-4 0.21374 0.02184 0.32043 0.15741 0.68833 0.78435 1.2467 1.4956 1.2861 0.26094 0.01827 0.52659-0.03754 0.75577-0.16345l-3.23e-4 -5.58e-4c0.01628-0.0091 0.03286-0.01803 0.0489-0.02765z"
-}


locPath : String
locPath =
    "m11.065 0.0072052c-3.76 0.13387-7.405 2.2141-9.3623 5.4396-1.0074 1.6314-1.5856 3.4998-1.6814 5.393-0.19447 3.119 0.96761 6.2857 3.1366 8.5362 1.9766 1.9916 3.9241 4.0132 5.9489 5.9568 0.49652 0.54078 1.1218 0.9957 1.8709 1.0817 1.0557 0.1876 2.1632-0.24442 2.8431-1.0675 2.0941-2.101 4.2095-4.1824 6.2664-6.3195 2.642-2.9647 3.5596-7.3628 2.2645-11.125-1.1425-3.461-3.986-6.3358-7.4921-7.3873-1.2392-0.38437-2.4999-0.55382-3.7944-0.50839zm0.39558 3.4417c3.0472-0.023627 6.016 1.8521 7.2729 4.6334 1.311 2.7559 0.86977 6.2402-1.1096 8.5679-0.93031 1.0426-1.9697 1.9837-2.9413 2.9881-1.0745 1.0738-2.1477 2.149-3.223 3.222-2.0546-2.0859-4.1488-4.1356-6.1631-6.2595-1.8259-2.1461-2.34-5.2831-1.3454-7.9126 0.75309-2.1276 2.476-3.8611 4.5785-4.6695 0.92894-0.37428 1.9288-0.57267 2.931-0.56973zm-0.30192 4.2139c-1.9408 0.072969-3.6553 1.8307-3.619 3.7852-0.036834 1.7504 1.2548 3.421 2.973 3.7882 1.6779 0.4401 3.5439-0.47328 4.2943-2.0224 0.78263-1.4808 0.42933-3.4486-0.83622-4.5508-0.75291-0.70047-1.7851-1.0695-2.812-1.0002z"


bulletPath : String
bulletPath =
    "m2.6458 2.6458v21.166h21.166v-21.166zm5.2916 5.2916h10.583v10.583h-10.583z"


darkPath : String
darkPath =
    "m18.796 2.6048-1.0454 2.058-2.3104-0.2376 1.6247 1.6564-0.66075 1.5082c0.13984 0.077681 0.27955 0.16016 0.41749 0.24778l1.3747-0.71279 1.7232 1.567-0.35074-2.2775 2.0015-1.1767-2.2855-0.3711zm2.8432 6.2466-0.68451 1.3509-1.4969-0.16519 1.0805 1.0364-0.4899 1.0669c0.01992 0.07251 0.03856 0.14573 0.05658 0.21949l1.1461-0.57476 1.1099 1.0058-0.25457-1.5093 1.3057-0.71958-1.4799-0.23081zm-12.511-1.1654c-0.98578 0.56842-1.8149 1.368-2.4846 2.4031-1.1154 1.7241-1.4593 3.5939-1.033 5.6084 0.4248 2.0072 1.4961 3.5733 3.2144 4.6977 1.7241 1.1154 3.59 1.46 5.5971 1.0352 2.0145-0.42639 3.5795-1.4977 4.6965-3.2144 0.47349-0.7277 0.80756-1.4805 1.0058-2.2583-0.66914 0.38486-1.41 0.66554-2.2266 0.83838-2.0072 0.42479-3.8731 0.08013-5.5971-1.0352-1.7182-1.1243-2.7896-2.6904-3.2144-4.6976-0.24874-1.1753-0.23414-2.3009 0.041861-3.3773z"


lightPath : String
lightPath =
    "m19.119 18.216q1.6842-2.5884 1.047-5.5992-0.63955-3.0218-3.228-4.706-2.5797-1.6976-5.6015-1.058-3.0108 0.6372-4.6926 3.2367-1.6731 2.5861-1.0336 5.6079 0.6372 3.0108 3.2146 4.6973 2.5861 1.6731 5.5969 1.0359 3.0218-0.63956 4.6973-3.2146zm-8.1041-13.755c-0.05515-0.011385-0.11602-0.010071-0.18227 0.00394-0.1262 0.026711-0.21102 0.090642-0.25541 0.19232l-0.63709 1.4465c0.37356-0.13213 0.76165-0.24292 1.1666-0.32863 0.41225-0.087249 0.81958-0.1456 1.2211-0.17601l-1.1645-1.0653c-0.04407-0.036826-0.09331-0.061304-0.14846-0.072711zm5.8257 0.61106c-0.08528-0.011823-0.16228 0.00482-0.232 0.049218l-1.3592 0.78529c0.75825 0.22725 1.4937 0.57327 2.2079 1.0335l-0.33413-1.5809c-0.02805-0.13248-0.09298-0.2212-0.19468-0.26561-0.03015-0.010071-0.05942-0.017734-0.08782-0.021675zm-11.003 2.3293c-0.11527-0.015326-0.21603 0.019048-0.30365 0.10352-0.07996 0.089438-0.10702 0.19792-0.08031 0.32411l0.33206 1.5674c0.46772-0.70512 0.99925-1.3159 1.5979-1.8277zm13.782 1.4631c0.51034 0.64311 0.91571 1.3579 1.2162 2.145l0.65813-1.4933c0.04171-0.11431 0.03024-0.21673-0.03538-0.30834-0.06058-0.099225-0.14538-0.15709-0.25435-0.1736zm-15.035 3.1864-1.3629 0.78937c-0.09929 0.06054-0.15602 0.14816-0.17115 0.26345-0.01642 0.10899 0.01773 0.20705 0.10211 0.29464l1.1935 1.087c-0.04315-0.83467 0.03634-1.646 0.23859-2.4344zm16.845 1.7495c0.04528 0.84388-0.03582 1.6649-0.24345 2.4628l1.3799-0.79332c0.09922-0.06056 0.15604-0.15195 0.16986-0.27355 0.01248-0.12788-0.02273-0.23256-0.10588-0.31385zm-16.489 3.5036-0.65173 1.4606c-0.04309 0.10811-0.03323 0.2182 0.03002 0.33002 0.06816 0.1047 0.15681 0.16534 0.26582 0.18131l1.5651 0.16447c-0.50692-0.64128-0.91046-1.3533-1.2092-2.1366zm15.049 1.5909c-0.47203 0.71054-1.0113 1.3249-1.62 1.8389l1.5794 0.16583c0.09999 5e-3 0.1997-0.03558 0.2999-0.12272 0.07365-0.08808 0.09901-0.18884 0.07497-0.30242zm-11.666 2.4833 0.33117 1.5651c0.03166 0.11843 0.09833 0.19946 0.20005 0.24394 0.13325 0.03757 0.24162 0.03586 0.32467-0.0085l1.3659-0.78573c-0.76217-0.22151-1.5024-0.56101-2.2218-1.0149zm7.5348 0.78917c-0.38569 0.13754-0.78804 0.2515-1.2077 0.3403-0.4118 0.08714-0.81856 0.14544-1.22 0.17612l1.1928 1.0825c0.08324 0.08215 0.19057 0.1063 0.32162 0.07131 0.13248-0.02802 0.2201-0.09303 0.26452-0.19486z"
