module GraphView exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Color exposing (Color)
import Force
import Graph exposing (Edge, Graph, NodeContext, NodeId, Vertex)
import Html exposing (Html)
import Html.Attributes as Ha
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Json.Decode as Decode
import Task
import TypedSvg as Ts
import TypedSvg.Attributes as Tsa
import TypedSvg.Attributes.InPx as TsPx
import TypedSvg.Core as Ts exposing (Svg)
import TypedSvg.Types as Ts
import Ui as El exposing (Element, el)
import Ui.Background as Bg
import Ui.Font as Font
import Zoom exposing (OnZoom, Zoom)


type alias Model =
    { width : Int
    , height : Int
    , graph : GraphModel
    , count : Float
    }


type alias Pal colorType =
    { black : colorType
    , red : colorType
    , green : colorType
    , yellow : colorType
    , blue : colorType
    , magenta : colorType
    , cyan : colorType
    , white : colorType
    }


type Msg
    = WinResize Int Int
    | DragAt ( Float, Float )
    | DragEnd ( Float, Float )
    | DragStart NodeId ( Float, Float )
    | ReceiveElementPosition (Result Dom.Error Dom.Element)
    | Resize
    | Tick
    | ZoomMsg OnZoom


type alias Flags =
    { width : Int, height : Int }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subs
        , view = view
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        graph : Graph Entity ()
        graph =
            Graph.mapContexts initNode graphData
    in
    ( { width = flags.width
      , height = flags.height
      , graph = Init graph
      , count = 0.0
      }
    , getElementPosition
    )


{-| The graph data we defined at the end of the module has the type
`Graph String ()`. We have to convert it into a `Graph Entity ()`.
`Force.Entity` is an extensible record which includes the coordinates for the
node.
-}
initNode : NodeContext String () -> NodeContext Entity ()
initNode ctx =
    { node =
        { label = Force.entity ctx.node.id ctx.node.label
        , id = ctx.node.id
        }
    , incoming = ctx.incoming
    , outgoing = ctx.outgoing
    }


{-| Initializes the simulation by setting the forces for the graph.
-}
initSimulation : Graph Entity () -> Float -> Float -> Force.State NodeId
initSimulation graph width height =
    let
        link : { c | from : a, to : b } -> ( a, b )
        link { from, to } =
            ( from, to )
    in
    Force.simulation
        [ -- Defines the force that pulls connected nodes together. You can use
          -- `Force.customLinks` if you need to adjust the distance and
          -- strength.
          Force.links <| List.map link <| Graph.edges graph

        -- Defines the force that pushes the nodes apart. The default strength
        -- is `-30`, but since we are drawing fairly large circles for each
        -- node, we need to increase the repulsion by decreasing the strength to
        -- `-150`.
        , Force.manyBodyStrength -50 <| List.map .id <| Graph.nodes graph
        , Force.collision 30 <| List.map .id <| Graph.nodes graph

        -- Defines the force that pulls nodes to a center. We set the center
        -- coordinates to the center of the svg viewport.
        , Force.center (width / 2) (height / 2)
        ]
        |> Force.iterations 400


{-| Initializes the zoom and sets a minimum and maximum zoom level.

You can also use `Zoom.translateExtent` to restrict the area in which the user
may drag, but since the graph is larger than the viewport and the exact
dimensions depend on the data and the final layout, you would either need to use
some kind of heuristic for the final dimensions here, or you would have to let
the simulation play out (or use `Force.computeSimulate` to calculate it at
once), find the min and max x and y positions of the graph nodes and use those
values to set the translate extent.

-}
initZoom : SvgElement -> Zoom
initZoom element =
    Zoom.init { width = element.width, height = element.height }
        |> Zoom.scaleExtent 0.5 3
        |> Zoom.translateExtent ( ( 0, 0 ), ( element.width, element.height ) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        mapGraphModel =
            Tuple.mapFirst (\gs -> { model | graph = gs })
    in
    case ( msg, model.graph ) of
        ( WinResize x y, _ ) ->
            ( { model
                | width = x
                , height = y
              }
            , Cmd.none
            )

        ( Tick, Ready state ) ->
            let
                newCount =
                    model.count + 1
            in
            handleTick state
                |> mapGraphModel
                |> (\( m, c ) -> ( { m | count = newCount }, c ))

        ( Tick, Init _ ) ->
            let
                newCount =
                    model.count + 1
            in
            ( { model | count = newCount }, Cmd.none )

        --else
        --    ( { model | count = 0 }, Cmd.none )
        ( DragAt xy, Ready state ) ->
            handleDragAt xy state |> mapGraphModel

        ( DragAt _, Init _ ) ->
            ( model, Cmd.none )

        ( DragEnd xy, Ready state ) ->
            case state.drag of
                Just { index } ->
                    ( { model
                        | graph =
                            Ready
                                { state
                                    | drag = Nothing
                                    , graph = updateNodePosition index xy state
                                }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | graph = Ready state }, Cmd.none )

        ( DragEnd _, Init _ ) ->
            ( model, Cmd.none )

        ( DragStart index xy, Ready state ) ->
            ( { model
                | graph =
                    Ready
                        { state
                            | drag =
                                Just
                                    { start = xy
                                    , current = xy
                                    , index = index
                                    }
                        }
              }
            , Cmd.none
            )

        ( DragStart _ _, Init _ ) ->
            ( model, Cmd.none )

        ( ReceiveElementPosition (Ok { element }), Init graph ) ->
            -- When we get the svg element position and dimensions, we are
            -- ready to initialize the simulation and the zoom, but we cannot
            -- show the graph yet. If we did, we would see a noticable jump.
            ( { model
                | graph =
                    Ready
                        { drag = Nothing
                        , element = element
                        , graph = graph
                        , showGraph = False
                        , simulation =
                            initSimulation
                                graph
                                element.width
                                element.height
                        , zoom = initZoom element
                        }
              }
            , Cmd.none
            )

        ( ReceiveElementPosition (Ok { element }), Ready state ) ->
            ( { model
                | graph =
                    Ready
                        { drag = state.drag
                        , element = element
                        , graph = state.graph
                        , showGraph = True
                        , simulation =
                            initSimulation
                                state.graph
                                element.width
                                element.height
                        , zoom = initZoom element
                        }
              }
            , Cmd.none
            )

        ( ReceiveElementPosition (Err _), _ ) ->
            ( model, Cmd.none )

        ( Resize, _ ) ->
            ( model, getElementPosition )

        ( ZoomMsg zoomMsg, Ready state ) ->
            ( { model
                | graph = Ready { state | zoom = Zoom.update zoomMsg state.zoom }
              }
            , Cmd.none
            )

        ( ZoomMsg _, Init _ ) ->
            ( model, Cmd.none )


handleDragAt : ( Float, Float ) -> ReadyState -> ( GraphModel, Cmd Msg )
handleDragAt xy ({ drag, simulation } as state) =
    case drag of
        Just { start, index } ->
            ( Ready
                { state
                    | drag =
                        Just
                            { start = start
                            , current = xy
                            , index = index
                            }
                    , graph = updateNodePosition index xy state
                    , simulation = Force.reheat simulation
                }
            , Cmd.none
            )

        Nothing ->
            ( Ready state, Cmd.none )


handleTick : ReadyState -> ( GraphModel, Cmd Msg )
handleTick state =
    let
        ( newSimulation, list ) =
            Force.tick state.simulation <|
                List.map .label <|
                    Graph.nodes state.graph
    in
    case state.drag of
        Nothing ->
            ( Ready
                { state
                    | graph = updateGraphWithList state.graph list
                    , showGraph = True
                    , simulation = newSimulation
                }
            , Cmd.none
            )

        Just { current, index } ->
            ( Ready
                { state
                    | graph =
                        Graph.update index
                            (Maybe.map
                                (updateNode
                                    (shiftPosition
                                        state.zoom
                                        ( state.element.x
                                        , state.element.y
                                        )
                                        current
                                    )
                                )
                            )
                            (updateGraphWithList state.graph list)
                    , showGraph = True
                    , simulation = newSimulation
                }
            , Cmd.none
            )


updateNode :
    ( Float, Float )
    -> NodeContext Entity ()
    -> NodeContext Entity ()
updateNode ( x, y ) nodeCtx =
    let
        nodeValue =
            nodeCtx.node.label
    in
    updateContextWithValue nodeCtx { nodeValue | x = x, y = y }


updateNodePosition : NodeId -> ( Float, Float ) -> ReadyState -> Graph Entity ()
updateNodePosition index xy state =
    Graph.update
        index
        (Maybe.map
            (updateNode
                (shiftPosition
                    state.zoom
                    ( state.element.x, state.element.y )
                    xy
                )
            )
        )
        state.graph


updateContextWithValue :
    NodeContext Entity ()
    -> Entity
    -> NodeContext Entity ()
updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
    { nodeCtx | node = { node | label = value } }


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


shiftPosition : Zoom -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
shiftPosition zoom ( elementX, elementY ) ( clientX, clientY ) =
    let
        zoomRecord =
            Zoom.asRecord zoom
    in
    ( (clientX - zoomRecord.translate.x - elementX) / zoomRecord.scale
    , (clientY - zoomRecord.translate.y - elementY) / zoomRecord.scale
    )



-- Subscriptions --


subs : Model -> Sub Msg
subs model =
    let
        dragSubscriptions : Sub Msg
        dragSubscriptions =
            Sub.batch
                [ Events.onMouseMove
                    (Decode.map (.clientPos >> DragAt) Mouse.eventDecoder)
                , Events.onMouseUp
                    (Decode.map (.clientPos >> DragEnd) Mouse.eventDecoder)
                , Events.onAnimationFrame (always Tick)
                ]

        readySubscriptions : ReadyState -> Sub Msg
        readySubscriptions { drag, simulation, zoom } =
            Sub.batch
                [ Zoom.subscriptions zoom ZoomMsg
                , case drag of
                    Nothing ->
                        if Force.isCompleted simulation then
                            Sub.none

                        else
                            Events.onAnimationFrame (always Tick)

                    Just _ ->
                        dragSubscriptions
                ]
    in
    Sub.batch
        [ -- Graph Subs
          case model.graph of
            Init _ ->
                Sub.none

            Ready state ->
                readySubscriptions state
        , Events.onResize WinResize
        ]



-- View --


renderSize : number
renderSize =
    -- 512
    -- 128
    256


view : Model -> Html Msg
view model =
    El.layout
        [ El.width El.fill
        , El.height El.fill
        , Bg.color dim.black
        , Font.color mid.white
        , Font.family [ Font.monospace ]
        ]
    <|
        El.row
            [ El.width El.fill
            , El.height El.fill
            ]
            [ el
                [ El.width <| El.px model.height
                , El.height El.fill
                , Bg.color mid.black
                ]
              <|
                El.html <|
                    WebGL.toHtmlWith
                        [ WebGL.alpha False
                        , WebGL.depth 1
                        , WebGL.clearColor 0 0 0 1.0
                        ]
                        [ Ha.width renderSize
                        , Ha.height renderSize
                        , Ha.style "display" "block"
                        , Ha.style "max-height" "100vh"
                        , Ha.style "overflow-y" "hidden"
                        , Ha.style "image-rendering" "pixelated"
                        , Ha.style "max-width" "100vw"
                        , Ha.style "overflow-x" "hidden"
                        , Ha.style "background-color" "black"
                        ]
                        [ WebGL.entity
                            vShader
                            fShader
                            mesh
                            { u_resolution =
                                -- vec2
                                --    (toFloat model.height)
                                --    (toFloat model.height)
                                vec2 renderSize renderSize
                            , u_time = model.count
                            }
                        ]
            , El.column
                [ El.width El.fill
                , El.height El.fill
                ]
                [ graphView model
                , palView model
                ]
            ]


perspective : Float -> Mat4
perspective t =
    Mat4.mul
        (Mat4.makePerspective 45 1 0.01 100)
        (Mat4.makeLookAt
            (vec3 (4 * cos t) 0 (4 * sin t))
            (vec3 0 0 0)
            (vec3 0 1 0)
        )



-- Shader Stuff --


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


type alias Uniforms =
    --{ u_perspective : Mat4
    { u_resolution : Vec2
    , u_time : Float
    }


vShader : Shader Vertex Uniforms { vcolor : Vec3 }
vShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        //uniform mat4 u_perspective;
        varying vec3 vcolor;

        void main() {
            gl_Position = 1.0 * vec4(position, 1.0);
            //vcolor = color;
        }

    |]


fShader : Shader {} Uniforms { vcolor : Vec3 }
fShader =
    [glsl|
        precision lowp float;
        uniform vec2 u_resolution;
        uniform float u_time;
        varying vec3 vcolor;

        /*  HSLUV-GLSL v4.2
            HSLUV is a human-friendly alternative to HSL. ( http://www.hsluv.org )
            GLSL port by William Malo ( https://github.com/williammalo )
            Put this code in your fragment shader.
        */

        vec3 hsluv_intersectLineLine(vec3 line1x, vec3 line1y, vec3 line2x, vec3 line2y) {
          return (line1y - line2y) / (line2x - line1x);
        }

        vec3 hsluv_distanceFromPole(vec3 pointx,vec3 pointy) {
          return sqrt(pointx*pointx + pointy*pointy);
        }

        vec3 hsluv_lengthOfRayUntilIntersect(float theta, vec3 x, vec3 y) {
          vec3 len = y / (sin(theta) - x * cos(theta));
          if (len.r < 0.0) {len.r=1000.0;}
          if (len.g < 0.0) {len.g=1000.0;}
          if (len.b < 0.0) {len.b=1000.0;}
          return len;
        }

        float hsluv_maxSafeChromaForL(float L){
            mat3 m2 = mat3(
              3.2409699419045214  ,-0.96924363628087983 , 0.055630079696993609,
              -1.5373831775700935  , 1.8759675015077207  ,-0.20397695888897657 ,
              -0.49861076029300328 , 0.041555057407175613, 1.0569715142428786  
            );
            float sub0 = L + 16.0;
            float sub1 = sub0 * sub0 * sub0 * .000000641;
            float sub2 = sub1 > 0.0088564516790356308 ? sub1 : L / 903.2962962962963;
    
            vec3 top1   = (284517.0 * m2[0] - 94839.0  * m2[2]) * sub2;
            vec3 bottom = (632260.0 * m2[2] - 126452.0 * m2[1]) * sub2;
            vec3 top2   = (838422.0 * m2[2] + 769860.0 * m2[1] + 731718.0 * m2[0]) * L * sub2;
    
            vec3 bounds0x = top1 / bottom;
            vec3 bounds0y = top2 / bottom;
    
            vec3 bounds1x =              top1 / (bottom+126452.0);
            vec3 bounds1y = (top2-769860.0*L) / (bottom+126452.0);
    
            vec3 xs0 = hsluv_intersectLineLine(bounds0x, bounds0y, -1.0/bounds0x, vec3(0.0) );
            vec3 xs1 = hsluv_intersectLineLine(bounds1x, bounds1y, -1.0/bounds1x, vec3(0.0) );
    
            vec3 lengths0 = hsluv_distanceFromPole( xs0, bounds0y + xs0 * bounds0x );
            vec3 lengths1 = hsluv_distanceFromPole( xs1, bounds1y + xs1 * bounds1x );
    
            return  min(lengths0.r,
                    min(lengths1.r,
                    min(lengths0.g,
                    min(lengths1.g,
                    min(lengths0.b,
                        lengths1.b)))));
        }

        float hsluv_maxChromaForLH(float L, float H) {
            float hrad = radians(H);
    
            mat3 m2 = mat3(
                3.2409699419045214  ,-0.96924363628087983 , 0.055630079696993609,
                -1.5373831775700935  , 1.8759675015077207  ,-0.20397695888897657 ,
                -0.49861076029300328 , 0.041555057407175613, 1.0569715142428786  
            );
            float sub1 = pow(L + 16.0, 3.0) / 1560896.0;
            float sub2 = sub1 > 0.0088564516790356308 ? sub1 : L / 903.2962962962963;
    
            vec3 top1   = (284517.0 * m2[0] - 94839.0  * m2[2]) * sub2;
            vec3 bottom = (632260.0 * m2[2] - 126452.0 * m2[1]) * sub2;
            vec3 top2   = (838422.0 * m2[2] + 769860.0 * m2[1] + 731718.0 * m2[0]) * L * sub2;
    
            vec3 bound0x = top1 / bottom;
            vec3 bound0y = top2 / bottom;
    
            vec3 bound1x =              top1 / (bottom+126452.0);
            vec3 bound1y = (top2-769860.0*L) / (bottom+126452.0);
    
            vec3 lengths0 = hsluv_lengthOfRayUntilIntersect(hrad, bound0x, bound0y );
            vec3 lengths1 = hsluv_lengthOfRayUntilIntersect(hrad, bound1x, bound1y );
    
            return  min(lengths0.r,
                min(lengths1.r,
                min(lengths0.g,
                min(lengths1.g,
                min(lengths0.b,
                    lengths1.b)))));
        }
    
        float hsluv_fromLinear(float c) {
            return c <= 0.0031308 ? 12.92 * c : 1.055 * pow(c, 1.0 / 2.4) - 0.055;
        }
        vec3 hsluv_fromLinear(vec3 c) {
            return vec3( hsluv_fromLinear(c.r), hsluv_fromLinear(c.g), hsluv_fromLinear(c.b) );
        }
    
        float hsluv_toLinear(float c) {
            return c > 0.04045 ? pow((c + 0.055) / (1.0 + 0.055), 2.4) : c / 12.92;
        }
    
        vec3 hsluv_toLinear(vec3 c) {
            return vec3( hsluv_toLinear(c.r), hsluv_toLinear(c.g), hsluv_toLinear(c.b) );
        }
    
        float hsluv_yToL(float Y){
            return Y <= 0.0088564516790356308 ? Y * 903.2962962962963 : 116.0 * pow(Y, 1.0 / 3.0) - 16.0;
        }
    
        float hsluv_lToY(float L) {
            return L <= 8.0 ? L / 903.2962962962963 : pow((L + 16.0) / 116.0, 3.0);
        }
    
        vec3 xyzToRgb(vec3 tuple) {
            const mat3 m = mat3( 
                 3.2409699419045214  ,-1.5373831775700935 ,-0.49861076029300328 ,
                -0.96924363628087983 , 1.8759675015077207 , 0.041555057407175613,
                0.055630079696993609,-0.20397695888897657, 1.0569715142428786  );
        
            return hsluv_fromLinear(tuple*m);
        }
    
        vec3 rgbToXyz(vec3 tuple) {
            const mat3 m = mat3(
                0.41239079926595948 , 0.35758433938387796, 0.18048078840183429 ,
                0.21263900587151036 , 0.71516867876775593, 0.072192315360733715,
                0.019330818715591851, 0.11919477979462599, 0.95053215224966058 
            );
            return hsluv_toLinear(tuple) * m;
        }
    
        vec3 xyzToLuv(vec3 tuple){
            float X = tuple.x;
            float Y = tuple.y;
            float Z = tuple.z;
    
            float L = hsluv_yToL(Y);
        
            float div = 1./dot(tuple,vec3(1,15,3)); 
    
            return vec3(
                1.,
                (52. * (X*div) - 2.57179),
                (117.* (Y*div) - 6.08816)
            ) * L;
        }
    
    
        vec3 luvToXyz(vec3 tuple) {
            float L = tuple.x;
    
            float U = tuple.y / (13.0 * L) + 0.19783000664283681;
            float V = tuple.z / (13.0 * L) + 0.468319994938791;
    
            float Y = hsluv_lToY(L);
            float X = 2.25 * U * Y / V;
            float Z = (3./V - 5.)*Y - (X/3.);
    
            return vec3(X, Y, Z);
        }
    
        vec3 luvToLch(vec3 tuple) {
            float L = tuple.x;
            float U = tuple.y;
            float V = tuple.z;
    
            float C = length(tuple.yz);
            float H = degrees(atan(V,U));
            if (H < 0.0) {
                H = 360.0 + H;
            }
        
        
            return vec3(L, C, H);
        }
    
        vec3 lchToLuv(vec3 tuple) {
            float hrad = radians(tuple.b);
            return vec3(
                tuple.r,
                cos(hrad) * tuple.g,
                sin(hrad) * tuple.g
            );
        }
    
        vec3 hsluvToLch(vec3 tuple) {
            tuple.g *= hsluv_maxChromaForLH(tuple.b, tuple.r) * .01;
            return tuple.bgr;
        }
    
        vec3 lchToHsluv(vec3 tuple) {
            tuple.g /= hsluv_maxChromaForLH(tuple.r, tuple.b) * .01;
                return tuple.bgr;
        }
    
        vec3 hpluvToLch(vec3 tuple) {
            tuple.g *= hsluv_maxSafeChromaForL(tuple.b) * .01;
            return tuple.bgr;
        }
    
        vec3 lchToHpluv(vec3 tuple) {
            tuple.g /= hsluv_maxSafeChromaForL(tuple.r) * .01;
            return tuple.bgr;
        }
    
        vec3 lchToRgb(vec3 tuple) {
            return xyzToRgb(luvToXyz(lchToLuv(tuple)));
        }
    
        vec3 rgbToLch(vec3 tuple) {
            return luvToLch(xyzToLuv(rgbToXyz(tuple)));
        }
    
        vec3 hsluvToRgb(vec3 tuple) {
            return lchToRgb(hsluvToLch(tuple));
        }
    
        vec3 rgbToHsluv(vec3 tuple) {
            return lchToHsluv(rgbToLch(tuple));
        }
    
        vec3 hpluvToRgb(vec3 tuple) {
            return lchToRgb(hpluvToLch(tuple));
        }
    
        vec3 rgbToHpluv(vec3 tuple) {
            return lchToHpluv(rgbToLch(tuple));
        }
    
        vec3 luvToRgb(vec3 tuple){
            return xyzToRgb(luvToXyz(tuple));
        }

        // allow vec4's
        vec4   xyzToRgb(vec4 c) {return vec4(   xyzToRgb( vec3(c.x,c.y,c.z) ), c.a);}
        vec4   rgbToXyz(vec4 c) {return vec4(   rgbToXyz( vec3(c.x,c.y,c.z) ), c.a);}
        vec4   xyzToLuv(vec4 c) {return vec4(   xyzToLuv( vec3(c.x,c.y,c.z) ), c.a);}
        vec4   luvToXyz(vec4 c) {return vec4(   luvToXyz( vec3(c.x,c.y,c.z) ), c.a);}
        vec4   luvToLch(vec4 c) {return vec4(   luvToLch( vec3(c.x,c.y,c.z) ), c.a);}
        vec4   lchToLuv(vec4 c) {return vec4(   lchToLuv( vec3(c.x,c.y,c.z) ), c.a);}
        vec4 hsluvToLch(vec4 c) {return vec4( hsluvToLch( vec3(c.x,c.y,c.z) ), c.a);}
        vec4 lchToHsluv(vec4 c) {return vec4( lchToHsluv( vec3(c.x,c.y,c.z) ), c.a);}
        vec4 hpluvToLch(vec4 c) {return vec4( hpluvToLch( vec3(c.x,c.y,c.z) ), c.a);}
        vec4 lchToHpluv(vec4 c) {return vec4( lchToHpluv( vec3(c.x,c.y,c.z) ), c.a);}
        vec4   lchToRgb(vec4 c) {return vec4(   lchToRgb( vec3(c.x,c.y,c.z) ), c.a);}
        vec4   rgbToLch(vec4 c) {return vec4(   rgbToLch( vec3(c.x,c.y,c.z) ), c.a);}
        vec4 hsluvToRgb(vec4 c) {return vec4( hsluvToRgb( vec3(c.x,c.y,c.z) ), c.a);}
        vec4 rgbToHsluv(vec4 c) {return vec4( rgbToHsluv( vec3(c.x,c.y,c.z) ), c.a);}
        vec4 hpluvToRgb(vec4 c) {return vec4( hpluvToRgb( vec3(c.x,c.y,c.z) ), c.a);}
        vec4 rgbToHpluv(vec4 c) {return vec4( rgbToHpluv( vec3(c.x,c.y,c.z) ), c.a);}
        vec4   luvToRgb(vec4 c) {return vec4(   luvToRgb( vec3(c.x,c.y,c.z) ), c.a);}
        // allow 3 floats
        vec3   xyzToRgb(float x, float y, float z) {return   xyzToRgb( vec3(x,y,z) );}
        vec3   rgbToXyz(float x, float y, float z) {return   rgbToXyz( vec3(x,y,z) );}
        vec3   xyzToLuv(float x, float y, float z) {return   xyzToLuv( vec3(x,y,z) );}
        vec3   luvToXyz(float x, float y, float z) {return   luvToXyz( vec3(x,y,z) );}
        vec3   luvToLch(float x, float y, float z) {return   luvToLch( vec3(x,y,z) );}
        vec3   lchToLuv(float x, float y, float z) {return   lchToLuv( vec3(x,y,z) );}
        vec3 hsluvToLch(float x, float y, float z) {return hsluvToLch( vec3(x,y,z) );}
        vec3 lchToHsluv(float x, float y, float z) {return lchToHsluv( vec3(x,y,z) );}
        vec3 hpluvToLch(float x, float y, float z) {return hpluvToLch( vec3(x,y,z) );}
        vec3 lchToHpluv(float x, float y, float z) {return lchToHpluv( vec3(x,y,z) );}
        vec3   lchToRgb(float x, float y, float z) {return   lchToRgb( vec3(x,y,z) );}
        vec3   rgbToLch(float x, float y, float z) {return   rgbToLch( vec3(x,y,z) );}
        vec3 hsluvToRgb(float x, float y, float z) {return hsluvToRgb( vec3(x,y,z) );}
        vec3 rgbToHsluv(float x, float y, float z) {return rgbToHsluv( vec3(x,y,z) );}
        vec3 hpluvToRgb(float x, float y, float z) {return hpluvToRgb( vec3(x,y,z) );}
        vec3 rgbToHpluv(float x, float y, float z) {return rgbToHpluv( vec3(x,y,z) );}
        vec3   luvToRgb(float x, float y, float z) {return   luvToRgb( vec3(x,y,z) );}
        // allow 4 floats
        vec4   xyzToRgb(float x, float y, float z, float a) {return   xyzToRgb( vec4(x,y,z,a) );}
        vec4   rgbToXyz(float x, float y, float z, float a) {return   rgbToXyz( vec4(x,y,z,a) );}
        vec4   xyzToLuv(float x, float y, float z, float a) {return   xyzToLuv( vec4(x,y,z,a) );}
        vec4   luvToXyz(float x, float y, float z, float a) {return   luvToXyz( vec4(x,y,z,a) );}
        vec4   luvToLch(float x, float y, float z, float a) {return   luvToLch( vec4(x,y,z,a) );}
        vec4   lchToLuv(float x, float y, float z, float a) {return   lchToLuv( vec4(x,y,z,a) );}
        vec4 hsluvToLch(float x, float y, float z, float a) {return hsluvToLch( vec4(x,y,z,a) );}
        vec4 lchToHsluv(float x, float y, float z, float a) {return lchToHsluv( vec4(x,y,z,a) );}
        vec4 hpluvToLch(float x, float y, float z, float a) {return hpluvToLch( vec4(x,y,z,a) );}
        vec4 lchToHpluv(float x, float y, float z, float a) {return lchToHpluv( vec4(x,y,z,a) );}
        vec4   lchToRgb(float x, float y, float z, float a) {return   lchToRgb( vec4(x,y,z,a) );}
        vec4   rgbToLch(float x, float y, float z, float a) {return   rgbToLch( vec4(x,y,z,a) );}
        vec4 hsluvToRgb(float x, float y, float z, float a) {return hsluvToRgb( vec4(x,y,z,a) );}
        vec4 rgbToHslul(float x, float y, float z, float a) {return rgbToHsluv( vec4(x,y,z,a) );}
        vec4 hpluvToRgb(float x, float y, float z, float a) {return hpluvToRgb( vec4(x,y,z,a) );}
        vec4 rgbToHpluv(float x, float y, float z, float a) {return rgbToHpluv( vec4(x,y,z,a) );}
        vec4   luvToRgb(float x, float y, float z, float a) {return   luvToRgb( vec4(x,y,z,a) );}

        /* END HSLUV-GLSL */

        void main(){
            vec2 uv = vec2(gl_FragCoord.x/u_resolution.x, gl_FragCoord.y/u_resolution.y);

            float t = u_time / 2000.0;

            /* Graphics */
            vec3 pixelColor =
                vec3(
                    hpluvToRgb(
                        vec3(
                            mod(uv.x * 360.0 + uv.y * t * 1000.0, 360.0),
                            100.0,
                            50.0
                        )
                    )
                );

            gl_FragColor = vec4(pixelColor, mod(uv.x * 100.0 + uv.y * 256.0 + t * 10.0, 2.0)/2.0);
        }
    |]


white : Vec3
white =
    vec3 1 1 1


mesh : Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec3 -1 -1 0) white
          , Vertex (vec3 1 1 0) white
          , Vertex (vec3 1 -1 0) white
          )
        , ( Vertex (vec3 -1 -1 0) white
          , Vertex (vec3 -1 1 0) white
          , Vertex (vec3 1 1 0) white
          )
        ]


palView : Model -> Element Msg
palView model =
    El.column
        [ El.centerY
        , El.width El.fill
        , El.height El.fill
        , El.padding 20
        , El.spacing 40
        ]
        [ El.row
            [ El.centerX ]
            [ el
                [ El.paddingEach { edges | top = 3, bottom = 3 }
                , Font.color light.cyan
                , Bg.color dim.cyan
                ]
              <|
                El.text <|
                    "Hello"
            , El.text " ( "
            , el [ Font.color light.yellow ] <|
                El.text <|
                    String.fromInt model.width
            , El.text <| ", "
            , el [ Font.color light.yellow ] <|
                El.text <|
                    String.fromInt model.height
            , El.text <| " ) "
            , el [ Font.color light.blue ] <|
                El.text <|
                    String.fromFloat model.count
            ]
        ]


graphView : Model -> Element Msg
graphView model =
    let
        zoomEvents : List (Ts.Attribute Msg)
        zoomEvents =
            case model.graph of
                Init _ ->
                    []

                Ready { zoom } ->
                    Zoom.events zoom ZoomMsg

        zoomTransformAttr : Ts.Attribute Msg
        zoomTransformAttr =
            case model.graph of
                Init _ ->
                    Tsa.class []

                Ready { zoom } ->
                    Zoom.transform zoom
    in
    El.html <|
        Ts.svg
            [ Tsa.id elementId
            , Tsa.width <| Ts.Percent 100
            , Tsa.height <| Ts.Percent 100
            ]
            [ Ts.defs [] [ arrowhead ]
            , Ts.rect
                ([ Tsa.width <| Ts.Percent 100
                 , Tsa.height <| Ts.Percent 100
                 , Tsa.fill <| Ts.Paint dim.black
                 , Tsa.cursor Ts.CursorMove
                 ]
                    ++ zoomEvents
                )
                []
            , Ts.g
                [ zoomTransformAttr ]
                [ renderGraph model.graph ]
            ]


bigsq : El.Color -> Element Msg
bigsq col =
    el
        [ Bg.color col
        , El.width <| El.px <| 2 * pixel
        , El.height <| El.px <| 2 * pixel
        ]
        El.none


sq : El.Color -> Element Msg
sq col =
    el
        [ Bg.color col
        , El.width <| El.px pixel
        , El.height <| El.px pixel
        ]
        El.none


pixel : Int
pixel =
    12



-- ▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀
{- Color Scheme -}


dim : Pal El.Color
dim =
    { black = El.rgb255 0x00 0x00 0x00 -- #000000
    , red = El.rgb255 0x53 0x31 0x46 -- #533146
    , green = El.rgb255 0x1F 0x5F 0x67 -- #1f5f67
    , yellow = El.rgb255 0xD4 0x88 0x5F -- #d4885f
    , blue = El.rgb255 0x1A 0x3F 0x56 -- #1a3f56
    , magenta = El.rgb255 0x75 0x4B 0x5A -- #754b5a
    , cyan = El.rgb255 0x26 0x80 0x6C -- #26806c
    , white = El.rgb255 0xCB 0xA9 0x9F -- #cba99f
    }


mid : Pal El.Color
mid =
    { black = El.rgb255 0x19 0x1A 0x26 -- #191a26
    , red = El.rgb255 0x8C 0x41 0x4C -- #8c414c
    , green = El.rgb255 0x6E 0xA1 0x6B -- #6ea16b
    , yellow = El.rgb255 0xE5 0xB3 0x7C -- #e5b37c
    , blue = El.rgb255 0x15 0x68 0x7F -- #15687f
    , magenta = El.rgb255 0x9E 0x68 0x6C -- #9e686c
    , cyan = El.rgb255 0x4B 0xB4 0x9D -- #4bb49d
    , white = El.rgb255 0xE1 0xCA 0xBF -- #e1cabf
    }


light : Pal El.Color
light =
    { black = El.rgb255 0x27 0x29 0x3A -- #27293a
    , red = El.rgb255 0xBA 0x61 0x54 -- #ba6154
    , green = El.rgb255 0xAE 0xBE 0x78 -- #aebe78
    , yellow = El.rgb255 0xF0 0xDB 0xA3 -- #f0dba3
    , blue = El.rgb255 0x2C 0x91 0x96 -- #2c9196
    , magenta = El.rgb255 0xB4 0x89 0x83 -- #b48983
    , cyan = El.rgb255 0xA2 0xD7 0xBF -- #a2d7bf
    , white = El.rgb255 0xF3 0xEC 0xDE -- #f3ecde
    }


elementId : String
elementId =
    "exercise-graph"


edgeColor : Ts.Paint
edgeColor =
    Ts.Paint dim.white


nodeColor : Ts.Paint
nodeColor =
    Ts.Paint mid.green


nodeTextColor : Ts.Paint
nodeTextColor =
    Ts.Paint light.yellow



-- Types


{-| In order to correctly calculate the node positions, we need to know the
coordinates of the svg element. The simulation is started when we
receive them.
-}
type GraphModel
    = Init (Graph Entity ())
    | Ready ReadyState


type alias ReadyState =
    { drag : Maybe Drag
    , graph : Graph Entity ()
    , simulation : Force.State NodeId
    , zoom : Zoom

    -- The position and dimensions of the svg element.
    , element : SvgElement

    -- If you immediately show the graph when moving from `Init` to `Ready`,
    -- you will briefly see the nodes in the upper left corner before the first
    -- simulation tick positions them in the center. To avoid this sudden jump,
    -- `showGraph` is initialized with `False` and set to `True` with the first
    -- `Tick`.
    , showGraph : Bool
    }


type alias Drag =
    { current : ( Float, Float )
    , index : NodeId
    , start : ( Float, Float )
    }


type alias SvgElement =
    { height : Float
    , width : Float
    , x : Float
    , y : Float
    }


type alias Entity =
    Force.Entity NodeId { value : String }


renderGraph : GraphModel -> Svg Msg
renderGraph model =
    case model of
        Init _ ->
            Ts.text ""

        Ready { graph, showGraph } ->
            if showGraph then
                Ts.g
                    []
                    [ Graph.edges graph
                        |> List.map (linkElement graph)
                        |> Ts.g [ Tsa.class [ "links" ] ]
                    , Graph.nodes graph
                        |> List.map nodeElement
                        |> Ts.g [ Tsa.class [ "nodes" ] ]
                    ]

            else
                Ts.text ""


nodeRad : Float
nodeRad =
    25.0


{-| Draws a single vertex (node).
-}
nodeElement : Node Entity -> Svg Msg
nodeElement node =
    Ts.g [ Tsa.class [ "node" ] ]
        [ Ts.circle
            ([ TsPx.r <| nodeRad
             , Tsa.fill <| Ts.Paint <| nodeLabelToColor node.label.value
             , Tsa.cursor Ts.CursorPointer

             -- The coordinates are initialized and updated by `Force.simulation`
             -- and `Force.tick`, respectively.
             , TsPx.cx node.label.x
             , TsPx.cy node.label.y

             -- Add event handler for starting a drag on the node.
             , onMouseDown node.id
             , Touch.onStart (touchCoords >> DragStart node.id)
             , Touch.onMove (touchCoords >> DragAt)
             , Touch.onEnd (touchCoords >> DragEnd)
             ]
                ++ (if node.label.value == "Dim Black" then
                        [ TsPx.strokeWidth 3
                        , Tsa.stroke <| Ts.Paint mid.black
                        ]

                    else
                        []
                   )
            )
            []

        --[ Ts.title [] [ Ts.text node.label.value ] ]
        {- , Ts.text_
           [ -- Align text label at the center of the circle.
             TsPx.dx <| node.label.x
           , TsPx.dy <| node.label.y
           , Tsa.alignmentBaseline Ts.AlignmentCentral
           , Tsa.textAnchor Ts.AnchorMiddle

           -- styling
           , Tsa.fontSize <| Ts.Px 12
           , Tsa.fill nodeTextColor

           -- Setting pointer events to none allows the user to click on the
           -- element behind the text, so in this case the circle. If you
           -- position the text label outside of the circle, you also should
           -- do this, so that drag and zoom operations are not interrupted
           -- when the cursor is above the text.
           , Tsa.pointerEvents "none"
           ]
           [ Ts.text node.label.value ]
        -}
        ]


nodeLabelToColor : String -> Color
nodeLabelToColor label =
    case label of
        "Dim Black" ->
            dim.black

        "Dim Red" ->
            dim.red

        "Dim Green" ->
            dim.green

        "Dim Yellow" ->
            dim.yellow

        "Dim Blue" ->
            dim.blue

        "Dim Magenta" ->
            dim.magenta

        "Dim Cyan" ->
            dim.cyan

        "Dim White" ->
            dim.white

        "Mid Black" ->
            mid.black

        "Mid Red" ->
            mid.red

        "Mid Green" ->
            mid.green

        "Mid Yellow" ->
            mid.yellow

        "Mid Blue" ->
            mid.blue

        "Mid Magenta" ->
            mid.magenta

        "Mid Cyan" ->
            mid.cyan

        "Mid White" ->
            mid.white

        "Light Black" ->
            light.black

        "Light Red" ->
            light.red

        "Light Green" ->
            light.green

        "Light Yellow" ->
            light.yellow

        "Light Blue" ->
            light.blue

        "Light Magenta" ->
            light.magenta

        "Light Cyan" ->
            light.cyan

        "Light White" ->
            light.white

        _ ->
            El.rgb 1 0 1


{-| This function draws the lines between the vertices.
-}
linkElement : Graph Entity () -> Edge () -> Svg msg
linkElement graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 "") <|
                Maybe.map (.node >> .label) <|
                    Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 "") <|
                Maybe.map (.node >> .label) <|
                    Graph.get edge.to graph
    in
    Ts.line
        [ TsPx.x1 source.x
        , TsPx.y1 source.y
        , TsPx.x2 target.x
        , TsPx.y2 target.y
        , TsPx.strokeWidth 4
        , Tsa.stroke <|
            Ts.Paint <|
                nodeLabelToColor target.value

        --, Tsa.markerEnd "url(#arrowhead)"
        ]
        []



-- Definitions


{-| This is the definition of the arrow head that is displayed at the end of
the edges.

It is a child of the svg `defs` element and can be referenced by its id with
`url(#arrowhead)`.

-}
arrowhead : Svg msg
arrowhead =
    Ts.marker
        [ Tsa.id "arrowhead"
        , Tsa.orient "auto"
        , Tsa.markerWidth <| Ts.Px 8.0
        , Tsa.markerHeight <| Ts.Px 8.0
        , Tsa.refX <| String.fromFloat <| nodeRad * 1.25
        , Tsa.refY "4"
        ]
        [ Ts.polygon
            [ Tsa.points [ ( 0, 0 ), ( 8, 4 ), ( 0, 8 ) ]
            , Tsa.fill edgeColor
            ]
            []
        ]



-- Events and tasks


{-| This is the event handler that handles clicks on the vertices (nodes).

The event catches the `clientPos`, which is a tuple with the
`MouseEvent.clientX` and `MouseEvent.clientY` values. These coordinates are
relative to the client area (browser viewport).

If the graph is positioned anywhere else than at the coordinates `(0, 0)`, the
svg element position must be subtracted when setting the node position. This is
handled in the update function by calling the `shiftPosition` function.

-}
onMouseDown : NodeId -> Ts.Attribute Msg
onMouseDown index =
    Mouse.onDown (.clientPos >> DragStart index)


{-| Generates coordinates from a Touch.Event.
-}
touchCoords : Touch.Event -> ( Float, Float )
touchCoords touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        -- .screenPos
        |> Maybe.withDefault ( 0, 0 )



--|> (\( x, y ) -> ( x - origX, y - origY ))


{-| This function returns a command to retrieve the position of the svg element.
-}
getElementPosition : Cmd Msg
getElementPosition =
    Task.attempt ReceiveElementPosition (Dom.getElement elementId)


edges : { top : Int, bottom : Int, left : Int, right : Int }
edges =
    { top = 0
    , bottom = 0
    , left = 0
    , right = 0
    }



-- Data


{-| This is the dataset for the graph.

The names are random. The edges of the dataset are derived from
<http://konect.uni-koblenz.de/networks/moreno_highschool>.

-}
graphData : Graph String ()
graphData =
    Graph.fromNodeLabelsAndEdgePairs
        [ "Dim Black" -- 0
        , "Dim Red" -- 1
        , "Dim Green" -- 2
        , "Dim Yellow" -- 3
        , "Dim Blue" -- 4
        , "Dim Magenta" -- 5
        , "Dim Cyan" -- 6
        , "Dim White" -- 7
        , "Mid Black" -- 8
        , "Mid Red" -- 9
        , "Mid Green" -- 10
        , "Mid Yellow" -- 11
        , "Mid Blue" -- 12
        , "Mid Magenta" -- 13
        , "Mid Cyan" -- 14
        , "Mid White" -- 15
        , "Light Black" -- 16
        , "Light Red" -- 17
        , "Light Green" -- 18
        , "Light Yellow" -- 19
        , "Light Blue" -- 20
        , "Light Magenta" -- 21
        , "Light Cyan" -- 22
        , "Light White" -- 23
        ]
        [ ( 0, 8 )
        , ( 8, 16 )
        , ( 16, 4 )
        , ( 16, 1 )
        , ( 4, 2 )
        , ( 4, 12 )
        , ( 1, 5 )
        , ( 1, 9 )
        , ( 2, 6 )
        , ( 12, 20 )
        , ( 6, 14 )
        , ( 6, 10 )
        , ( 20, 14 )
        , ( 14, 22 )
        , ( 22, 23 )
        , ( 10, 18 )
        , ( 18, 19 )
        , ( 19, 23 )
        , ( 9, 17 )
        , ( 9, 13 )
        , ( 17, 3 )
        , ( 3, 11 )
        , ( 11, 19 )
        , ( 5, 13 )
        , ( 13, 21 )
        , ( 21, 7 )
        , ( 7, 15 )
        , ( 15, 23 )
        , ( 3, 7 )
        ]
