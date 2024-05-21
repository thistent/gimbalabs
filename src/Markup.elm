module Markup exposing (..)

-- import MarkdownAst as Ast

import Browser.Dom exposing (Viewport)
import Docs exposing (..)
import Markdown.Block as Md
import Markdown.Html as MdHtml
import Markdown.Parser as Md
import Markdown.Renderer as Md
import Style exposing (..)
import Types exposing (..)
import Ui exposing (..)
import Ui.Background as Bg
import Ui.Border as Border
import Ui.Font as Font



-- Markdown Stuff --
{-
   renderMd : Model -> Viewport -> String -> Element Msg
   renderMd model vp str =
       str
           |> Md.parse
           |> Result.withDefault
               [ Md.Paragraph [ Md.Text "Markdown parsing error!" ] ]
           --|> Md.render (markdownRenderer model vp)
           --|> Md.render Md.defaultHtmlRenderer
           --|> Result.map (List.map html)
           |> Md.render (mdRenderer model vp)
           |> Result.withDefault
               [ text "Markdown rendering error!" ]
           |> (\stuff ->
                   textColumn
                       [ spacing spcNum
                       , width <| px <| round <| vp.viewport.width - 2 * spcNum - 4 * lineSize
                       ]
                       stuff
              )
-}
-- Testing --


type MdToken
    = Heading Float (List MdToken) (List MdToken)
    | Paragraph (List MdToken)
    | BlockQuote (List MdToken)
    | Text String
    | CodeSpan String
    | Strong (List MdToken)
    | Emphasis (List MdToken)
    | StrongEmphasis (List MdToken)
    | Strike (List MdToken)
    | LineBreak
    | Link { title : Maybe String, destination : String } (List MdToken)
    | Image { alt : String, src : String, title : Maybe String }
    | ListBlock (Maybe Int) (List MdToken)
    | ListItem { task : Md.Task, content : List MdToken, children : List MdToken }
    | CodeBlock { body : String, language : Maybe String }
    | ThematicBreak
    | Table (List MdToken)
    | TableHeader (List MdToken)
    | TableBody (List MdToken)
    | TableRow (List MdToken)
    | TableCell (Maybe Md.Alignment) (List MdToken)
    | TableHeaderCell (Maybe Md.Alignment) (List MdToken)


splitWhile : (a -> Bool) -> List a -> ( List a, List a )
splitWhile pred =
    let
        sw : List a -> List a -> ( List a, List a )
        sw acc list =
            case list of
                [] ->
                    ( List.reverse acc, [] )

                x :: xs ->
                    if pred x then
                        sw (x :: acc) xs

                    else
                        ( List.reverse acc, list )
    in
    sw []


tokenize : String -> List MdToken
tokenize mdDoc =
    mdDoc
        |> Md.parse
        |> Result.withDefault
            [ Md.Paragraph [ Md.Text "Markdown parsing error!" ] ]
        |> Md.render mdTokenizer
        |> Result.withDefault
            [ Text "Markdown rendering error!" ]


renderMd : Model -> Viewport -> String -> Element Msg
renderMd model vp str =
    str
        |> tokenize
        |> nestHeadings
        |> List.map (renderToken model vp)
        |> textColumn [ fillSpace, spacing spcNum ]


mdTokenizer : Md.Renderer MdToken
mdTokenizer =
    { heading =
        {-
           { level : HeadingLevel
           , rawText : String
           , children : List view
           }
           -> view
        -}
        \{ level, children } ->
            case level of
                Md.H1 ->
                    Heading 1 children []

                Md.H2 ->
                    Heading 2 children []

                Md.H3 ->
                    Heading 3 children []

                Md.H4 ->
                    Heading 4 children []

                Md.H5 ->
                    Heading 5 children []

                Md.H6 ->
                    Heading 6 children []
    , paragraph =
        -- List view -> view
        Paragraph
    , blockQuote =
        -- List view -> view
        BlockQuote
    , html =
        -- Renderer (List view -> view)
        MdHtml.oneOf []
    , text =
        -- String -> view
        Text
    , codeSpan =
        -- String -> view
        CodeSpan
    , strong =
        -- List view -> view
        Strong
    , emphasis =
        -- List view -> view
        Emphasis
    , strikethrough =
        -- List view -> view
        Strike
    , hardLineBreak =
        -- view
        LineBreak
    , link =
        {-
           { title : Maybe String
           , destination : String
           }
           -> List view
           -> view
        -}
        Link
    , image =
        {-
           { alt : String
           , src : String
           , title : Maybe String
           }
           -> view
        -}
        Image
    , unorderedList =
        -- List (ListItem view) -> view
        \list ->
            let
                convertListItem : Md.ListItem MdToken -> MdToken
                convertListItem li =
                    case li of
                        Md.ListItem task kids ->
                            let
                                ( content, children ) =
                                    splitWhile isInline kids
                            in
                            ListItem
                                { task = task
                                , content = content
                                , children = children
                                }

                l2 : List MdToken
                l2 =
                    list
                        |> List.map convertListItem
            in
            ListBlock Nothing l2
    , orderedList =
        -- Int -> List (List view) -> view
        let
            toListItems : List (List MdToken) -> List MdToken
            toListItems ls =
                ls
                    |> List.map
                        (\l ->
                            let
                                ( content, children ) =
                                    splitWhile isInline l
                            in
                            ListItem
                                { task = Md.NoTask
                                , content = content
                                , children = children
                                }
                        )
        in
        \i ls -> ListBlock (Just i) (toListItems ls)
    , codeBlock =
        {- { body : String
           , language : Maybe String
           }
           -> view
        -}
        CodeBlock
    , thematicBreak =
        -- view
        ThematicBreak
    , table =
        -- List view -> view
        Table
    , tableHeader =
        -- List view -> view
        TableHeader
    , tableBody =
        -- List view -> view
        TableBody
    , tableRow =
        -- List view -> view
        TableRow
    , tableCell =
        -- Maybe Alignment -> List view -> view
        TableCell
    , tableHeaderCell =
        -- Maybe Alignment -> List view -> view
        TableHeaderCell
    }


nestHeadings : List MdToken -> List MdToken
nestHeadings tokens =
    case tokens of
        tok :: toks ->
            case tok of
                Heading level title subsections ->
                    let
                        ( inside, outside ) =
                            splitWhile (isUnderHeading tok) toks
                    in
                    Heading level title (subsections ++ nestHeadings inside) :: nestHeadings outside

                _ ->
                    tok :: nestHeadings toks

        [] ->
            []


isUnderHeading : MdToken -> MdToken -> Bool
isUnderHeading h x =
    case h of
        Heading hi _ _ ->
            case x of
                Heading xi _ _ ->
                    hi < xi

                _ ->
                    True

        _ ->
            False


renderToken : Model -> Viewport -> MdToken -> Element Msg
renderToken model vp tok =
    let
        p : Attribute Msg
        p =
            batch
                [ spacing 10
                , width fill
                ]

        wr : Attribute Msg
        wr =
            batch
                []

        render : List MdToken -> List (Element Msg)
        render =
            List.map (renderToken model vp)
    in
    case tok of
        Heading level title subsections ->
            if level == 1.0 then
                textColumn [ width fill, spacing spcNum ] <|
                    paragraph
                        [ Border.widthEach { edges | bottom = lineSize * 2 }
                        , Border.roundEach { corners | topLeft = spcNum // 4, topRight = spcNum // 4 }
                        , padding <| spcNum // 2
                        , Bg.color <| Style.mix 0.9 model.color.fg model.color.bg
                        , width fill
                        , Font.size <| round <| spcNum * (1.0 - ((level - 1) * 0.1))
                        , spacing <| spcNum // 2
                        ]
                        (render title)
                        :: render subsections

            else
                mdGroup level model.color (render title) (render subsections)

        Paragraph toks ->
            paragraph
                [ p
                ]
            <|
                List.map (renderToken model vp) toks

        BlockQuote toks ->
            paragraph [ p ] <|
                List.map (renderToken model vp) toks

        Text str ->
            paragraph [ width fill ]
                [ wrappedRow
                    [ wr
                    ]
                    [ text str ]
                ]

        CodeSpan str ->
            paragraph
                [ width fill
                , Bg.color <| Style.mix 0.08 model.color.bg model.color.fg
                , paddingXY 5 2
                ]
                [ wrappedRow
                    [ wr
                    , Font.family [ Font.monospace ]
                    ]
                    [ text str ]
                ]

        Strong toks ->
            paragraph [ width fill ]
                [ wrappedRow [ wr, Font.bold ] <|
                    List.map (renderToken model vp) toks
                ]

        Emphasis toks ->
            paragraph [ width fill ]
                [ wrappedRow [ wr, Font.italic ] <|
                    List.map (renderToken model vp) toks
                ]

        StrongEmphasis toks ->
            paragraph [ width fill ]
                [ wrappedRow [ wr, Font.bold, Font.italic ] <|
                    List.map (renderToken model vp) toks
                ]

        Strike toks ->
            paragraph [ width fill ]
                [ wrappedRow [ wr, Font.strike ] <|
                    List.map (renderToken model vp) toks
                ]

        LineBreak ->
            paragraph [ p ] [ text "<br>" ]

        Link { title, destination } toks ->
            iconButton model
                (GetDoc destination)
                Nothing
            <|
                paragraph [] <|
                    render toks

        Image { alt, src, title } ->
            column
                [ width <| px <| round <| vp.viewport.width * 0.33
                , paddingEach { edges | right = spcNum }
                ]
                [ image
                    [ width fill
                    ]
                    { src = src
                    , description = alt
                    }
                , case title of
                    Just t ->
                        el
                            [ centerX
                            , Bg.color <| Style.mix 0.05 model.color.bg model.color.fg
                            , padding <| spcNum // 2
                            , Font.size <| spcNum // 2

                            -- TESTING --
                            , Font.bold
                            , Font.italic
                            , Font.alignRight
                            , width fill
                            , Border.roundEach { corners | bottomLeft = spcNum // 4, bottomRight = spcNum // 4 }
                            ]
                        <|
                            text t

                    Nothing ->
                        none

                --, el [ height <| px <| spcNum ] none
                ]

        ListBlock maybeIndex toks ->
            textColumn
                [ spacing <| spcNum // 2
                , width fill
                , paddingEach { edges | top = spcNum // 2 }
                ]
                (render toks)

        ListItem { task, content, children } ->
            mdItem model.color task (render content) (render children)

        CodeBlock { body, language } ->
            let
                lang : Element Msg
                lang =
                    case language of
                        Just l ->
                            el
                                [ Font.size <| spcNum // 2
                                , padding <| spcNum // 4
                                , Bg.color <| Style.mix 0.15 model.color.bg model.color.fg
                                , width fill
                                ]
                            <|
                                text l

                        Nothing ->
                            none
            in
            column
                [ fillSpace
                , Font.family [ Font.monospace ]
                ]
                [ lang
                , link
                    [ width fill
                    , Bg.color <|
                        Style.mix 0.05 model.color.bg model.color.fg
                    ]
                    { url = ""
                    , label =
                        column
                            [ width fill
                            , scrollbarX
                            , Bg.color <|
                                Style.mix 0.05 model.color.bg model.color.fg
                            ]
                            (body
                                |> String.split "\n"
                                |> (\xs ->
                                        List.take (List.length xs - 1) xs
                                            |> List.indexedMap
                                                (\i str ->
                                                    row
                                                        [ width fill
                                                        , height <| px <| spcNum
                                                        ]
                                                        [ el
                                                            [ Bg.color <| Style.mix 0.1 model.color.bg model.color.fg
                                                            , paddingEach { edges | right = spcNum // 4 }
                                                            , height fill
                                                            ]
                                                          <|
                                                            el [ centerY, width fill ] <|
                                                                (i + 1 |> String.fromInt |> String.padLeft 4 ' ' |> text)
                                                        , el
                                                            [ Bg.color <| Style.mix 0.05 model.color.bg model.color.fg
                                                            , paddingEach { edges | left = spcNum // 4 }
                                                            , fillSpace
                                                            ]
                                                          <|
                                                            el [ centerY, width fill ] <|
                                                                text str
                                                        ]
                                                )
                                   )
                            )
                    }
                ]

        ThematicBreak ->
            hBar

        Table toks ->
            column
                [ Bg.color <| Style.mix 0.05 model.color.bg model.color.fg
                , spacing <| spcNum // 2
                , padding <| spcNum // 2
                ]
                (render toks)

        TableHeader toks ->
            row
                [ width fill
                , spacing <| spcNum // 2
                , paddingEach { edges | bottom = spcNum // 2 }
                , Border.widthEach { edges | bottom = 2 }
                ]
                (render toks)

        TableBody toks ->
            column [ width fill, spacing <| spcNum // 2 ] (render toks)

        TableRow toks ->
            row [ width fill, spacing <| spcNum // 2 ] (render toks)

        TableCell maybeAlign toks ->
            paragraph
                [ width fill
                , spacing <| spcNum // 2
                , case maybeAlign of
                    Just arg ->
                        case arg of
                            Md.AlignLeft ->
                                alignLeft

                            Md.AlignCenter ->
                                centerX

                            Md.AlignRight ->
                                alignRight

                    Nothing ->
                        batch []
                ]
            <|
                render toks

        TableHeaderCell maybeAlign toks ->
            paragraph
                [ width fill
                , Font.bold
                , spacing <| spcNum // 2
                , case maybeAlign of
                    Just arg ->
                        case arg of
                            Md.AlignLeft ->
                                alignLeft

                            Md.AlignCenter ->
                                centerX

                            Md.AlignRight ->
                                alignRight

                    Nothing ->
                        batch []
                ]
                (render toks)



-- Predicates --


isInline : MdToken -> Bool
isInline tok =
    case tok of
        Text _ ->
            True

        Link _ _ ->
            True

        Image _ ->
            True

        CodeSpan _ ->
            True

        Emphasis _ ->
            True

        StrongEmphasis _ ->
            True

        Strike _ ->
            True

        -- LineBreak ->
        --     True
        _ ->
            False



{-

    type MdAst
         = AstSection Ast.Section
         | AstBlock Ast.BlockElement
         | AstInline Ast.InlineElement
         | AstListItem Ast.ListItem


    renderAst : MdAst -> Element Msg
    renderAst node =
            case node of
                AstSection section ->
                    paragraph [ width fill ] [ text <| Debug.toString section ]

                AstBlock block ->
                    paragraph [ width fill ] [ text <| Debug.toString block ]

                AstInline inline ->
                    paragraph [ width fill ] [ text <| Debug.toString inline ]

                AstListItem li ->
                    paragraph [ width fill ] [ text <| Debug.toString li ]



   unorderedList =
         -- List (ListItem view) -> view
         \items ->
             items
                 |> List.map
                     (\it ->
                         case it of
                             Md.ListItem task xs ->
                                 case task of
                                     Md.NoTask ->
                                         row [ a, w ]
                                             [ el [ a, b ] <| text " ⏺ "
                                             , textColumn [ a, w ] xs
                                             ]

                                     Md.IncompleteTask ->
                                         row [ a, w ]
                                             [ el [ a, b ] <| text "[ ]"
                                             , textColumn [ a, w ] xs
                                             ]

                                     Md.CompletedTask ->
                                         row [ a, w ]
                                             [ el [ a, b ] <| text "[X]"
                                             , textColumn [ a, w ] xs
                                             ]
                     )
                 |> textColumn [ a, w ]

   orderedList =
         -- Int -> List (List view) -> view
         \startIndex items ->
             items
                 |> List.indexedMap
                     (\index xs ->
                         row [ a, w ]
                             [ (String.fromInt <| index + startIndex)
                                 ++ "."
                                 |> (\s -> el [ a, b ] <| text s)
                             , column [ a, w ] xs
                             ]
                     )
                 |> column [ a, w ]

-}


mdItem : Pal -> Md.Task -> List (Element Msg) -> List (Element Msg) -> Element Msg
mdItem color task content children =
    row [ width fill, spacing <| spcNum // 2 ]
        [ el [ alignTop ] <|
            text <|
                case task of
                    Md.NoTask ->
                        "■"

                    Md.IncompleteTask ->
                        -- "⬚"
                        "⬚"

                    Md.CompletedTask ->
                        -- "▨"
                        "▨"

        -- ■▨▣□⬚▨▩●▪
        , paragraph [ width fill ] <| content ++ children
        ]


mdGroup : Float -> Pal -> List (Element Msg) -> List (Element Msg) -> Element Msg
mdGroup level pal hd els =
    let
        resize : Int
        resize =
            round <| spcNum * (1.0 - ((level - 1) * 0.1))
    in
    textColumn
        [ Border.widthEach
            { edges
                | left = round <| lineSize * 2
            }
        , Border.roundEach
            { corners
                | topLeft = spcNum // 4
                , bottomLeft = spcNum // 4
            }
        , paddingEach { edges | bottom = spcNum // 2 }
        , spacing <| spcNum
        , width fill
        ]
        [ paragraph
            [ Font.size resize

            --, Font.underline
            , spacing <| resize // 4
            , paddingXY spcNum <| resize // 3
            , Border.roundEach
                { corners
                    | topRight = resize // 4
                    , bottomRight = resize // 4
                }
            , moveDown <| lineSize * 3.0
            , Bg.color <| Style.mix 0.9 pal.fg pal.bg
            ]
            hd
        , textColumn
            [ width fill
            , paddingEach { edges | left = spcNum }
            , spacing spcNum
            ]
            els
        ]



{-
   mdRenderer : Model -> Viewport -> Md.Renderer (Element Msg)
   mdRenderer model vp =
       let
           b : Attribute Msg
           b =
               batch [ Font.family [ Font.monospace ], Font.bold ]

           a : Attribute Msg
           a =
               batch
                   [ alignTop
                   , spacingXY (spcNum // 2) spcNum
                   ]

           w : Attribute Msg
           w =
               width fill

           vw =
               width <| px <| round <| vp.viewport.width - 2 * spcNum
       in
       { heading =
           -- {HeadingLevel, String, List view} -> view
           \{ level, rawText, children } ->
               let
                   h : Float -> Element Msg
                   h i =
                       paragraph [ width fill ]
                           [ wrappedRow
                               [ Font.size <|
                                   round <|
                                       spcNum
                                           * (1.0 - ((i - 1) * 0.1))
                               , Bg.color <|
                                   Style.mix 0.9 model.color.fg model.color.bg
                               , paddingXY (spcNum // 2) (spcNum // 3)
                               , spacing <| spcNum // 2
                               , width fill
                               ]
                               [ el
                                   [ alignTop
                                   , Font.color <| Style.mix 0.6 model.color.fg model.color.bg
                                   , Font.letterSpacing 0
                                   ]
                                 <|
                                   text <|
                                       String.repeat (i |> round) "#"
                               , paragraph
                                   [ fillSpace
                                   , spacing <| spcNum // 2
                                   ]
                                   children
                               ]
                           ]
               in
               case level of
                   Md.H1 ->
                       h 1

                   Md.H2 ->
                       h 2

                   Md.H3 ->
                       h 3

                   Md.H4 ->
                       h 4

                   Md.H5 ->
                       h 5

                   Md.H6 ->
                       h 6
       , paragraph =
           -- List view -> view
           \list ->
               paragraph
                   [ spacing <| spcNum // 2
                   , width fill
                   ]
                   list
       , blockQuote =
           -- List view -> view
           \list ->
               paragraph
                   [ width fill
                   ]
                   list
       , html =
           -- Renderer (List view -> view)
           MdHtml.oneOf []
       , text =
           -- String -> view
           text
       , codeSpan =
           -- String -> view
           \str ->
               wrappedRow
                   [ Font.family [ Font.monospace ]
                   , Bg.color <| Style.mix 0.05 model.color.bg model.color.fg
                   , padding 5
                   ]
                   [ text str ]
       , strong =
           -- List view -> view
           wrappedRow
               [ Font.bold
               , width fill
               ]
       , emphasis =
           -- List view -> view
           wrappedRow
               [ Font.italic
               , width fill
               ]
       , strikethrough =
           -- List view -> view
           wrappedRow
               [ Font.strike
               , width fill
               ]
       , hardLineBreak =
           -- view
           hBar
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
                   (GetDoc destination)
                   Nothing
               <|
                   paragraph [] list
       , image =
           {-
              { alt : String
              , src : String
              , title : Maybe String
              }
              -> view
           -}
           \{ alt, src, title } ->
               column
                   [ vp
                       |> .viewport
                       |> .width
                       |> (\x -> x * 0.5)
                       |> round
                       |> px
                       |> width

                   --width fill
                   , centerX
                   , alignRight
                   ]
                   [ image
                       [ width fill
                       , Bg.color <| Style.mix 0.05 model.color.bg model.color.fg
                       , centerX
                       ]
                       { src = src
                       , description = alt
                       }
                   , case title of
                       Just t ->
                           column [ width fill ]
                               [ paragraph
                                   [ centerX
                                   , Bg.color <| Style.mix 0.05 model.color.bg model.color.fg
                                   , padding <| spcNum // 2
                                   , Font.size <| spcNum // 2

                                   -- TESTING --
                                   , Font.bold
                                   , Font.italic
                                   , Font.alignRight
                                   ]
                                   [ text t ]
                               , el
                                   [ width fill
                                   , height <| px <| spcNum // 2
                                   , Bg.color <| rgba 0 0 0 0
                                   ]
                                   none
                               ]

                       Nothing ->
                           none
                   ]
       , unorderedList =
           -- List (ListItem view) -> view
           \items ->
               items
                   |> List.map
                       (\it ->
                           case it of
                               Md.ListItem task xs ->
                                   case task of
                                       Md.NoTask ->
                                           row [ a, w ]
                                               [ el [ a, b ] <| text "⏺"
                                               , textColumn [ a, w ] xs
                                               ]

                                       Md.IncompleteTask ->
                                           row [ a, w ]
                                               [ el [ a, b ] <| text "[ ]"
                                               , textColumn [ a, w ] xs
                                               ]

                                       Md.CompletedTask ->
                                           row [ a, w ]
                                               [ el [ a, b ] <| text "[X]"
                                               , textColumn [ a, w ] xs
                                               ]
                       )
                   |> textColumn [ a, w ]
       , orderedList =
           -- Int -> List (List view) -> view
           \startIndex items ->
               items
                   |> List.indexedMap
                       (\index xs ->
                           row [ a, w ]
                               [ (String.fromInt <| index + startIndex)
                                   ++ "."
                                   |> (\s -> el [ a, b ] <| text s)
                               , column [ a, w ] xs
                               ]
                       )
                   |> column [ a, w ]
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
                                   [ Font.size <| spcNum // 2
                                   , padding <| spcNum // 4
                                   , Bg.color <|
                                       Style.mix 0.1 model.color.bg model.color.fg
                                   , width fill
                                   ]
                               <|
                                   text l

                           Nothing ->
                               none
               in
               column
                   [ fillSpace
                   , Font.family [ Font.monospace ]
                   ]
                   [ lang
                   , link
                       [ width fill
                       , Bg.color <| Style.mix 0.05 model.color.bg model.color.fg
                       ]
                       { url = ""
                       , label =
                           column
                               [ width fill
                               , scrollbarX
                               , Bg.color <| Style.mix 0.05 model.color.bg model.color.fg
                               ]
                               (body
                                   |> String.split "\n"
                                   |> (\xs ->
                                           List.take (List.length xs - 1) xs
                                               |> List.indexedMap
                                                   (\i str ->
                                                       row
                                                           [ width fill
                                                           , height <| px <| spcNum
                                                           ]
                                                           [ el
                                                               [ Bg.color <| Style.mix 0.2 model.color.bg model.color.fg
                                                               , paddingEach { edges | right = spcNum // 4 }
                                                               , height fill
                                                               ]
                                                             <|
                                                               el [ centerY, width fill ] <|
                                                                   (i + 1 |> String.fromInt |> String.padLeft 4 ' ' |> text)
                                                           , el
                                                               [ Bg.color <| Style.mix 0.05 model.color.bg model.color.fg
                                                               , paddingEach { edges | left = spcNum // 4 }
                                                               , fillSpace
                                                               ]
                                                             <|
                                                               el [ centerY, width fill ] <|
                                                                   text str
                                                           ]
                                                   )
                                      )
                               )
                       }
                   ]
       , thematicBreak =
           -- view
           hBar
       , table =
           -- List view -> view
           \list ->
               column
                   [ Bg.color <| Style.mix 0.05 model.color.bg model.color.fg
                   , spacing <| spcNum // 2
                   , padding <| spcNum // 2
                   ]
                   list
       , tableHeader =
           -- List view -> view
           \list -> row [ width fill, spacing <| spcNum // 2, Border.widthEach { edges | bottom = 2 } ] list
       , tableBody =
           -- List view -> view
           \list -> column [ width fill, spacing <| spcNum // 2 ] list
       , tableRow =
           -- List view -> view
           \list -> row [ width fill, spacing <| spcNum // 2 ] list
       , tableCell =
           -- Maybe Alignment -> List view -> view
           \maybeArg list ->
               paragraph
                   [ width fill
                   , spacing <| spcNum // 2
                   , case maybeArg of
                       Just arg ->
                           case arg of
                               Md.AlignLeft ->
                                   alignLeft

                               Md.AlignCenter ->
                                   centerX

                               Md.AlignRight ->
                                   alignRight

                       Nothing ->
                           batch []
                   ]
                   list
       , tableHeaderCell =
           -- Maybe Alignment -> List view -> view
           \maybeArg list ->
               paragraph
                   [ width fill
                   , Font.bold
                   , spacing <| spcNum // 2
                   , case maybeArg of
                       Just arg ->
                           case arg of
                               Md.AlignLeft ->
                                   alignLeft

                               Md.AlignCenter ->
                                   centerX

                               Md.AlignRight ->
                                   alignRight

                       Nothing ->
                           batch []
                   ]
                   list
       }
-}
