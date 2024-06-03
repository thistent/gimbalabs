module Markup exposing (..)

import Browser.Dom exposing (Viewport)
import Docs exposing (..)
import Html
import Html.Attributes as Attrs
import Markdown.Block as Md
import Markdown.Html as MdHtml
import Markdown.Parser as Md
import Markdown.Renderer as Md
import Pic
import Style exposing (..)
import Types exposing (..)
import Ui exposing (..)
import Ui.Background as Bg
import Ui.Border as Border
import Ui.Font as Font


type MdToken
    = Heading Float (List MdToken) (List MdToken)
    | Paragraph (List MdToken)
    | BlockQuote (List MdToken)
    | Text String
    | CodeSpan String
    | Strong (List MdToken)
    | Emphasis (List MdToken)
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
        |> textColumn [ fillSpace, spacing <| fontSize * 2 ]


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
        render : List MdToken -> List (Element Msg)
        render =
            List.map (renderToken model vp)
    in
    case tok of
        Heading level title subsections ->
            if level == 1.0 then
                textColumn
                    [ width fill
                    , spacing <| fontSize * 2
                    ]
                <|
                    column
                        [ width fill
                        , Bg.color <| Style.mix 0.9 model.color.fg model.color.bg
                        , Border.roundEach { corners | topLeft = fontSize // 2, topRight = fontSize // 2 }
                        ]
                        [ paragraph
                            [ paddingXY (fontSize * 2) fontSize
                            , width fill
                            , Font.size <| round <| fontSize * 2 * (1.0 - ((level - 1) * 0.1))
                            , spacing <| fontSize
                            ]
                            (render title)
                        , el
                            [ width fill
                            , height <| px <| fontSize // 2
                            , Border.color <| Style.mix 0.5 model.color.bg model.color.fg
                            , Border.widthEach { edges | top = lineSize * 2 }
                            , Border.roundEach { corners | topLeft = fontSize // 2, topRight = fontSize // 2 }
                            , Bg.color model.color.bg
                            ]
                            none
                        ]
                        :: render subsections

            else
                textColumn [ width fill ] <|
                    [ paragraph
                        [ Border.widthEach { edges | left = lineSize * 2 }
                        , Border.roundEach { corners | topLeft = fontSize // 2, topRight = fontSize // 2, bottomRight = fontSize // 2 }
                        , Border.color <| Style.mix 0.5 model.color.bg model.color.fg
                        , paddingXY (fontSize * 2) fontSize
                        , Bg.color <| Style.mix 0.9 model.color.fg model.color.bg
                        , Font.size <| round <| fontSize * 2 * (1.0 - ((level - 1) * 0.1))
                        , spacing <| fontSize
                        , width fill
                        ]
                        (render title)
                    , el
                        [ height <| px <| fontSize * 2
                        , Border.color <| Style.mix 0.5 model.color.bg model.color.fg
                        , Border.widthEach { edges | left = lineSize * 2 }
                        ]
                        none
                    , textColumn
                        [ Border.widthEach { edges | left = lineSize * 2 }
                        , Border.roundEach { corners | bottomLeft = fontSize // 2 }
                        , Border.color <| Style.mix 0.5 model.color.bg model.color.fg
                        , paddingEach { edges | left = fontSize, bottom = fontSize }
                        , spacing <| fontSize * 2
                        , width fill
                        ]
                      <|
                        render subsections
                    ]

        Paragraph toks ->
            paragraph
                [ spacing <| fontSize // 2
                , width fill
                ]
            <|
                List.map (renderToken model vp) toks

        BlockQuote toks ->
            paragraph
                [ spacing <| fontSize // 2
                , width fill
                , Border.color <| Style.mix 0.75 model.color.fg model.color.bg
                , Border.widthEach { edges | left = lineSize * 2 }
                , Border.roundEach { corners | topLeft = fontSize // 2, bottomLeft = fontSize // 2 }
                , paddingXY (fontSize * 2) 0
                ]
            <|
                List.map (renderToken model vp) toks

        Text str ->
            paragraph [ width fill ]
                [ wrappedRow
                    []
                    [ text str ]
                ]

        CodeSpan str ->
            paragraph
                [ width fill
                , Bg.color <| Style.mix 0.08 model.color.bg model.color.fg
                , paddingXY 5 2
                ]
                [ wrappedRow
                    [ Font.family [ Font.monospace ]
                    ]
                    [ text str ]
                ]

        Strong toks ->
            paragraph [ width fill ]
                [ wrappedRow [ Font.bold ] <|
                    List.map (renderToken model vp) toks
                ]

        Emphasis toks ->
            paragraph [ width fill ]
                [ wrappedRow [ Font.italic ] <|
                    List.map (renderToken model vp) toks
                ]

        Strike toks ->
            paragraph [ width fill ]
                [ wrappedRow [ Font.strike ] <|
                    List.map (renderToken model vp) toks
                ]

        LineBreak ->
            paragraph
                [ spacing <| fontSize // 2
                , width fill
                ]
                [ text "<br>" ]

        Link { destination } toks ->
            -- { title, destination }
            iconButton model
                (GetDoc destination)
                Nothing
            <|
                paragraph [] <|
                    render toks

        Image { alt, src, title } ->
            column
                [ width <| px <| round <| vp.viewport.width * 0.33
                , paddingEach { edges | right = fontSize * 2 }
                ]
                [ image
                    [ width fill
                    ]
                    { src = src
                    , description = alt
                    }
                , case title of
                    Just t ->
                        paragraph
                            [ centerX
                            , Bg.color <| Style.mix 0.05 model.color.bg model.color.fg
                            , paddingXY fontSize <| fontSize // 2
                            , Font.size fontSize
                            , Font.bold
                            , Font.italic
                            , width fill
                            , Border.roundEach { corners | bottomLeft = fontSize // 2, bottomRight = fontSize // 4 }
                            , spacing <| fontSize // 2
                            ]
                            [ text t ]

                    Nothing ->
                        none
                ]

        ListBlock _ toks ->
            -- ListBlock maybeIndex toks ->
            textColumn
                [ spacing <| fontSize
                , paddingEach { edges | top = fontSize }
                , width fill
                ]
                (render toks)

        ListItem { task, content, children } ->
            mdItem model.color task (render content) (render children)

        CodeBlock codeRec ->
            el [ width fill ] <|
                renderCodeBlock model vp codeRec

        ThematicBreak ->
            hBar

        Table toks ->
            el [ fillSpace ] <|
                column
                    [ Bg.color <| Style.mix 0.15 model.color.bg model.color.fg
                    , Border.rounded <| fontSize // 2
                    , width <| px <| round <| vp.viewport.width * 0.85

                    --, centerX
                    , alignRight
                    ]
                    (render toks)

        TableHeader toks ->
            row
                [ width fill
                , height fill
                , spacing fontSize
                , Border.roundEach { corners | topLeft = fontSize // 2, topRight = fontSize // 2 }
                , Bg.color <| Style.mix 0.15 model.color.bg model.color.fg
                ]
                (render toks)

        TableBody toks ->
            column
                [ width fill
                , Border.roundEach { corners | bottomRight = fontSize }
                , Border.rounded <| fontSize // 2
                , Bg.color <| Style.mix 0.05 model.color.bg model.color.fg
                ]
                (render toks)

        TableRow toks ->
            row
                [ width fill
                , height fill
                , Border.color <| Style.mix 0.2 model.color.bg model.color.fg
                , Border.roundEach { corners | topLeft = fontSize // 2, topRight = fontSize // 2 }
                , Border.widthEach { edges | top = 3 }
                ]
                (render toks
                    |> List.intersperse
                        (el
                            [ height fill
                            , Border.color <| Style.mix 0.2 model.color.bg model.color.fg
                            , Border.widthEach { edges | left = 3, top = 3, bottom = 3 }
                            ]
                            none
                        )
                )

        TableCell maybeAlign toks ->
            paragraph
                [ width fill
                , centerY
                , spacing fontSize
                , paddingXY (fontSize * 2) fontSize
                , Font.justify
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
                , spacing fontSize
                , paddingXY (fontSize * 2) fontSize
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

        Strike _ ->
            True

        _ ->
            False


mdItem : Pal -> Md.Task -> List (Element Msg) -> List (Element Msg) -> Element Msg
mdItem pal task content children =
    row
        [ width fill
        , spacing <| fontSize // 2
        ]
        [ case task of
            Md.NoTask ->
                el
                    [ alignTop
                    , width <| px <| fontSize * 5 // 4
                    , moveDown <| fontSize / 16
                    ]
                <|
                    Pic.bullet pal.fg

            Md.IncompleteTask ->
                el
                    [ alignTop
                    , width <| px <| fontSize * 5 // 4
                    , moveDown <| fontSize / 16
                    ]
                <|
                    Pic.unchecked <|
                        Style.mix 0.3 pal.bg pal.fg

            Md.CompletedTask ->
                el
                    [ alignTop
                    , width <| px <| fontSize * 5 // 4
                    , moveDown <| fontSize / 16
                    ]
                <|
                    Pic.checked pal.link

        -- ■▨▣□⬚▨▩●▪
        , paragraph
            [ fillSpace
            , alignTop
            ]
          <|
            content
                ++ children
        ]


renderCodeBlock : Model -> Viewport -> { body : String, language : Maybe String } -> Element Msg
renderCodeBlock model vp { body, language } =
    textColumn
        [ Font.family [ Font.monospace ]
        , Bg.color <| Style.mix 0.2 model.color.bg model.color.fg
        , width <| px <| round <| vp.viewport.width * 0.85
        , Border.rounded <| fontSize // 2

        --, centerX
        , alignRight
        ]
        [ el
            [ Font.size <| fontSize
            , padding <| fontSize // 2
            , Bg.color <| Style.mix 0.2 model.color.bg model.color.fg
            , Border.rounded <| fontSize // 2
            , noSelect
            ]
          <|
            case language of
                Just l ->
                    text l

                Nothing ->
                    none
        , el
            [ width fill
            , Bg.color <|
                Style.mix 0.05 model.color.bg model.color.fg
            , clip
            , scrollbars
            ]
          <|
            column
                [ Bg.color <|
                    Style.mix 0.05 model.color.bg model.color.fg
                , width fill
                ]
                (body
                    |> String.split "\n"
                    |> (\xs ->
                            xs
                                |> List.reverse
                                |> splitWhile (\x -> x == "")
                                |> Tuple.second
                                |> List.reverse
                                |> List.indexedMap
                                    (\i str ->
                                        let
                                            bg : { num : Color, text : Color }
                                            bg =
                                                if modBy 2 i == 0 then
                                                    { num =
                                                        Style.mix 0.1 model.color.bg model.color.fg
                                                    , text =
                                                        Style.mix 0.05 model.color.bg model.color.fg
                                                    }

                                                else
                                                    { num =
                                                        Style.mix 0.15 model.color.bg model.color.fg
                                                    , text =
                                                        Style.mix 0.1 model.color.bg model.color.fg
                                                    }
                                        in
                                        row
                                            [ width fill ]
                                            [ el
                                                [ Bg.color bg.num
                                                , padding <| fontSize // 2
                                                , height fill
                                                ]
                                                (i
                                                    + 1
                                                    |> String.fromInt
                                                    |> String.padLeft
                                                        (List.length xs
                                                            |> toFloat
                                                            |> logBase 10
                                                            |> (\x -> floor x + 1)
                                                        )
                                                        '0'
                                                    |> (\s -> el [ centerY, noSelect ] (text s))
                                                )
                                            , paragraph
                                                [ Bg.color bg.text
                                                , paddingXY (fontSize // 2) 0
                                                , width fill
                                                , height fill
                                                ]
                                                [ if str /= "" then
                                                    spaceText str

                                                  else
                                                    spaceText "\n"
                                                ]
                                            ]
                                    )
                       )
                )
        ]


spaceText : String -> Element msg
spaceText str =
    html <|
        Html.pre
            [ Attrs.style "padding" "0px"
            , Attrs.style "margins" "0px"
            ]
            [ Html.text str ]
