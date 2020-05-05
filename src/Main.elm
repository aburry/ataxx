module Main exposing (..)

import Browser
import Browser.Events exposing (..)
import Html exposing (div, table, td, tr)
import Html.Attributes
import Html.Events exposing (..)
import Matrix exposing (..)
import String exposing (..)
import Svg exposing (circle, defs, ellipse, linearGradient, radialGradient, stop, svg, text_)
import Svg.Attributes exposing (cx, cy, fill, fillOpacity, fx, fy, id, offset, r, rx, ry, stopColor, stopOpacity, x1, x2, xlinkHref, y1, y2)
import Tuple exposing (..)


type Square
    = Error
    | Empty
    | Red
    | Blue


type alias Model =
    { board : Matrix Square
    , fromClick : Maybe ( Int, Int )
    , turn : Square
    , time : Float
    }


type Msg
    = Click Int Int
    | Tick Float


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscribe
        }


subscribe _ =
    Sub.batch
        [ onAnimationFrameDelta Tick ]


init =
    let
        startPosition =
            emptyBoard 7 7
                |> set 0 0 Red
                |> set 6 6 Red
                |> set 0 6 Blue
                |> set 6 0 Blue
    in
    ( { board = startPosition
      , fromClick = Nothing
      , turn = Red
      , time = 0
      }
    , Cmd.none
    )


update msg model =
    case msg of
        Click r c ->
            onClick r c model

        Tick t ->
            ( { model | time = model.time + t }, Cmd.none )


onClick row col model =
    let
        nextModel =
            if model.fromClick == Nothing then
                actionFrom row col model

            else
                actionTo row col model
    in
    ( nextModel, Cmd.none )


emptyBoard row col =
    initialize row col (\_ _ -> Empty)


get row col board =
    Maybe.withDefault Error (Matrix.get row col board)


distance : ( Int, Int ) -> ( Int, Int ) -> Int
distance a b =
    max (abs (first b - first a)) (abs (second b - second a))


actionTo row col model =
    if get row col model.board == model.turn then
        { model | fromClick = Just ( row, col ) }

    else if get row col model.board /= Empty then
        model

    else if 2 < distance (Maybe.withDefault ( 0, 0 ) model.fromClick) ( row, col ) then
        model

    else
        move row col model


move toRow toCol model =
    let
        ( fromRow, fromCol ) =
            Maybe.withDefault ( 0, 0 ) model.fromClick

        d =
            distance ( fromRow, fromCol ) ( toRow, toCol )

        fromSq =
            if d == 2 then
                Empty

            else
                model.turn

        newBoard =
            model.board
                |> set fromRow fromCol fromSq
                |> set toRow toCol model.turn
                |> captureNeighbours toRow toCol model.turn
    in
    { model | board = newBoard, fromClick = Nothing, turn = next model.turn }


next square =
    case square of
        Red ->
            Blue

        Blue ->
            Red

        _ ->
            square


captureNeighbours row col turn board =
    let
        enemies =
            listSquares (captureIterator row col) (next turn) board

        captureOne position aBoard =
            set (first position) (second position) turn aBoard
    in
    List.foldl captureOne board enemies


actionFrom row col model =
    if List.member ( row, col ) (listFromMoves model.turn model.board) then
        { model | fromClick = Just ( row, col ) }

    else
        model


iterator topRow bottomRow leftCol rightCol =
    let
        makeRow n =
            List.map (Tuple.pair n) (List.range leftCol rightCol)
    in
    List.concatMap makeRow (List.range topRow bottomRow)


boardIterator =
    iterator 0 6 0 6


captureIterator row col =
    iterator (row - 1) (row + 1) (col - 1) (col + 1)


moveIterator row col =
    iterator (row - 2) (row + 2) (col - 2) (col + 2)


listToMoves row col board =
    let
        isEmptySquare position =
            Empty == get (first position) (second position) board
    in
    List.filter isEmptySquare (moveIterator row col)


listFromMoves turn board =
    let
        me =
            listSquares boardIterator turn board
    in
    List.filter (\e -> not (List.isEmpty (listToMoves (first e) (second e) board))) me


listSquares iter sq board =
    List.filter (\e -> sq == get (first e) (second e) board) iter


score turn board =
    List.length (listSquares boardIterator turn board)



-- VIEW


view model =
    div []
        [ title
        , table
            [ Html.Attributes.style "margin" "auto"
            , Html.Attributes.style "border-spacing" "30px"
            ]
            [ tr []
                [ viewScore Red model
                , td [] [ viewBoard model ]
                , viewScore Blue model
                ]
            ]
        ]


viewScore turn model =
    td
        [ Html.Attributes.style "font-family" "monospace"
        , Html.Attributes.style "font-size" "32pt"
        , Html.Attributes.style "padding" "40px"
        , Html.Attributes.style "background" (scoreBackgroundColor turn model)
        , Html.Attributes.style "border-radius" "8px"
        ]
        [ Html.text (paddedFromInt (score turn model.board)) ]


viewBoard model =
    let
        n =
            Basics.toFloat (modBy 128 (floor (model.time / 12))) / 128.0

        percent =
            floor (((0.1 * sin (2.0 * pi * n)) + 0.8) * 100.0)

        squareSize row col =
            if model.fromClick == Just ( row, col ) then
                fromInt percent ++ "%"

            else
                "90%"

        viewSq row col sq =
            td
                [ Html.Attributes.style "border" "1px solid lightgrey"
                , Html.Attributes.style "width" "80px"
                , Html.Attributes.style "height" "80px"
                , Html.Attributes.style "background" "darkcyan"
                , Html.Attributes.style "border-radius" "8px"
                , Html.Events.onClick (Click row col)
                , highlight row col model
                ]
                [ squareIcon sq (squareSize row col) ]

        viewRow row =
            tr [] (List.map (\col -> viewSq row col (get row col model.board)) (List.range 0 6))
    in
    table
        [ Html.Attributes.style "border-collapse" "collapse"
        , Html.Attributes.style "margin" "auto"
        , Html.Attributes.style "box-shadow" "5px 5px 3px grey"
        , Html.Attributes.style "border" "5px solid black"
        , Html.Attributes.style "background" "lightgrey"
        ]
        (List.map (\e -> viewRow e) (List.range 0 6))


squareIcon square size =
    case square of
        Error ->
            emptySquare

        Empty ->
            emptySquare

        Red ->
            pieceSquare "red" size

        Blue ->
            pieceSquare "blue" size


highlight row col model =
    let
        validList =
            case model.fromClick of
                Just ( fromRow, fromCol ) ->
                    listToMoves fromRow fromCol model.board

                _ ->
                    listFromMoves model.turn model.board
    in
    if List.member ( row, col ) validList then
        Html.Attributes.style "background" "yellow"

    else
        Html.Attributes.style "" ""


paddedFromInt i =
    if i < 10 then
        "0" ++ String.fromInt i

    else
        String.fromInt i


scoreBackgroundColor turn model =
    let
        n =
            Basics.toFloat (modBy 128 (floor (model.time / 12))) / 128.0

        alpha =
            String.fromFloat ((0.2 * sin (2.0 * pi * n)) + 0.4)
    in
    case turn of
        Red ->
            if model.turn == Red then
                "rgba(255, 0, 0, " ++ alpha ++ ")"

            else
                "rgba(255, 0, 0, 0.5)"

        Blue ->
            if model.turn == Blue then
                "rgba(0, 0, 255, " ++ alpha ++ ")"

            else
                "rgba(0, 0, 255, 0.5)"

        _ ->
            "black"


title =
    svg
        [ Svg.Attributes.width "300"
        , Svg.Attributes.height "100"
        , Html.Attributes.style "display" "block"
        , Html.Attributes.style "margin" "auto"
        ]
        [ text_
            [ Svg.Attributes.stroke "yellow"
            , Svg.Attributes.strokeWidth "5"
            , Svg.Attributes.fill "black"
            , Svg.Attributes.x "50%"
            , Svg.Attributes.y "90%"
            , Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.fontFamily "sans-serif"
            , Svg.Attributes.fontWeight "bolder"
            , Svg.Attributes.fontSize "72pt"
            , Svg.Attributes.dx "0 -14.5 -11.5 0 -6.5"
            ]
            [ Svg.text "ATAXX" ]
        ]


squareStyle size =
    [ Svg.Attributes.viewBox "0 0 84 84"
    , Svg.Attributes.width size
    , Svg.Attributes.height size
    , Html.Attributes.style "display" "block"
    , Html.Attributes.style "margin" "auto"
    ]


emptySquare =
    svg (squareStyle "90%") []


pieceSquare color size =
    svg
        (squareStyle size)
        [ defs []
            [ linearGradient [ id "b" ]
                [ stop [ offset "0", stopColor "white" ] []
                , stop [ offset "1", stopColor "white", stopOpacity "0" ] []
                ]
            , linearGradient [ id "a" ]
                [ stop [ offset "0", stopColor "white", stopOpacity "0" ] []
                , stop [ offset "1", stopOpacity ".34" ] []
                ]
            , linearGradient
                [ id "e", xlinkHref "#b", x1 "0", y1 "-0.3", x2 "0", y2 "1.0" ]
                []
            , radialGradient
                [ id "c", xlinkHref "#a", cx ".5", cy ".5", r ".52", fx ".57", fy ".2" ]
                []
            , radialGradient
                [ id "d", xlinkHref "#b", cx ".5", cy ".5", r ".52" ]
                []
            ]
        , circle [ cx "44", cy "44", r "40", fillOpacity ".4", fill "black" ] []
        , circle [ cx "42", cy "42", r "40", fillOpacity ".4", fill "black" ] []
        , circle [ cx "40", cy "40", r "40", fill color ] []
        , circle [ cx "40", cy "40", r "40", fill "url(#c)" ] []
        , ellipse [ cx "40", cy "22", rx "31", ry "18", fillOpacity ".9", fill "url(#e)" ] []
        , ellipse [ cx "42", cy "66", rx "22", ry "13", fillOpacity ".55", fill "url(#d)" ] []
        ]
