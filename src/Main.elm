module Main exposing (main)

import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, src)



-- Model


type Colour
    = White
    | Black


type PieceType
    = King
    | Queen
    | Rook
    | Bishop
    | Knight
    | Pawn


type alias Row =
    Int


type alias Column =
    Int


type alias Location =
    { row : Row
    , column : Column
    }


type alias Piece =
    { colour : Colour
    , type_ : PieceType
    }


type alias Square =
    { location : Location
    , shaded : Bool
    , piece : Maybe Piece
    }


type alias Board =
    List Square


type Status
    = PlayerToMove Colour
    | Win Colour
    | Draw


type alias Model =
    { board : Board
    , status : Status
    }


reset : Model
reset =
    Model initialBoard initialStatus


initialBoard : Board
initialBoard =
    rowOfPieces 1 White
        ++ rowOfPawns 2 White
        ++ emptyRows 3 6
        ++ rowOfPawns 7 Black
        ++ rowOfPieces 8 Black


rowOfPieces : Row -> Colour -> List Square
rowOfPieces row colour =
    [ pieceSquare colour Rook row 1
    , pieceSquare colour Knight row 2
    , pieceSquare colour Bishop row 3
    , pieceSquare colour Queen row 4
    , pieceSquare colour King row 5
    , pieceSquare colour Bishop row 6
    , pieceSquare colour Knight row 7
    , pieceSquare colour Rook row 8
    ]


pieceSquare : Colour -> PieceType -> Row -> Column -> Square
pieceSquare colour piece row column =
    Square (Location row column)
        (isSquareShaded row column)
        (Just <| Piece colour piece)


isSquareShaded : Row -> Column -> Bool
isSquareShaded row column =
    if modBy 2 row == 0 then
        modBy 2 column == 0

    else
        modBy 2 column == 1


rowOfPawns : Row -> Colour -> List Square
rowOfPawns row colour =
    List.range 1 8
        |> List.map (pieceSquare colour Pawn row)


emptyRows : Row -> Row -> List Square
emptyRows rowStart rowEnd =
    List.range rowStart rowEnd
        |> List.concatMap emptyRow


emptyRow : Row -> List Square
emptyRow row =
    List.range 1 8
        |> List.map (emptySquare row)


emptySquare : Row -> Column -> Square
emptySquare row column =
    Square (Location row column)
        (isSquareShaded row column)
        Nothing


initialStatus : Status
initialStatus =
    PlayerToMove White



-- Update


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    model



-- View


view : Model -> Html Msg
view model =
    div
        [ css
            [ boxSizing borderBox
            ]
        ]
        [ div
            [ css
                [ property "display" "grid"
                , property "grid-template-columns" "repeat(8, 80px)"
                , property "grid-template-rows" "repeat(8, 80px)"
                , margin auto
                ]
            ]
            (List.range 1 8
                |> List.reverse
                |> List.map (squaresInRow model.board)
                |> List.map viewRow
            )
        ]


squaresInRow : Board -> Row -> List Square
squaresInRow board row =
    board
        |> List.filter (\square -> square.location.row == row)


viewRow : List Square -> Html Msg
viewRow row =
    div [ css [ property "display" "contents" ] ]
        (List.map viewSquare row)


viewSquare : Square -> Html Msg
viewSquare square =
    let
        contents =
            case square.piece of
                Nothing ->
                    []

                Just piece ->
                    [ img
                        [ src <| imageSource piece
                        , css
                            [ width (pct 100)
                            , height (pct 100)
                            ]
                        ]
                        []
                    ]

        colour =
            case square.shaded of
                True ->
                    "#b58761"

                False ->
                    "#efdab4"
    in
    div
        [ css
            [ backgroundColor (hex colour)
            ]
        ]
        contents


imageSource : Piece -> String
imageSource { colour, type_ } =
    let
        colourString =
            case colour of
                Black ->
                    "black"

                White ->
                    "white"

        piece =
            case type_ of
                Rook ->
                    "rook"

                Knight ->
                    "knight"

                Bishop ->
                    "bishop"

                Queen ->
                    "queen"

                King ->
                    "king"

                Pawn ->
                    "pawn"
    in
    "images/" ++ piece ++ "_" ++ colourString ++ ".svg"



-- Main


main : Program () Model Msg
main =
    Browser.sandbox
        { init = reset
        , update = update
        , view = view >> toUnstyled
        }
