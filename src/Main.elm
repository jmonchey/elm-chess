module Main exposing (main)

import Browser
import Html exposing (..)



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


type alias Piece =
    { colour : Colour
    , type_ : PieceType
    }


type alias Square =
    { row : Row
    , column : Column
    , piece : Maybe Piece
    }


type alias Board =
    List Square


type GameState
    = PlayerToMove Colour
    | Win Colour
    | Draw


type alias Model =
    { board : Board
    , state : GameState
    }


reset : Model
reset =
    Model initialBoard initialState


initialBoard : Board
initialBoard =
    rowOfPieces 1 White
        ++ rowOfPawns 2 White
        ++ emptyRows 3 6
        ++ rowOfPawns 7 Black
        ++ rowOfPieces 8 Black


rowOfPieces : Row -> Colour -> List Square
rowOfPieces row colour =
    [ Square row 1 (Just <| Piece colour Rook)
    , Square row 2 (Just <| Piece colour Knight)
    , Square row 3 (Just <| Piece colour Bishop)
    , Square row 4 (Just <| Piece colour Queen)
    , Square row 5 (Just <| Piece colour King)
    , Square row 6 (Just <| Piece colour Bishop)
    , Square row 7 (Just <| Piece colour Knight)
    , Square row 8 (Just <| Piece colour Rook)
    ]


rowOfPawns : Row -> Colour -> List Square
rowOfPawns row colour =
    List.range 1 8
        |> List.map (pawnSquare colour row)


pawnSquare : Colour -> Row -> Column -> Square
pawnSquare colour row column =
    Square row column (Just <| Piece colour Pawn)


emptyRows : Row -> Row -> List Square
emptyRows rowStart rowEnd =
    List.range rowStart rowEnd
        |> List.concatMap emptyRow


emptyRow : Row -> List Square
emptyRow row =
    List.range 1 8
        |> List.map (\column -> Square row column Nothing)


initialState : GameState
initialState =
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
    div []
        []



-- Main


main : Program () Model Msg
main =
    Browser.sandbox
        { init = reset
        , update = update
        , view = view
        }
