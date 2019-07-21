module Main exposing (main)

import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, src)
import Html.Styled.Events exposing (onClick)



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
    , activeSquare : Maybe Location
    }


reset : Model
reset =
    Model initialBoard initialStatus Nothing


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
    = SquareClicked Location


type alias Move =
    { player : Colour
    , from : Location
    , to : Location
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SquareClicked location ->
            squareClicked model location


squareClicked : Model -> Location -> Model
squareClicked model location =
    let
        targetSquare =
            squareAt model.board location

        player =
            case model.status of
                PlayerToMove colour ->
                    colour

                _ ->
                    White

        activeLocation =
            Maybe.withDefault (Location 1 1) model.activeSquare

        intendedMove =
            Move player activeLocation location
    in
    if model.activeSquare == Nothing then
        if isEmptySquare targetSquare then
            model

        else if hasPieceAt targetSquare player then
            validMovesAt model.board location
                |> modelFromValidMoves model location

        else
            model

    else if isLocationEqual location activeLocation then
        { model | activeSquare = Nothing }

    else if isOneOfValidMoves model.board activeLocation intendedMove then
        makeMove model.board intendedMove
            |> modelFromUpdatedBoard model

    else
        { model | activeSquare = Nothing }


squareAt : Board -> Location -> Square
squareAt board location =
    board
        |> List.filter (isSquareAtLocation location)
        |> List.head
        |> Maybe.withDefault (emptySquare 1 1)


hasPieceAt : Square -> Colour -> Bool
hasPieceAt square colour =
    case square.piece of
        Nothing ->
            False

        Just piece ->
            piece.colour == colour


validMovesAt : Board -> Location -> List Move
validMovesAt board location =
    let
        square =
            squareAt board location
    in
    case square.piece of
        Nothing ->
            []

        Just piece ->
            validPieceMoves board location piece


isOneOfValidMoves : Board -> Location -> Move -> Bool
isOneOfValidMoves board location move =
    validMovesAt board location
        |> List.filter (isMoveEqual move)
        |> List.isEmpty
        |> not


isMoveEqual : Move -> Move -> Bool
isMoveEqual first second =
    isLocationEqual first.from second.from
        && isLocationEqual first.to second.to


modelFromValidMoves : Model -> Location -> List Move -> Model
modelFromValidMoves model location moves =
    { model | activeSquare = Just location }


modelFromUpdatedBoard : Model -> Board -> Model
modelFromUpdatedBoard model board =
    let
        currentPlayer =
            case model.status of
                PlayerToMove colour ->
                    colour

                _ ->
                    White
    in
    { model
        | board = board
        , activeSquare = Nothing
        , status = PlayerToMove (oppositeColour currentPlayer)
    }


isSquareAtLocation : Location -> Square -> Bool
isSquareAtLocation location square =
    (location.row == square.location.row)
        && (location.column == square.location.column)


validPieceMoves : Board -> Location -> Piece -> List Move
validPieceMoves board location piece =
    possiblePieceMoves board location piece
        |> List.filter (isValidMove board)


possiblePieceMoves : Board -> Location -> Piece -> List Move
possiblePieceMoves board location piece =
    case piece.type_ of
        Rook ->
            possibleRookMoves board location piece.colour

        Knight ->
            possibleKnightMoves board location piece.colour

        Bishop ->
            possibleBishopMoves board location piece.colour

        Queen ->
            possibleQueenMoves board location piece.colour

        King ->
            possibleKingMoves board location piece.colour

        Pawn ->
            possiblePawnMoves board location piece.colour


squaresLeft : Board -> Location -> List Square
squaresLeft board location =
    let
        distance =
            location.column - 1
    in
    if location.column == 1 then
        []

    else
        List.range 1 distance
            |> List.map (\d -> squaresFromChange board location ( 0, -d ))
            |> List.concat


squaresRight : Board -> Location -> List Square
squaresRight board location =
    let
        distance =
            8 - location.column
    in
    if location.column == 8 then
        []

    else
        List.range 1 distance
            |> List.map (\d -> squaresFromChange board location ( 0, d ))
            |> List.concat


squaresUp : Board -> Location -> List Square
squaresUp board location =
    let
        distance =
            8 - location.row
    in
    if location.row == 8 then
        []

    else
        List.range 1 distance
            |> List.map (\d -> squaresFromChange board location ( d, 0 ))
            |> List.concat


squaresDown : Board -> Location -> List Square
squaresDown board location =
    let
        distance =
            location.row - 1
    in
    if location.row == 1 then
        []

    else
        List.range 1 distance
            |> List.map (\d -> squaresFromChange board location ( -d, 0 ))
            |> List.concat


squaresUpLeft : Board -> Location -> List Square
squaresUpLeft board location =
    let
        distance =
            location.column - 1
    in
    if location.column == 1 then
        []

    else
        List.range 1 distance
            |> List.map (\d -> squaresFromChange board location ( d, -d ))
            |> List.concat


squaresUpRight : Board -> Location -> List Square
squaresUpRight board location =
    let
        distance =
            8 - location.column
    in
    if location.column == 8 then
        []

    else
        List.range 1 distance
            |> List.map (\d -> squaresFromChange board location ( d, d ))
            |> List.concat


squaresDownLeft : Board -> Location -> List Square
squaresDownLeft board location =
    let
        distance =
            location.column - 1
    in
    if location.column == 1 then
        []

    else
        List.range 1 distance
            |> List.map (\d -> squaresFromChange board location ( -d, -d ))
            |> List.concat


squaresDownRight : Board -> Location -> List Square
squaresDownRight board location =
    let
        distance =
            8 - location.column
    in
    if location.column == 8 then
        []

    else
        List.range 1 distance
            |> List.map (\d -> squaresFromChange board location ( -d, d ))
            |> List.concat


type alias SquarePathSearch =
    { colour : Colour
    , complete : Bool
    , validSquares : List Square
    }


initialSearch : Colour -> SquarePathSearch
initialSearch colour =
    SquarePathSearch colour False []


isEmptyOrCapturableSquareInPath : Square -> SquarePathSearch -> SquarePathSearch
isEmptyOrCapturableSquareInPath square search =
    case ( search.complete, square.piece ) of
        ( True, _ ) ->
            search

        ( False, Nothing ) ->
            { search | validSquares = search.validSquares ++ [ square ] }

        ( False, Just piece ) ->
            if piece.colour == search.colour then
                { search | complete = True }

            else
                { search
                    | validSquares = search.validSquares ++ [ square ]
                    , complete = True
                }


searchResultSquares : SquarePathSearch -> List Square
searchResultSquares search =
    search.validSquares


possibleMovesInPath : Location -> Colour -> List Square -> List Move
possibleMovesInPath location colour squares =
    squares
        |> List.foldl isEmptyOrCapturableSquareInPath (initialSearch colour)
        |> searchResultSquares
        |> List.map (\square -> Move colour location square.location)


possibleRookMoves : Board -> Location -> Colour -> List Move
possibleRookMoves board location colour =
    let
        movesLeft =
            squaresLeft board location
                |> possibleMovesInPath location colour

        movesRight =
            squaresRight board location
                |> possibleMovesInPath location colour

        movesUp =
            squaresUp board location
                |> possibleMovesInPath location colour

        movesDown =
            squaresDown board location
                |> possibleMovesInPath location colour
    in
    List.concat [ movesLeft, movesRight, movesUp, movesDown ]


lShapedSquares : Board -> Location -> List Square
lShapedSquares board location =
    let
        moves =
            [ ( 1, -2 )
            , ( 2, -1 )
            , ( 1, 2 )
            , ( 2, 1 )
            , ( -1, -2 )
            , ( -2, -1 )
            , ( -1, 2 )
            , ( -2, 1 )
            ]
    in
    moves
        |> List.map (squaresFromChange board location)
        |> List.concat


squaresFromChange : Board -> Location -> ( Int, Int ) -> List Square
squaresFromChange board location change =
    let
        row =
            location.row + Tuple.first change

        column =
            location.column + Tuple.second change
    in
    if row < 1 || row > 8 || column < 1 || column > 8 then
        []

    else
        [ squareAt board (Location row column) ]


isEmptyOrCapturableSquare : Colour -> Square -> Bool
isEmptyOrCapturableSquare colour square =
    case square.piece of
        Nothing ->
            True

        Just piece ->
            piece.colour /= colour


possibleKnightMoves : Board -> Location -> Colour -> List Move
possibleKnightMoves board location colour =
    lShapedSquares board location
        |> List.filter (isEmptyOrCapturableSquare colour)
        |> List.map (\square -> Move colour location square.location)


possibleBishopMoves : Board -> Location -> Colour -> List Move
possibleBishopMoves board location colour =
    let
        movesUpLeft =
            squaresUpLeft board location
                |> possibleMovesInPath location colour

        movesUpRight =
            squaresUpRight board location
                |> possibleMovesInPath location colour

        movesDownLeft =
            squaresDownLeft board location
                |> possibleMovesInPath location colour

        movesDownRight =
            squaresDownRight board location
                |> possibleMovesInPath location colour
    in
    List.concat [ movesUpLeft, movesUpRight, movesDownLeft, movesDownRight ]


possibleQueenMoves : Board -> Location -> Colour -> List Move
possibleQueenMoves board location colour =
    List.concat
        [ possibleRookMoves board location colour
        , possibleBishopMoves board location colour
        ]


possibleKingMoves : Board -> Location -> Colour -> List Move
possibleKingMoves board location colour =
    let
        moves =
            [ ( -1, 0 )
            , ( -1, 1 )
            , ( 0, 1 )
            , ( 1, 1 )
            , ( 1, 0 )
            , ( 1, -1 )
            , ( 0, -1 )
            , ( -1, -1 )
            ]
    in
    moves
        |> List.map (squaresFromChange board location)
        |> List.concat
        |> List.filter (isEmptyOrCapturableSquare colour)
        |> List.map (\square -> Move colour location square.location)


possiblePawnMoves : Board -> Location -> Colour -> List Move
possiblePawnMoves board location colour =
    let
        rowChange =
            case colour of
                White ->
                    1

                Black ->
                    -1

        singleAdvance =
            squaresFromChange board location ( rowChange, 0 )

        doubleAdvance =
            if isPawnOnStartRank location colour then
                squaresFromChange board location ( 2 * rowChange, 0 )

            else
                []

        advanceSquares =
            List.filter isEmptySquare (singleAdvance ++ doubleAdvance)

        captureSquares =
            (squaresFromChange board location ( rowChange, -1 )
                ++ squaresFromChange board location ( rowChange, 1 )
            )
                |> List.filter (isCapturableSquare colour)
    in
    advanceSquares
        ++ captureSquares
        |> List.map (\square -> Move colour location square.location)


isPawnOnStartRank : Location -> Colour -> Bool
isPawnOnStartRank location colour =
    case colour of
        White ->
            location.row == 2

        Black ->
            location.row == 7


isEmptySquare : Square -> Bool
isEmptySquare square =
    square.piece == Nothing


isCapturableSquare : Colour -> Square -> Bool
isCapturableSquare colour square =
    case square.piece of
        Nothing ->
            False

        Just piece ->
            piece.colour /= colour


isValidMove : Board -> Move -> Bool
isValidMove board move =
    let
        kingInCheck =
            makeMove board move
                |> isKingInCheck move.player
    in
    not kingInCheck


makeMove : Board -> Move -> Board
makeMove board move =
    let
        piece =
            squareAt board move.from
                |> .piece
                |> Maybe.withDefault (Piece White Pawn)
    in
    List.map (updateSquareWithMove piece move) board


updateSquareWithMove : Piece -> Move -> Square -> Square
updateSquareWithMove piece move square =
    if isSquareAtLocation move.from square then
        { square | piece = Nothing }

    else if isSquareAtLocation move.to square then
        { square | piece = Just piece }

    else
        square


findKing : Board -> Colour -> Square
findKing board colour =
    board
        |> List.filter (isKingSquare colour)
        |> List.head
        |> Maybe.withDefault (emptySquare 1 1)


isKingSquare : Colour -> Square -> Bool
isKingSquare colour square =
    case square.piece of
        Nothing ->
            False

        Just piece ->
            piece.type_ == King && piece.colour == colour


oppositeColour : Colour -> Colour
oppositeColour colour =
    case colour of
        White ->
            Black

        Black ->
            White


possibleMovesAt : Board -> Colour -> Square -> List Move
possibleMovesAt board colour square =
    case square.piece of
        Nothing ->
            []

        Just piece ->
            if piece.colour == colour then
                possiblePieceMoves board square.location piece

            else
                []


isLocationEqual : Location -> Location -> Bool
isLocationEqual first second =
    first.row == second.row && first.column == second.column


isKingInCheck : Colour -> Board -> Bool
isKingInCheck colour board =
    let
        kingLocation =
            findKing board colour
                |> .location

        enemyColour =
            oppositeColour colour
    in
    board
        |> List.map (possibleMovesAt board enemyColour)
        |> List.concat
        |> List.filter (\move -> isLocationEqual kingLocation move.to)
        |> List.isEmpty
        |> not



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
                |> List.map (viewRow model)
            )
        ]


squaresInRow : Board -> Row -> List Square
squaresInRow board row =
    board
        |> List.filter (\square -> square.location.row == row)


viewRow : Model -> List Square -> Html Msg
viewRow model row =
    div [ css [ property "display" "contents" ] ]
        (List.map (viewSquare model) row)


viewSquare : Model -> Square -> Html Msg
viewSquare model square =
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

        squareColour =
            case square.shaded of
                True ->
                    "#b58761"

                False ->
                    "#efdab4"

        backgroundColour =
            if isActiveSquare model square then
                "#33e05a"

            else
                squareColour
    in
    div
        [ onClick (SquareClicked square.location)
        , css
            [ backgroundColor (hex backgroundColour)
            ]
        ]
        contents


isActiveSquare : Model -> Square -> Bool
isActiveSquare model square =
    case model.activeSquare of
        Just location ->
            isLocationEqual location square.location

        Nothing ->
            False


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
