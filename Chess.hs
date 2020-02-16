module Chess where

data PieceType
  = Pawn
  | Rook
  | Knight
  | Bishop
  | Queen
  | King
  deriving (Show, Eq)

data Colour
  = White
  | Black
  deriving (Show, Eq)

data Piece = Piece Colour PieceType
  deriving (Show, Eq)

data Square = Square (Maybe Piece)
  deriving (Show, Eq)

type Board = [[Square]]

type Location = (Int, Int)

data GameState
  = Normal
  | Check
  | CheckMate
  | Draw String
  deriving (Show, Eq)

data Move = Move
  { piece :: Piece
  , from :: Location
  , to :: Location
  , state :: GameState
  }
  deriving (Show, Eq)

type MoveHistory = [Move]

data HorizontalDirection
  = MoveLeft
  | MoveRight
  deriving (Show, Eq)

initialBoard :: Board
initialBoard =
  let
    whiteKingRow = kingRow White
    whitePawnRow = pawnRow White
    middleRows = take 4 $ repeat emptyRow
    blackPawnRow = pawnRow Black
    blackKingRow = kingRow Black
  in
    [whiteKingRow, whitePawnRow]
      ++ middleRows
      ++ [blackPawnRow, blackKingRow]

kingRow :: Colour -> [Square]
kingRow colour =
  let
    types = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
    pieces = map (Square . Just . Piece colour) types
  in
    pieces

pawnRow :: Colour -> [Square]
pawnRow colour =
  take 8 . repeat . Square $ Just (Piece colour Pawn)

emptyRow :: [Square]
emptyRow =
  take 8 . repeat $ Square Nothing

squareAt :: Location -> Board -> Square
squareAt (row, col) board =
  (board !! row) !! col

emptyAt :: Location -> Board -> Bool
emptyAt location board =
  let
    square = squareAt location board
  in
    case square of
      Square Nothing -> True
      _ -> False

enemyAt :: Colour -> Location -> Board -> Bool
enemyAt colour location board =
  let
    square = squareAt location board
  in
    case square of
      Square (Just (Piece colourAt _)) -> colour /= colourAt
      _ -> False

lastMove :: MoveHistory -> Maybe Move
lastMove moves@(x:xs) = Just $ last moves
lastMove _ = Nothing

lastMoveEq :: Location -> Location -> MoveHistory -> Bool
lastMoveEq from to history =
  let
    previousMove = lastMove history
  in
    case previousMove of
      Just (Move {from = f, to = t}) -> from == f && to == t
      _ -> False

pawnMoves :: Colour -> Location -> Board -> MoveHistory -> [Location]
pawnMoves colour location@(row, col) board history =
  let
    advanceOneLocation =
      if canAdvanceOne then
        [(row + dy, col)]
      else
        []
    
    advanceTwoLocation =
      if canAdvanceOne then
        [(row + dy * 2, col)]
      else
        []
    
    captureLeftLocation =
      if canCaptureLeft then
        [(row + dy, col - 1)]
      else
        []
    
    captureRightLocation =
      if canCaptureRight then
        [(row + dy, col + 1)]
      else
        []
  
  in
    advanceOneLocation
    ++ advanceTwoLocation
    ++ captureLeftLocation
    ++ captureRightLocation

  where
    onStartingRank = case colour of
      White -> row == 1
      _ -> row == 6

    dy = case colour of
      White -> 1
      _ -> -1

    canAdvanceOne = emptyAt (row + dy, col) board

    canAdvanceTwo = onStartingRank && emptyAt (row + dy * 2, col) board

    onEnPassantRank = case colour of
      White -> row == 5
      _ -> row == 4

    enPassantSource colour col =
      case colour of
        White -> ((6, col), (4, col))
        _ -> ((1, col), (3, col))

    canMove direction =
      case direction of
        MoveLeft -> col > 0
        _ -> col < 7

    dxForDirection direction =
      case direction of
        MoveLeft -> 1
        _ -> 1

    canEnPassant direction =
      if canMove direction then
        let
          dx = dxForDirection direction
          (from, to) = enPassantSource colour $ col + dx
        in
          lastMoveEq from to history
      else
        False

    canEnPassantLeft = onEnPassantRank && canEnPassant MoveLeft

    canEnPassantRight = onEnPassantRank && canEnPassant MoveRight

    canCapture direction =
      if canMove direction then
        let
          rowTo = case colour of
            White -> row + 1
            _ -> row - 1

          colTo = col + dxForDirection direction
        in
          enemyAt colour (rowTo, colTo) board
      else
        False

    canCaptureLeft = canCapture MoveLeft || canEnPassantLeft

    canCaptureRight = canCapture MoveRight || canEnPassantRight
