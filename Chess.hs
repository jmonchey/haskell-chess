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

data HorizontalMove
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

lastMoveWas :: Location -> Location -> MoveHistory -> Bool
lastMoveWas from to history =
  let
    previousMove = lastMove history
  in
    case previousMove of
      Just (Move {from = f, to = t}) -> from == f && to == t
      _ -> False

pawnMoves :: Colour -> Location -> Bool -> MoveHistory -> Board -> [Location]
pawnMoves colour (row, col) onlyCaptures history board =
    advanceOneLocation
    ++ advanceTwoLocation
    ++ captureLeftLocation
    ++ captureRightLocation

  where
    onStartingRank =
      case colour of
        White -> row == 1
        _ -> row == 6

    dy =
      case colour of
        White -> 1
        _ -> -1

    canAdvanceOne = emptyAt (row + dy, col) board

    canAdvanceTwo = onStartingRank && emptyAt (row + dy * 2, col) board

    onEnPassantRank =
      case colour of
        White -> row == 5
        _ -> row == 4

    enPassantSource col =
      case colour of
        White -> ((6, col), (4, col))
        _ -> ((1, col), (3, col))

    canMove direction =
      case direction of
        MoveLeft -> col > 0
        _ -> col < 7

    dxForDirection direction =
      case direction of
        MoveLeft -> -1
        _ -> 1

    canEnPassant direction =
      if canMove direction then
        let
          dx = dxForDirection direction
          (from, to) = enPassantSource $ col + dx
        in
          lastMoveWas from to history
      else
        False

    canEnPassantLeft = onEnPassantRank && canEnPassant MoveLeft

    canEnPassantRight = onEnPassantRank && canEnPassant MoveRight

    canCapture direction =
      if canMove direction then
        let
          rowTo = row + dy
          colTo = col + dxForDirection direction
        in
          enemyAt colour (rowTo, colTo) board
      else
        False

    canCaptureLeft = canCapture MoveLeft || canEnPassantLeft

    canCaptureRight = canCapture MoveRight || canEnPassantRight

    advanceOneLocation =
      if canAdvanceOne && not onlyCaptures then
        [(row + dy, col)]
      else
        []
    
    advanceTwoLocation =
      if canAdvanceTwo && not onlyCaptures then
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

rookMoves :: Colour -> Location -> Board -> [Location]
rookMoves colour location board =
  let
    locationsLeft = lineLocations location (-1) 0
    movesLeft = possibleLineMoves colour locationsLeft board
    locationsRight = lineLocations location 1 0
    movesRight = possibleLineMoves colour locationsRight board
    locationsUp = lineLocations location 0 1
    movesUp = possibleLineMoves colour locationsUp board
    locationsDown = lineLocations location 0 (-1)
    movesDown = possibleLineMoves colour locationsDown board
  in
    movesLeft ++ movesRight ++ movesUp ++ movesDown

lineLocations :: Location -> Int -> Int -> [Location]
lineLocations (row, col) dx dy =
  let
    endX =
      case dx of
        -1 -> 0
        1 -> 7
        _ -> col

    endY =
      case dy of
        -1 -> 0
        1 -> 7
        _ -> row

    x1 = col + dx
    x2 = col + dx * 2
    y1 = row + dy
    y2 = row + dy * 2
  in
    zip [y1,y2..endY] [x1,x2..endX]

possibleLineMoves :: Colour -> [Location] -> Board -> [Location]
possibleLineMoves colour locations board =
  snd $ foldr canMoveTo (False, []) $ reverse locations

  where
    canMoveTo _ result@(True, _) = result
    canMoveTo location (_, xs) =
      if emptyAt location board then
        (False, location:xs)
      else
        if enemyAt colour location board then
          (True, location:xs)
        else
          (True, xs)

bishopMoves :: Colour -> Location -> Board -> [Location]
bishopMoves colour location board =
  let
    locationsUpLeft = lineLocations location (-1) 1
    movesUpLeft = possibleLineMoves colour locationsUpLeft board
    locationsUpRight = lineLocations location 1 1
    movesUpRight = possibleLineMoves colour locationsUpRight board
    locationsDownLeft = lineLocations location (-1) (-1)
    movesDownLeft = possibleLineMoves colour locationsDownLeft board
    locationsDownRight = lineLocations location 1 (-1)
    movesDownRight = possibleLineMoves colour locationsDownRight board
  in
    movesUpLeft ++ movesUpRight ++ movesDownLeft ++ movesDownRight

queenMoves :: Colour -> Location -> Board -> [Location]
queenMoves colour location board =
  rookMoves colour location board ++ bishopMoves colour location board

knightMoves :: Colour -> Location -> Board -> [Location]
knightMoves colour location board =
  filter (canMoveTo colour board) $ validLocations locations

  where
    displacements =
      [(-2, 1), (-1, 2), (2, 1), (1, 2), (-2, -1), (-1, -2), (2, -1), (1, -2)]

    locations =
      map (displace location)  displacements

type Displacement = (Int, Int)

displace :: Location -> Displacement -> Location
displace (row, col) (dx, dy) =
  (row + dy, col + dx)

validLocations :: [Location] -> [Location]
validLocations =
  filter isValidLocation

  where
    isValidLocation (row, col) =
      row >= 0 && row <=7 && col >= 0 && col <= 7

canMoveTo :: Colour -> Board -> Location -> Bool
canMoveTo colour board to =
  emptyAt to board || enemyAt colour to board

kingMoves :: Colour -> Location -> Bool -> MoveHistory -> Board -> [Location]
kingMoves colour location onlyCaptures history board =
  normalMoves ++ castleKingSideMove ++ castleQueenSideMove

  where
    displacements =
      [(-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)]

    locations =
      map (displace location) displacements

    normalMoves =
      filter (canMoveTo colour board) $ validLocations locations

    castleKingSide = canCastleKingSide colour history board

    castleQueenSide = canCastleQueenSide colour history board
    
    castleKingSideMove =
      if castleKingSide && not onlyCaptures then
        [displace location (-2, 0)]
      else
        []

    castleQueenSideMove =
      if castleQueenSide && not onlyCaptures then
        [displace location (2, 0)]
      else
        []

canCastle :: Colour -> HorizontalMove -> MoveHistory -> Board -> Bool
canCastle colour direction history board =
  not haveMoved && not inCheck && not hasThreats

  where
    kingLocation =
      case colour of
        White -> (0, 3)
        _ -> (7, 3)
    
    rookLocation =
      case colour of
        White -> (0, 7)
        _ -> (7, 7)

    haveMoved =
      pieceMoved kingLocation history || pieceMoved rookLocation history

    inCheck =
      isThreatened colour history board kingLocation

    dx =
      case direction of
        MoveLeft -> -1
        _ -> 1

    displacements =
      [(dx, 0), (dx * 2, 0)]

    targetLocations =
      map (displace kingLocation) displacements

    hasThreats =
      any (isThreatened colour history board) targetLocations

canCastleQueenSide :: Colour -> MoveHistory -> Board -> Bool
canCastleQueenSide colour history board =
  canCastle colour MoveRight history board

canCastleKingSide :: Colour -> MoveHistory -> Board -> Bool
canCastleKingSide colour history board =
  canCastle colour MoveLeft history board

pieceMoved :: Location -> MoveHistory -> Bool
pieceMoved location history =
  any isFromOrTo history

  where
    isFromOrTo (Move {from = from, to = to}) =
      location == from || location == to

enemyPieces :: Colour -> Board -> [(Piece, Location)]
enemyPieces colour board =
  undefined

possibleMoves :: Piece -> Location -> Bool -> MoveHistory -> Board -> [Location]
possibleMoves piece location onlyCaptures history board =
  case piece of
    Piece colour Pawn -> pawnMoves colour location onlyCaptures history board
    Piece colour Rook -> rookMoves colour location board
    Piece colour Bishop -> bishopMoves colour location board
    Piece colour Queen -> queenMoves colour location board
    Piece colour King -> kingMoves colour location onlyCaptures history board

isThreatened :: Colour -> MoveHistory -> Board -> Location -> Bool
isThreatened colour history board location =
  location `elem` threatLocations

  where
    enemies = enemyPieces colour board

    enemyMoves (piece, enemyLocation) =
      possibleMoves piece enemyLocation True history board

    threatLocations = concat $ map enemyMoves enemies

findKing :: Colour -> Board -> Location
findKing colour board =
  undefined

makeMove :: Location -> Location -> MoveHistory -> Board -> Board
makeMove from to history board =
  undefined