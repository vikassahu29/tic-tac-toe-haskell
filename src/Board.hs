module Board
( TurnType(..)
, Cell(..)    -- do we need this (..)?
, Board(..) -- same as above, since these dont have value constructors but they make data types more explicit
, getCellAtPos
, checkValidMove
, hasPlayerAtThisPosWon
, makeTurn
, initBoard
) where

    data TurnType = Zero | Cross deriving (Show, Eq)

    data Cell = TurnType TurnType | Empty deriving (Show, Eq)

    type Board = [[Cell]]

    initBoard = [[Empty, Empty,Empty], [Empty, Empty,Empty], [Empty, Empty,Empty]]

    getCellAtPos :: Board -> (Int, Int) -> Cell
    getCellAtPos b (x,y) = (b!!x)!!y

    isEmptyCell :: Cell -> Bool
    isEmptyCell Empty = True
    isEmptyCell _ = True

    checkBounds :: (Int, Int) -> Bool
    checkBounds (x,y) = (x < 3 && x > -1 ) && (y < 3 && y > -1)

    checkValidMove :: Board -> (Int, Int) -> Bool
    checkValidMove b (x,y) = (checkBounds (x,y)) && (isEmptyCell $ getCellAtPos b (x,y))

    makeTurn :: Board -> (Int, Int) -> TurnType -> Board
    makeTurn b (x,y) t
      | checkValidMove b (x, y) =
          if x==0
          then [makeTurnHelper (b!!0) y t] ++ [b!!1] ++ [b!!2]
          else if x == 1
            then [b!!0] ++ [makeTurnHelper (b!!1) y t] ++ [b!!2]
            else [b!!0] ++ [b!!1] ++ [makeTurnHelper (b!!2) y t]
      | otherwise = b

    makeTurnHelper :: [Cell] -> Int -> TurnType -> [Cell]
    makeTurnHelper (x:xs) 0 t = [TurnType t] ++ xs
    makeTurnHelper (x:xs) y t = [x] ++ (makeTurnHelper xs (y-1) t)

    -- hasPlayerAtThisPosWon: We only check for flows starting with current pos/cell
    -- A more general version of it would check all the possible ways
    hasPlayerAtThisPosWon :: Board -> (Int, Int) -> Bool
    hasPlayerAtThisPosWon b (x,y) =
        hasPlayerAtThisPosWonHelper b (x,y) False False ||
        hasPlayerAtThisPosWonHelper b (x,y) False True  ||
        hasPlayerAtThisPosWonHelper b (x,y) True False  ||
        hasPlayerAtThisPosWonHelper b (x,y) True True

    -- Look at this beautiful declaration :P
    hasPlayerAtThisPosWonHelper :: Board -> (Int, Int) -> Bool -> Bool -> Bool
    hasPlayerAtThisPosWonHelper b (x,y) isDiag isVert =
        let
        xdist =
                if (isDiag)
                    then 1
                    else if (isVert)
                            then 0
                            else 1
        ydist =
                if (isDiag)
                    then if (isVert) then (-1) else 1
                    else if (isVert) then 1 else 0
        p1 = getCellAtPos b (x,y)
        p2 = getCellAtPos b ((x+xdist)`mod`3, (y+ydist)`mod`3)
        p3 = getCellAtPos b ((x+2*xdist)`mod`3, (y+2*ydist)`mod`3)

        in
            isSame p1 p2 p3


    isSame :: Cell -> Cell -> Cell -> Bool
    isSame (TurnType Cross) (TurnType Cross) (TurnType Cross) = True
    isSame (TurnType Zero) (TurnType Zero) (TurnType Zero) = True
    isSame _ _ _ = False
