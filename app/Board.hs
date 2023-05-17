module Board
  ( Board
  , hasWon
  , isFull
  , cells
  , getCell
  , update
  , empty
  ) where

import           Cell          (Cell)
import qualified Cell
import           Data.Foldable (Foldable (toList))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Player        (Player (..))

-- | Abstract representation of the 3x3 grid.
newtype Board = Board (Seq Cell)
  deriving Show

-- | Create an empty board.
empty :: Board
empty = Board $ Seq.replicate 9 Cell.Empty

-- | Update a single cell on a board.
update :: Board -> Player -> Int -> Board
update (Board board) player i = Board $ Seq.update i (Cell.Occupied player) board

-- | Retrieve a cell by index (0-based).
getCell :: Board -> Int -> Maybe Cell
getCell (Board board) i = Seq.lookup i board

-- | Check if a player has won the game.
hasWon :: Player -> Board -> Bool
hasWon player board = any (all ((== Just (Cell.Occupied player)) . getCell board)) possibilities
  where
    possibilities =
      [ [0, 1, 2]
      , [3, 4, 5]
      , [6, 7, 8]
      , [0, 3, 6]
      , [1, 4, 7]
      , [2, 5, 8]
      , [0, 4, 8]
      , [2, 4, 6]
      ]

-- | Check if the board is full, which indicates a draw if none of the players won.
isFull :: Board -> Bool
isFull (Board board) = Cell.Empty `notElem` board

-- | Retrieve all cells in row-major order.
cells :: Board -> [Cell]
cells (Board board) = toList board
