module Cell
  ( Cell(..)
  ) where

import Player (Player)

-- | Cells on the grid
data Cell
  = Occupied Player -- ^ A cell, occupied by either X or O.
  | Empty           -- ^ An empty cell.
  deriving (Eq, Show)
