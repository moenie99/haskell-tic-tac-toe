module Player
  ( Player(..)
  , other
  ) where

-- | The players of Tic-Tac-Toe.
data Player
  = X -- ^ Cross
  | O -- ^ Naught
  deriving (Show, Eq, Read)

-- | Get the other player.
other :: Player -> Player
other X = O
other O = X
