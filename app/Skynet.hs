{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Skynet
  ( generateMove
  , buildGametree
  , minMaxTree
  ) where

import           Board      (Board)
import qualified Board
import qualified Cell
import           Data.List  (elemIndices, find)
import           Data.Maybe (fromMaybe)
import           Data.Tree  (Tree (Node, subForest), foldTree, rootLabel,
                             unfoldTree)
import           Player     (Player)
import qualified Player

-- | State of the game, generic over 'state' to account for different phases of move finding.
data GameState a = GameState
  { position :: Int    -- ^ The position that 'player' made their move on.
  , player   :: Player -- ^ The player that made the last move at 'position'.
  , state    :: a      -- ^ The state of the game, including extra information.
  } deriving (Show, Functor)

-- | Given the current player and a board, construct a tree of all possible future states.
buildGametree :: Player -> Board -> Tree (GameState Board)
buildGametree player board = unfoldTree go (-1, player, board)
  where
    go (path, player, board) = (GameState path player board, map seedFromPath freeIndices)
      where
        other = Player.other player

        freeIndices
          -- It is not neccessary to check 'Board.hasWon current', as it is impossible
          -- for the turn taking players move to directly cause the win of the other.
          | Board.hasWon other board = []
          -- subsumes 'Board.isFull board' as this implies 'Board.cells board = []'
          | otherwise                = elemIndices Cell.Empty (Board.cells board)

        seedFromPath path = (path, other, Board.update board player path)

-- | The strategy, by which each player selects an ideal score from a list of scores.
strategy :: Player -> [Int] -> Int
strategy Player.O = foldr1 (\x y -> if x == minBound then x else min x y)
strategy Player.X = foldr1 (\x y -> if x == maxBound then x else max x y)
-- Note that this way of defining minimum and maximum short cuircuits,
-- taking into account the knowledge of the bounds of Int. This helps avoiding
-- construction of branches that will get discarded either way, dramatically
-- reducing time and memory consumption.

-- | Annotate each state with its score from the view of the turn-taking player.
minMaxTree :: Tree (GameState Board) -> Tree (GameState (Board, Int))
minMaxTree = foldTree go
  where
    go move children = Node (fmap (,score) move) children
      where
        board = state move

        score
          | Board.hasWon Player.X board = maxBound
          | Board.hasWon Player.O board = minBound
          | Board.isFull board          = 0        -- Draw
          | otherwise                   =
              strategy (player move) (snd . state . rootLabel <$> children)

-- | This optimization to the minimax algorithm ensures that if there is a move that
-- guarantees an immediate victory, it will be picked over other moves that are also
-- considered "optimal" from the perspective of the algorithm
selectOptimalMove :: Player -> [GameState (Board, Int)] -> Int
selectOptimalMove player optimalMoves = position $ fromMaybe
  (head optimalMoves)                                     -- other "optimal" move
  (find (Board.hasWon player . fst . state) optimalMoves) -- guaranteed win

-- | Generate an "optimal" move using the minimax algorithm.
generateMove :: Player -> Board -> Int
generateMove player board = selectOptimalMove player optimalMoves
  where
    potentialStates = rootLabel <$> subForest (minMaxTree (buildGametree player board))

    optimalScore = strategy player (snd . state <$> potentialStates)

    optimalMoves = filter ((== optimalScore) . snd . state) potentialStates
