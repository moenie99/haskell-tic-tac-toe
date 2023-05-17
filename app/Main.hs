module Main  where

import           Board     (Board)
import qualified Board
import qualified Cell
import           Data.Bool (bool)
import           Data.List (intercalate)
import qualified GHC.Clock
import           Player    (Player)
import qualified Player
import qualified Skynet
import           System.IO (hFlush, stdout)
import           Text.Read (readMaybe)

prompt :: Read a => String -> (a -> Bool) -> IO a
prompt request validate = do
  putStr request
  hFlush stdout
  input <- getLine
  case readMaybe input of
    Just input | validate input -> return input
    _ -> do
      putStrLn "Invalid input."
      prompt request validate

showBoard :: Board -> String
showBoard board = intercalate (pad "\n" bar) (intercalate "|" <$> chunks 3 cells)
  where
    cells = pad " " <$> zipWith showCell [1..9 :: Int] (Board.cells board)

    showCell i Cell.Empty             = show i
    showCell _ (Cell.Occupied player) = show player

    pad s = (s++) . (++s)

    bar = replicate 11 '-'

    chunks _ [] = []
    chunks n xs = let (x, xs') = splitAt n xs in x : chunks n xs'

game :: Player -> Player -> Board -> IO (Maybe Player)
game man current board
  | Board.hasWon other board = return $ Just (Player.other current)
  | Board.isFull board = return Nothing
  | otherwise = do
      position <- if man == current then getPlayerMove else getSkynetMove
      let newBoard = Board.update board current position
      putStrLn $ showBoard newBoard
      game man other newBoard
  where
    other = Player.other current

    getPlayerMove = subtract 1 <$>
      prompt "Input a number: " ((== Just Cell.Empty) . Board.getCell board . subtract 1)

    getSkynetMove = putStrLn "Skynet is generating a move..." >>
      return (Skynet.generateMove current board)

randPlayer :: IO Player
randPlayer = bool Player.X Player.O . even <$> GHC.Clock.getMonotonicTimeNSec

main :: IO ()
main = do
  putStrLn "Welcome to the best game of tic-tac-toe you will ever play!"
  man <- prompt "Which symbol do you wish to play as? (X/O) " (const True)
  startingPlayer <- randPlayer
  outcome <- game man startingPlayer Board.empty
  case outcome of
    -- This will never happen
    Just player | player == man -> putStrLn "Humanity prevails another day!"
    Just _                      -> putStrLn "Skynet has taken over. Initiating self-destruction."
    Nothing                     -> putStrLn "A draw! Maybe mankind will prevail after all?"
