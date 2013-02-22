module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List
import Data.Char
import Text.ParserCombinators.Parsec

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt idx f xs = take (idx) xs ++ [f x] ++ drop (idx + 1) xs
  where x = xs !! idx

display :: String -> String
display str = [ if x == ',' then ' ' else x | x <- str ]

------------

type Cell = Int
type Board = [Cell]
type Head = Int

type Underlying = IO
type BoardState = StateT Board Underlying
type BFState = StateT Head BoardState

initialHead :: Head
initialHead = 0

initialBoard :: Board
initialBoard = [0, 0, 0, 0]

runBFState :: BFState a -> Underlying ((a, Head), Board)
runBFState s = runStateT (runStateT s initialHead) initialBoard

currentValue :: BFState Cell
currentValue = do h <- get
                  lift $ do board <- get
                            return $ board !! h

modifyCurrent :: (Cell -> Cell) -> BFState ()
modifyCurrent f = do h <- get
                     lift $ modify (updateAt h f)

inc :: BFState ()
inc = modifyCurrent succ

dec :: BFState ()
dec = do cur <- currentValue
         if cur == 0
            then return ()
            else modifyCurrent pred

left :: BFState ()
left = do h <- get
          if h == 0 then return () else modify pred

right :: BFState ()
right = do modify succ
           h <- get
           lift $ do board <- get
                     if h < length board
                        then return ()
                        else modify (++ [0 | _ <- board])

dump :: BFState ()
dump = do cur <- currentValue
          liftIO $ putChar (chr cur)

input :: BFState ()
input = do char <- liftIO getChar
           modifyCurrent $ const (ord char)

------------

data Command = MoveLeft | MoveRight | Inc | Dec | Dump | Input | Loop [Command] deriving (Show)

doCommand :: BFState () -> Command -> BFState ()
doCommand s MoveLeft = s >> left
doCommand s MoveRight = s >> right
doCommand s Inc = s >> inc
doCommand s Dec = s >> dec
doCommand s Dump = s >> dump
doCommand s Input = s >> input
doCommand s (Loop commands) = s >> do cur <- currentValue
                                      if cur == 0
                                         then return ()
                                         else let newState = foldl' doCommand (return ()) commands
                                              in doCommand newState (Loop commands)

run :: [Command] -> Underlying (((), Head), Board)
run commands = runBFState $ foldl' doCommand (return ()) commands

------------

op :: Char -> Command
op c = case c of
  '<' -> MoveLeft
  '>' -> MoveRight
  '+' -> Inc
  '-' -> Dec
  '.' -> Dump
  ',' -> Input

simpleExpr :: Parser Command
simpleExpr = do c <- oneOf "<>+-.,"
                return $ op c

loopExpr :: Parser Command
loopExpr = do char '['
              exprs <- many expr
              char ']'
              return $ Loop exprs

expr :: Parser Command
expr = simpleExpr <|> loopExpr

program :: Parser [Command]
program = many1 expr

------------

main :: IO ()
main = forever $ do putStr "enter program: "
                    prog <- getLine
                    case parse program "" prog of
                      Left err       -> putStrLn $ show err
                      Right commands -> do putStr "output: "
                                           (_, board ) <- run commands
                                           putStrLn $ "\nboard: " ++ display (show board)
                                           putStrLn ""
