module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt idx f xs = take (idx) xs ++ [f x] ++ drop (idx + 1) xs
  where x = xs !! idx

------------

type Cell = Integer
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
          liftIO $ print $ show cur
          modifyCurrent $ const 0


------------

data Command = MoveLeft | MoveRight | Inc | Dec | Dump | Loop [Command]

doCommand :: BFState () -> Command -> BFState ()
doCommand s MoveLeft = s >> left
doCommand s MoveRight = s >> right
doCommand s Inc = s >> inc
doCommand s Dec = s >> dec
doCommand s Dump = s >> dump
doCommand s (Loop commands) = s >> do cur <- currentValue
                                      if cur == 0
                                         then return ()
                                         else let newState = foldl' doCommand (return ()) commands
                                              in doCommand newState (Loop commands)

run :: [Command] -> Underlying (((), Head), Board)
run commands = runBFState $ foldl' doCommand (return ()) commands
