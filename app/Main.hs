
import Data.Functor ((<&>))

import qualified Direction; import Direction (type Direction(Up, Down, Left, Right))
import Control.Monad
import Data.Foldable
import Debug.Trace

import Data.Function ((&))
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import qualified Game; import Game (type Game, type Message(Move))

tickRate :: Int
tickRate = 200000

main :: IO ()
main = do
    configure
    shared <- newTVarIO 'w'
    withAsync (readInput shared) $ \writer ->
        withAsync (process shared play Game.new) $ \reader ->
            wait reader >> cancel writer
            

configure :: IO ()
configure = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hSetBuffering stdout NoBuffering
    hSetBinaryMode stdout True

readInput :: TVar Char -> IO ()
readInput shared = do
    key <- getChar
    if key == 'c' then 
        return () 
    else do 
        atomically (writeTVar shared key)
        readInput shared

process :: Show state => TVar Char -> (Char -> state -> state) -> state -> IO ()
process shared fn state = do
    threadDelay tickRate
    item <- atomically (readTVar shared)
    print item
    let state' = fn item state
    print state'
    process shared fn state' 


play :: Char -> Game -> Game
play = 
    singleton
    .> read @Message
    .> Game.update

    

