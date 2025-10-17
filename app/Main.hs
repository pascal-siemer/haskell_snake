import Flow
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.List as List
import System.Random as Random;


import qualified Game; import Game (type Game)
import qualified Message; import Message (type Message)
import qualified Position;

                          
tickRate :: Int
tickRate = 200000
    
                
main :: IO ()
main = do
    configure_terminal
    shared <- newTVarIO 'w'
    random_positions <- generate_random_values @(Int, Int) ((0, 0), (12, 8))
    withAsync (readInput shared) $ \writer ->
        withAsync (process shared play Game.init) $ \reader ->
            wait reader >> cancel writer
            

configure_terminal :: IO ()
configure_terminal = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hSetBuffering stdout NoBuffering
    hSetBinaryMode stdout True


generate_random_values :: UniformRange a => (a, a) -> IO [a]
generate_random_values range =
    Random.initStdGen 
    |> fmap (Random.uniformRs range)


readInput :: TVar Char -> IO ()
readInput shared = do
    key <- getChar
    if  | key == 'c' -> return () 
        | otherwise -> do
            atomically (writeTVar shared key);
            readInput shared;


process :: Show state => TVar Char -> (Char -> state -> state) -> state -> IO ()
process shared fn state = do
    threadDelay tickRate
    item <- atomically (readTVar shared)    
    let state' = fn item state
    print state'
    process shared fn state' 


play :: Char -> Game -> Game
play = 
    List.singleton
    .> read @Message
    .> Game.update

    

