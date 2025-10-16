import Flow
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.List as List
import System.Random as Random;

import qualified Game; import Game (type Game, type Message(Move))
                       
tickRate :: Int
tickRate = 200000

    main :: IO ()
    main = do
        configure_terminal
        shared <- newTVarIO 'w'
        withAsync (readInput shared) $ \writer ->
            withAsync (process shared play (Game.init 12 8)) $ \reader ->
                wait reader >> cancel writer;
    
                
main :: IO ()
main = do
    configure_terminal
    shared <- newTVarIO 'w'
    random_positions <- generate_random_values ((0, 0), (12, 8))
    withAsync (readInput shared) $ \writer ->
        withAsync (process shared play Game.new) $ \reader ->
            wait reader >> cancel writer
            

    configure_terminal :: IO ()
    configure_terminal = do
        hSetBuffering stdin NoBuffering
        hSetEcho stdin False
        hSetBuffering stdout NoBuffering
        hSetBinaryMode stdout True

configure_terminal :: IO ()
configure_terminal = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hSetBuffering stdout NoBuffering
    hSetBinaryMode stdout True


generate_random_values range =
        Random.initStdGen 
        >>= Random.uniformRs range


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

    

