import Flow
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.List as List
import System.Random as Random;


import qualified Game; import Game (type Game)
import qualified Message; import Message (type Message)
import qualified Position; import Position (type Position)
import qualified Position;

                          
tickRate :: Int
tickRate = 200000
    
                
main :: IO ()
main = do
    configure_terminal
    shared <- newTVarIO 'w'
    random_positions <- generate_random_values @Position ((0, 0), (12, 8))
    withAsync (readInput shared) $ \writer ->
        withAsync (process (play random_positions) shared Game.init) $ \reader ->
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
readInput shared =
    getChar >>= \case
        'c' -> 
            return () 
        key -> do
            atomically (writeTVar shared key)
            readInput shared


process :: Show state => (Char -> state -> state) -> TVar Char -> state -> IO ()
process fn shared state = do
    threadDelay tickRate
    update <- fn <$> atomically (readTVar shared)
    let state' = update state
    print state'
    process fn shared state' 


play :: [Position] -> Char -> Game -> Game
play random_positions = 
    List.singleton
    .> read @Message
    .> Game.update random_positions 

    

