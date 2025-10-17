module Game where
    import Flow
    import qualified Position; import Position (type Position)
    import qualified Direction; import Direction (type Direction(Up, Down, Left, Right))
    import qualified Message; import Message (type Message(Move))
    import qualified Snake; import Snake (type Snake)
    import qualified Item; import Item (type Item(Apple))

    import qualified Data.List.Split as List
    import qualified System.Random as Random
    import Data.Foldable (fold)


    data Game = Game {
        width :: Int,
        height :: Int,
        snake :: Snake,
        items :: [Item]
    }


    instance Show Game where 
        show game = 
            indexes
            |> fmap (\index -> if any (== index) segments then 'x' else '_')
            |> List.chunksOf game.width
            |> foldMap (++ "\r\n")
            where 
                length = game.width * game.height
                indexes = [0..(length - 1)]
                segments = fmap (\(x, y) -> x + (y * game.width)) game.snake.segments


    init :: Game
    init = Game {
        width = 12,
        height = 8,
        snake = Snake.new (6, 4),
        items = []
    }


    update :: [Position] -> Message -> Game -> Game
    update random_positions (Move direction) game = 
        game.snake
        |> Snake.move direction
        |> Snake.map (overflow game)
        |> \snake -> game { snake }


    overflow :: Game -> Position -> Position
    overflow game (x, y) =
        let x' = mod x game.width in
        let y' = mod y game.height in
        (x', y')


    indent :: String -> String
    indent = 
        lines 
        .> fmap ("\n\t" ++)
        .> foldl (++) ""
