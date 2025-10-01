module Game where
    
    import Flow
    
    import qualified Position; import Position (type Position)
    import qualified Direction; import Direction (type Direction(Up, Down, Left, Right))
    import qualified Snake; import Snake (type Snake)
    import qualified Item; import Item (type Item(Apple))


    data Game = Game {
        width :: Int,
        height :: Int,
        snake :: Snake,
        items :: [Item]
    }

    data Message = Move Direction

    new :: Game
    new = Game {
        width = 12,
        height = 8,
        snake = Snake.new (6, 4),
        items = []
    }

    update :: Message -> Game -> Game
    update (Move direction) game = 
        let {
            snake = game.snake
            |> Snake.move direction
            |> Snake.map (overflow game)
        } in 
            game { snake }
        

            

    overflow :: Game -> Position -> Position
    overflow game (x, y) =
        let {
            x' = mod x game.width;
            y' = mod y game.height;
        } in
            (x', y')


    indent :: String -> String
    indent = lines 
        .> fmap ("\n\t" ++)
        .> foldl (++) ""


    instance Show Game where
        show self = 
            "Game {\n" 
            ++ "\twidth = " ++ show self.width ++ ",\n"
            ++ "\theight = " ++ show self.height ++ ",\n"
            ++ "\tsnake = \n" ++ indent (show self.snake)  ++ ",\n"
            ++ "\titems = " ++ show self.items ++ ",\n"
            ++ "}"


    -- getKeys :: IO [Char]
    -- getKeys = reverse <$> recurse "" where { 
    --     recurse sequence = do
    --         keys <- hGetContents stdin
    --         sequence <- hGetChar stdin <&> (\item -> item : sequence)
    --         recurse sequence
    -- }