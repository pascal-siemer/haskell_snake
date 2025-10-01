module Snake where
    import Flow
    import qualified Direction; import Direction (type Direction);
    import qualified Position; import Position (type Position)

    data Snake = Snake {
        segments :: [Position]
    }

    new :: Position -> Snake
    new position = Snake { 
        segments = [position] 
    }

    map :: (Position -> Position) -> Snake -> Snake
    map f snake = 
        Snake { segments }
        where segments = fmap f snake.segments

    move :: Direction -> Snake -> Snake
    move direction snake =
        case snake.segments of 
            [] -> snake
            (item:rest) -> snake {
                segments = (item |> Position.add direction) : (rest |> dropLast)
            }

            


    dropLast :: [a] -> [a]
    dropLast [] = []
    dropLast [_] = []
    dropLast (item:rest) = item : dropLast rest

    instance Show Snake where
        show self =
            let segments = show self.segments in
            "Snake {\n"
            ++ "\tsegments = " ++ segments ++ ",\n"
            ++ "}"