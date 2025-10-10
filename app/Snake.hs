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

    any :: (Position -> Bool) -> Snake -> Bool
    any condition snake =
        Prelude.any condition snake.segments

    move :: Direction -> Snake -> Snake
    move direction snake =
        case snake.segments of 
            [] -> snake
            (item:rest) -> snake {
                segments = (item |> Position.add direction) : (rest |> dropLast)
            }

    prepend :: Direction -> Snake -> Snake
    prepend direction snake =
        case snake.segments of
            [] -> snake
            (item:rest) -> 
                let new_head = item |> Position.add direction in 
                snake { 
                    segments = (new_head:item:rest) 
                }

            


    dropLast :: [a] -> [a]
    dropLast = \case
        [] -> []
        [_] -> []
        (item:rest) -> item : dropLast rest

    instance Show Snake where
        show self =
            let segments = show self.segments in
            "Snake {\n"
            ++ "\tsegments = " ++ segments ++ ",\n"
            ++ "}"