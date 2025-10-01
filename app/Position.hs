module Position where
    import qualified Direction; import Direction (type Direction(Up, Down, Left, Right))

    type Position = (Int, Int)

    x :: Position -> Int
    x (x, _) = x

    y :: Position -> Int
    y (_, y) = y

    add :: Direction -> Position -> Position
    add direction (x, y) =
        case direction of
            Direction.Up -> (x, y + 1)
            Direction.Down -> (x, y - 1)
            Direction.Left -> (x - 1, y)
            Direction.Right -> (x + 1, y)
