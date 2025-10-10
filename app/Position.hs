module Position where
    import qualified Direction; import Direction (type Direction(Up, Down, Left, Right))

    type Position = (Int, Int)

    x :: Position -> Int
    x (a, _) = a

    y :: Position -> Int
    y (_, b) = b

    add :: Direction -> Position -> Position
    add direction (a, b) =
        case direction of
            Direction.Up -> (a, b - 1)
            Direction.Down -> (a, b + 1)
            Direction.Left -> (a - 1, b)
            Direction.Right -> (a + 1, b)
