module Snake2 where
    import Flow
    import qualified Position; import Position (type Position)
    import qualified Data.List as List

    type Snake = [Segment]

    data Segment = 
        = Segment Position
        | Upgrade Position


    new :: Position -> Snake
    new position =
        [Segment position]

    move :: Direction -> Snake -> Snake
    move direction = \case
        [] -> []
        snake@(current:rest) ->
            current
            |> get_position
            |> Position.add direction
            |> Segment
            |> \state -> move' state snake


    eat :: Direction -> Snake -> Snake
    eat direction = \case
        [] -> []
        snake@(current:rest) ->
            current
            |> get_position
            |> Position.add direction
            |> Upgrade
            |> \state -> move' state snake


    get_position :: Segment -> Position
    get_position = \case
        Segment pos -> pos
        Upgrade pos -> pos


    move' :: Segment -> Snake -> Snake
    move' state snake =
        case state, snake of
            Segment new_pos, [] ->
                []

            Upgrade new_pos, [] ->
                Segment new_pos : []

            state, (current : rest) ->
                let new_state = current in
                let new_current = state in
                new_segment : (move' new_state rest)

            -- Segment new_pos, (Segment pos : rest) ->
            --     let new_state = Segment pos in
            --     let new_segment = Segment new_pos in
            --     new_segment : (move' new_state rest)

            -- Upgrade new_pos, (Segment pos : rest) -> 
            --     let new_state = Segment pos in
            --     let new_segment = Upgrade new_pos in
            --     new_segment : (move' new_state rest)

            -- Segment new_pos, (Upgrade pos : rest) ->
            --     let new_state = Upgrade pos in
            --     let new_segment = Segment new_pos in
            --     new_segment : (move' new_state rest)

            -- Upgrade new_pos, (Upgrade pos: rest) ->
            --     let new_state = Upgrade pos in
            --     let new_segment = Upgrade new_pos in
            --     new_segment : (move' new_state rest)

            


