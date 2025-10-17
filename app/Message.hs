module Message where
    import Flow
    import Text.Read
    import qualified Direction; import Direction (type Direction)


    data Message 
        = Move Direction


    instance Show Message where
        show = \case
            (Move direction) -> "Move(" ++ show direction ++ ")"


    instance Read Message where
        readPrec = 
            readPrec @Direction
            |> fmap Move