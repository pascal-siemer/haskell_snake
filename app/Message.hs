module Message where

    data Message 
        = Move Direction

    instance Show Message where
        show = \case
            (Move direction) -> "Move(" ++ show direction ++ ")"


    instance Read Direction where
        readPrec = 
            readPrec @Direction