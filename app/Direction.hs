{-# LANGUAGE LambdaCase #-}

module Direction where
    import Text.Read

    data Direction =
        Up
        | Down
        | Left
        | Right

    instance Read Direction where
        readPrec =
            tokenize >>= \case
                Ident "w" -> return Direction.Up
                Ident "s" -> return Direction.Down
                Ident "a" -> return Direction.Left
                Ident "d" -> return Direction.Right
                _ -> failParsing
            where 
                tokenize = lexP
                failParsing = pfail

    instance Show Direction where
        show = \case
            Direction.Up -> "Up"
            Direction.Down -> "Down"
            Direction.Left -> "Left"
            Direction.Right -> "Right"


