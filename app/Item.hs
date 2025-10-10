module Item where
    import qualified Position; import Position (type Position)

    data Item 
        = Apple { position :: Position }

    instance Show Item where
        show = \case
            Apple { position } -> "Apple " ++ show position