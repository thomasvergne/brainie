module Language.Brainie.CST.Located where
  import Text.Parsec (SourcePos)

  type Position = (SourcePos, SourcePos)

  data Located a
    = a :>: Position
    deriving Eq
  
  instance Show a => Show (Located a) where
    show (a :>: _) = show a
  
  instance Functor Located where
    fmap f (a :>: p) = f a :>: p