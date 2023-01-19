module Language.Brainie.CST.Literal where
  data Literal
    = IntLit Integer 
    | FloatLit Double
    | StringLit String
    | CharLit Char
    deriving Eq

  instance Show Literal where
    show (IntLit i) = show i
    show (FloatLit f) = show f
    show (StringLit s) = show s
    show (CharLit c) = show c