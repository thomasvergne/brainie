module Language.Brainie.CST.Expression where
  import Language.Brainie.CST.Located ( Located )
  import Language.Brainie.CST.Literal ( Literal )

  data CST_Expression
    = CST_EBinary String (Located CST_Expression) (Located CST_Expression)
    | CST_EUnary String (Located CST_Expression)
    | CST_ELiteral Literal
    | CST_EVariable String
    | CST_EFunctionCall String [Located CST_Expression]
    deriving (Eq, Show)

  data CST_Statement 
    = CST_SExpression (Located CST_Expression)
    | CST_SFunction String [String] [Located CST_Statement]
    | CST_SReturn (Located CST_Expression)
    | CST_SIf (Located CST_Expression) [Located CST_Statement] [Located CST_Statement]  
    deriving (Eq, Show)
