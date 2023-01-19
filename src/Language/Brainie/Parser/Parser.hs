module Language.Brainie.Parser.Parser where
  import Language.Brainie.CST.Located ( Located(..) )
  import Language.Brainie.CST.Literal ( Literal(..) )
  import Language.Brainie.CST.Expression ( CST_Expression(..), CST_Statement(..) )
  import Control.Applicative ( Alternative((<|>), some) )
  import Control.Monad.State ( void, modify )

  import qualified Text.Parsec as P
  import qualified Language.Brainie.Parser.Lexer as L
  import qualified Text.Parsec.Expr as E
  import qualified Text.Parsec.Token as Token

  import Language.Brainie.Parser.Modules.Operators
    ( operators, operatorsTable )
  import Language.Brainie.Parser.Modules.Literal
    ( literal, stringLiteral )
  
  parseBrainie :: String -> String -> Either P.ParseError [Located CST_Statement]
  parseBrainie file x = P.runParser (P.sepEndBy parser (P.optionMaybe L.semi) <* P.eof) () file x

  parser :: Monad m => L.Brainie m CST_Statement
  parser = (L.whiteSpace *> cstStatement)

  -- Statement parsing
  
  cstStatement :: Monad m => L.Brainie m CST_Statement
  cstStatement = P.choice [
      cstSFunction   P.<?> "function definition",
      cstSIf         P.<?> "if statement",
      cstSReturn     P.<?> "return statement",
      cstSExpression P.<?> "expression"
    ]
  
  cstSExpression :: Monad m => L.Brainie m CST_Statement
  cstSExpression = L.locate $ CST_SExpression <$> expression

  cstSIf :: Monad m => L.Brainie m CST_Statement
  cstSIf = L.locate $ do
    L.reserved "if"
    cond <- expression
    L.reserved "then"
    then' <- block
    L.reserved "else"
    else' <- ((:[]) <$> cstSIf) <|> block
    return $ CST_SIf cond then' else'
  
  cstSFunction :: Monad m => L.Brainie m CST_Statement
  cstSFunction = L.locate $ do
    L.reserved "function"
    name <- L.identifier
    args <- P.many L.identifier
    body <- block
    return $ CST_SFunction name args body
  
  cstSReturn :: Monad m => L.Brainie m CST_Statement
  cstSReturn = L.locate $ do
    L.reserved "return"
    expr <- expression
    return $ CST_SReturn expr
  
  block :: Monad m => L.Parser m [Located CST_Statement]
  block = Token.braces L.lexer $ P.many cstStatement