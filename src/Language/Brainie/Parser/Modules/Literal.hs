module Language.Brainie.Parser.Modules.Literal where
  import Language.Brainie.CST.Expression ( CST_Expression(..) )
  import Control.Applicative ( Alternative((<|>)) )
  import Language.Brainie.CST.Literal ( Literal(..) )

  import qualified Text.Parsec as P
  import qualified Language.Brainie.Parser.Lexer as L
  import qualified Text.Parsec.Token as Token

  literal :: Monad m => L.Brainie m CST_Expression
  literal = L.locate $ CST_ELiteral <$> literal'

  literal' :: Monad m => L.Parser m Literal
  literal' = P.try floatLiteral
          <|> intLiteral
          <|> stringLiteral
          <|> charLiteral

  intLiteral :: Monad m => L.Parser m Literal
  intLiteral = IntLit <$> L.integer
  
  floatLiteral :: Monad m => L.Parser m Literal
  floatLiteral = FloatLit <$> Token.float L.lexer

  stringLiteral :: Monad m => L.Parser m Literal
  stringLiteral = StringLit <$> Token.stringLiteral L.lexer

  charLiteral :: Monad m => L.Parser m Literal
  charLiteral = CharLit <$> Token.charLiteral L.lexer