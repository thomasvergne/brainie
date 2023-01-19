module Language.Brainie.Parser.Modules.Operators where
  import Language.Brainie.CST.Located ( Located((:>:)) )
  import Language.Brainie.CST.Expression ( CST_Expression(..) )
  import Control.Applicative ( Alternative(some) )

  import qualified Language.Brainie.Parser.Lexer as L
  import qualified Text.Parsec.Expr as E
  import qualified Text.Parsec as P
  
  makeUnaryOp :: Alternative f => f (a -> a) -> f (a -> a)
  makeUnaryOp s = foldr1 (.) . reverse <$> some s

  operatorsTable :: Monad m => [[E.Operator String () m (Located CST_Expression)]]
  operatorsTable = do
    [
        equalities,
        [ E.Infix 
            (L.reservedOp "*" >> 
              return (\x@(_ :>: (s, _)) y@(_ :>: (_, e)) -> CST_EBinary "*" x y :>: (s, e))) 
            E.AssocLeft,
          E.Infix 
            (L.reservedOp "/" >> 
              return (\x@(_ :>: (s, _)) y@(_ :>: (_, e)) -> CST_EBinary "/" x y :>: (s, e))) 
            E.AssocLeft ],
        [ E.Prefix 
            (makeUnaryOp $ L.reservedOp "-" >> return (\x -> CST_EUnary "-" x :>: (L.getLoc x))) ],
        [ E.Infix 
            (L.reservedOp "+" >> 
              return (\x@(_ :>: (s, _)) y@(_ :>: (_, e)) -> CST_EBinary "+" x y :>: (s, e))) 
            E.AssocLeft,
          E.Infix 
            (L.reservedOp "-" >> 
              return (\x@(_ :>: (s, _)) y@(_ :>: (_, e)) -> CST_EBinary "-" x y :>: (s, e))) 
            E.AssocLeft ]
      ]
    where equalityOp = ["==", "!=", "<", ">", "<=", ">="]
          equalities = map (\op -> E.Infix (L.reservedOp op >> return (\x@(_ :>: (s, _)) y@(_ :>: (_, e)) -> CST_EBinary op x y :>: (s, e))) E.AssocLeft) equalityOp

  operator :: Monad m => L.Parser m Char
  operator = P.choice $ map (\x -> L.reserved [x] >> return x) L.operators

  operators :: Monad m => L.Parser m String
  operators = some operator