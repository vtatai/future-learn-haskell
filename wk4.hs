import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr

-- 1st part
data Tag = MkTag String deriving Show 

parseDiv :: Parser Tag
parseDiv = do 
  string "<div>" 
  return (MkTag "div")

-- 2nd part
bag_bog :: Parser String
bag_bog =
  do  xs <- string "bag" <|> string "bog"
      return xs

bag_bog_try :: Parser String
bag_bog_try =
  do  xs <- try (string "bag") <|> string "bog"
      return xs

-- 3rd part
varname :: Parser String
varname =
  do  x <- letter
      xs <- many (letter <|> digit)
      return (x:xs)


-- 4th part
{-expr_parser :: Parser Expr-}
{-expr_parser = buildExpressionParser optable term <?> "expression"-}

{-optable =-}
  {-let-}
    {-op name assoc   =-}
      {-Infix ( do {  reservedOp name;-}
          {-return (\x y ->(Op (MkOpExpr name x y))) } ) assoc-}
    {-prefix name =-}
      {-Prefix  (-}
        {-reservedOp name >>-}
            {-return (\x->(Pref (MkPrefixOpExpr name x))) )-}
  {-in-}
    {-[ [ op "*"  AssocLeft, op "/"  AssocLeft, op "%" AssocLeft ]-}
    {-, [ op "+"  AssocLeft, op "-"  AssocLeft ], [ prefix "-" ] ]-}

