module APL.Parser (parseAPL, lInteger, pExp, lVName) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    parseTest,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)


-- Do not change this definition.
type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

lInteger :: Parser Integer
lInteger = lexeme (read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlpha))

lVName :: Parser VName
lVName = lexeme $ do 
  a <- satisfy isAlpha
  an <- many (satisfy isAlphaNum)
  let v = a:an
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

lKeyword :: String -> Parser ()
lKeyword kw = lexeme $ void $ try $ chunk kw <* notFollowedBy (satisfy isAlphaNum)

keywords :: [String]
keywords =
  [ "if",
    "then",
    "else",
    "true",
    "false"
  ]


pBool :: Parser Bool
pBool = choice [
  True <$ lKeyword "true",
  False <$ lKeyword "false"
  ]

-- pExp :: Parser Exp
-- pExp = choice [
--   CstInt <$> lInteger,
--   CstBool <$> pBool,
--   Var <$> lVName
--   ]

pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> pBool,
      Var <$> lVName,
      lString "(" *> pExp <* lString ")"
    ]

pLExp :: Parser Exp
pLExp =
  choice
    [ If
        <$> (lKeyword "if" *> pExp0)
        <*> (lKeyword "then" *> pExp0)
        <*> (lKeyword "else" *> pExp0),
      pAtom
    ]

pExp1 :: Parser Exp
pExp1 = pLExp >>= chain
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pLExp
            chain $ Mul x y,
          do
            lString "/"
            y <- pLExp
            chain $ Div x y,
          pure x
        ]

pExp0 :: Parser Exp
pExp0 = pExp1 >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pExp1
            chain $ Add x y,
          do
            lString "-"
            y <- pExp1
            chain $ Sub x y,
          pure x
        ]

pExp :: Parser Exp
pExp = pExp0


-- Do not change this definition.
parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x
