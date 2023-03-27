module Language.LambdaCalculus.Parser (parseTerm, ParserResult(..)) where

import Data.Foldable (foldl', foldr')

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.List.NonEmpty (NonEmpty((:|)))

import Language.LambdaCalculus (Var(..), Term(..))

type Parser = Parsec Void String

charws :: Char -> Parser Char
charws c = char c <* space

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

var :: Parser Var
var  =  Var . pure <$> letterChar <* space
    <|> Var <$> (charws '{' *> many1 alphaNumChar <* charws '}')
    <?> "variable"

churchNum :: Parser Term
churchNum = church . read <$> (try (many1 (digitChar <* char '\x0305')) <|> many1 digitChar) <* space
  where church n = Abs (Var "x") (Abs (Var "y") (xypow n))
        xypow 0 = TermVar (Var "y")
        xypow n = App (TermVar (Var "x")) (xypow (n-1))

powerTerm :: Term -> Int -> Term -> Term
powerTerm _ 0 n = n
powerTerm m p n = App m (powerTerm m (p-1) n)

power :: Parser Int
power = read <$> (char '^' *> many1 digitChar <|> map unsuper <$> many1 (oneOf "⁰¹²³⁴⁵⁶⁷⁸⁹")) <* space
  where unsuper '⁰' = '0'
        unsuper '¹' = '1'
        unsuper '²' = '2'
        unsuper '³' = '3'
        unsuper '⁴' = '4'
        unsuper '⁵' = '5'
        unsuper '⁶' = '6'
        unsuper '⁷' = '7'
        unsuper '⁸' = '8'
        unsuper '⁹' = '9'

unit :: Parser Term
unit = (charws '(' *> term <* charws ')') <|> TermVar <$> var <|> churchNum

term :: Parser Term
term  =  flip (foldr' Abs) <$> (oneOf ['λ', '\\'] *> space *> many1 var <* charws '.') <*> term
     <|> try (foldl' App <$> (powerTerm <$> unit <*> (option 1 power) <*> unit) <*> many unit)
     <|> unit

fullTerm :: Parser Term
fullTerm = hspace *> (char '\'' *> term <* char '\'' <|> term) <* eof

data ParserResult = Error String | Incomplete | Success Term

handleParserResult :: Either (ParseErrorBundle String Void) Term -> ParserResult
handleParserResult (Left (ParseErrorBundle { bundleErrors = (TrivialError _ (Just EndOfInput) _) :| _ })) = Incomplete
handleParserResult (Left err) = Error (errorBundlePretty err)
handleParserResult (Right m) = Success m

parseTerm :: String -> ParserResult
parseTerm = handleParserResult . runParser fullTerm ""
