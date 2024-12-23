{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..),
    runP,
    pChar,
    parseError,
    pAbbr,
    pEof,
    parseExpr,
    pE,
    pT,
    pF,
    pEE,
    pTT,
    pDouble,
    pParE,
    skipWS,
    skipChar,
    val,
  )
where

import Control.Applicative
import Control.Monad
import Data.Char (digitToInt, isDigit, isSpace, isUpper)
import Data.Scientific
import HW4.T1 (ExceptState (..))
import HW4.Types (Annotated ((:#)), Except (..), Expr (Op, Val), Prim (..))
import Numeric.Natural (Natural)

newtype ParseError = ErrorAtPos Natural
  deriving (Show)

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P state) str =
  let res = runES state (0, str)
   in case res of
        Error e -> Error e
        Success (a :# _) -> Success a

-- Just an example of parser that may be useful
-- in the implementation of 'parseExpr'
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    [] -> Error (ErrorAtPos pos)
    (c : cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (<|>) (P state1) (P state2) = P $ ES $ \s ->
    let res = runES state1 s
     in case res of
          Error _ -> runES state2 s
          _ -> res

-- No metohds
instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) ->
  case s of
    [] -> Success (() :# (pos, s))
    _ -> Error (ErrorAtPos pos)

-- For instance, here is a parser that accepts only non-empty sequences
-- of uppercase letters:
pAbbr :: Parser String
pAbbr = do
  abbr <- some (mfilter Data.Char.isUpper pChar)
  pEof
  return abbr

-- E  -> TE'
-- E' -> +TE' | eps
-- T -> FT'
-- T' -> FT' | eps
-- F -> VAL | (E)

parseExpr :: String -> Except ParseError Expr
parseExpr = runP $ do
  expr <- pE
  pEof
  return expr

pE :: Parser Expr
pE = do
  tValue <- pT
  pEE tValue

pT :: Parser Expr
pT = do
  fValue <- pF
  pTT fValue

pF :: Parser Expr
pF = val <|> pParE

val :: Parser Expr
val = fmap Val pDouble

-- E in parentheses : (E)
pParE :: Parser Expr
pParE = do
  skipWS
  skipChar '('
  skipWS
  expr <- pE
  skipWS
  skipChar ')'
  skipWS
  return expr

pEE :: Expr -> Parser Expr
-- pEE tValue = do
--   skipChar '+'
--   t1Value <- pT
--   pEE $ tValue + t1Value
pEE tValue = doOperation Add '+' tValue pT <|> doOperation Sub '-' tValue pT <|> return tValue

doOperation :: (Expr -> Expr -> Prim Expr) -> Char -> Expr -> Parser Expr -> Parser Expr
doOperation op sign tValue cons = do
  skipChar sign
  t1Value <- cons
  pEE $ Op $ op tValue t1Value

pTT :: Expr -> Parser Expr
pTT fValue = doOperation Mul '*' fValue pF <|> doOperation Div '/' fValue pF <|> return fValue

skipWS :: Parser ()
skipWS = void $ many $ pSatisfiedChar isSpace

skipChar :: Char -> Parser ()
skipChar char = void $ pSatisfiedChar (== char)

pSatisfiedChar :: (Char -> Bool) -> Parser Char
pSatisfiedChar condition = mfilter condition pChar

strToInt :: String -> Integer
strToInt = foldl (\acc c -> toInteger (digitToInt c) + 10 * acc) 0

pStrInt :: Parser String
pStrInt = some (pSatisfiedChar isDigit)

pDouble :: Parser Double
pDouble = do
  skipWS
  x <- pStrInt
  skipChar '.'
  y <- pStrInt
  skipWS
  return $ toRealFloat $ scientific (strToInt (x ++ y)) (-length y)
