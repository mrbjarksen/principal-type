module Language.LambdaCalculus (Var(..), Term(..), freeVars) where

import Data.Maybe (fromMaybe)
import Data.List (intersperse)
import Data.Set (Set)
import qualified Data.Set as S

newtype Var = Var String deriving (Ord, Eq)

instance Show Var where
  show (Var s) = s

instance Enum Var where
  toEnum n = Var $ let (q, r) = n `quotRem` 26 in toEnum (fromEnum 'a' + r) : replicate q '\''
  fromEnum (Var (c:p)) = let (q, r) = (length p, fromEnum c - fromEnum 'a') in q*26 + r

data Term = TermVar !Var
          | Abs !Var !Term
          | App !Term !Term
          deriving Eq

powerOf :: Term -> (Int, Term)
powerOf (App m n@(App m' _))
  | m == m' = let (p, t) = powerOf n in (p+1, t)
powerOf (App m n) = (1, n)

instance Show Term where
  show (TermVar x) = show x
  show (Abs (Var "x") (Abs (Var "y") (TermVar (Var "y")))) = "0\x0305"
  show (Abs (Var "x") (Abs (Var "y") m@(App (TermVar (Var "x")) _))) 
    | t == TermVar (Var "y") = intersperse '\x0305' (show p) ++ "\x0305"
    where (p, t) = powerOf m
  show (Abs x m@(Abs _ _)) = "(λ" ++ show x ++ "" ++ (drop 2 . show) m
  show (Abs x m) = "(λ" ++ show x ++ ". " ++ show m ++ ")"
  show (App m@(App _ _) n) = "(" ++ (tail . init . show) m ++ " " ++ show n ++ ")"
  show m'@(App m _) = "(" ++ show m ++ (if p > 1 then supernum p else "") ++ " " ++ show n ++ ")"
    where (p, n) = powerOf m'
          supernum = map super . show
          super '0' = '⁰'
          super '1' = '¹'
          super '2' = '²'
          super '3' = '³'
          super '4' = '⁴'
          super '5' = '⁵'
          super '6' = '⁶'
          super '7' = '⁷'
          super '8' = '⁸'
          super '9' = '⁹'

freeVars :: Term -> Set Var
freeVars (TermVar x) = S.singleton x
freeVars (Abs x m) = S.delete x (freeVars m)
freeVars (App m n) = S.union (freeVars m) (freeVars n)
