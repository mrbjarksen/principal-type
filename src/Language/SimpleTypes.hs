module Language.SimpleTypes (Var(..), Type(..), hasVar, typeVars, newVar) where

import Data.Set (Set)
import qualified Data.Set as S

newtype Var = Var String deriving (Ord, Eq)

instance Show Var where
  show (Var s) = s

instance Enum Var where
  toEnum n = Var $ let (q, r) = n `quotRem` 26 in toEnum (fromEnum 'a' + r) : replicate q '\''
  fromEnum (Var (c:p)) = let (q, r) = (length p, fromEnum c - fromEnum 'a') in q*26 + r

data Type = TypeVar !Var
          | Fun !Type !Type
          deriving Eq

instance Show Type where
  show (TypeVar a) = show a
  show (Fun sigma tau@(Fun _ _)) = "(" ++ show sigma ++ " → " ++ (tail . init . show) tau ++ ")"
  show (Fun sigma tau) = "(" ++ show sigma ++ " → " ++ show tau ++ ")"

hasVar :: Var -> Type -> Bool
hasVar a (TypeVar a') = a == a'
hasVar a (Fun sigma tau) = hasVar a sigma || hasVar a tau

typeVars :: [Type] -> Set Var
typeVars [] = S.empty
typeVars (TypeVar a : sigma) = S.insert a $ typeVars sigma
typeVars (Fun alpha beta : sigma) = typeVars $ alpha : beta : sigma

newVar :: [Type] -> Var
newVar = toEnum . succ . maximum . (0:) . map maxi 
  where maxi (TypeVar a) = fromEnum a
        maxi (Fun sigma tau) = max (maxi sigma) (maxi tau)

