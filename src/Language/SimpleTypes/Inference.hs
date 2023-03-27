module Language.SimpleTypes.Inference (Context, principalPair, Judgement, principalJudgement) where

import Data.List (intercalate)
import Data.Function (on)

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Language.LambdaCalculus (Term(..), freeVars)
import qualified Language.LambdaCalculus as LC (Var(..))
import Language.SimpleTypes (Var(..), Type(..), newVar, typeVars)
import Language.SimpleTypes.Substitution (substitute, renaming, renaming', unifier)

type Context = Map LC.Var Type

principalPair :: Term -> Maybe (Context, Type)
principalPair (TermVar x) = Just $ let a = TypeVar (toEnum 0) in (M.singleton x a, a)
principalPair (Abs x n) = principalPair n >>= newPair
  where newPair :: (Context, Type) -> Maybe (Context, Type)
        newPair (gamma, tau') =
          case (S.member x $ freeVars n, M.lookup x gamma) of
               (True, Just sigma) -> Just (M.delete x gamma, Fun sigma tau')
               (False, _)         -> Just (gamma, Fun (TypeVar . newVar $ tau' : M.elems gamma) tau')
               _                  -> Nothing
principalPair (App p q) = mapM principalPair [p, q] >>= newPair
  where newPair :: [(Context, Type)] -> Maybe (Context, Type)
        newPair [(gamma1, tau1), (gamma2', tau2')] = do
          let r = renaming' (newVar $ tau1 : M.elems gamma1) $ tau2' : M.elems gamma2'
              gamma2 = M.map (substitute r) gamma2'
              tau2 = substitute r tau2'
              x = S.toList $ (S.intersection `on` freeVars) p q
              a = newVar $ tau1 : tau2 : M.elems gamma1 ++ M.elems gamma2
          alpha <- mapM (flip M.lookup gamma1) x
          beta  <- mapM (flip M.lookup gamma2) x
          s'' <- unifier (tau1:alpha) (Fun tau2 (TypeVar a) : beta)
          let addVar s b = M.insert b (M.findWithDefault (TypeVar b) b s'') s
              vars = typeVars (tau1 : tau2 : (TypeVar a) : alpha ++ beta)
              s' = S.foldl' addVar M.empty vars
              r'' = renaming' (newVar $ tau1 : tau2 : M.elems gamma1 ++ M.elems gamma2) $ M.elems s'
              r' = S.foldr' M.delete r'' vars
              s = M.map (substitute r') s'
          return ((M.union `on` M.map (substitute s)) gamma1 gamma2, substitute s (TypeVar a))

newtype Judgement = Judgement (Context, Term, Type)

instance Show Judgement where
  show (Judgement (gamma, m, tau)) =
    intercalate ", " (map assignment $ M.toList gamma)
    ++ (if M.null gamma then "" else " ")
    ++ "⊢ " ++ show m ++ ":" ++ show tau
    where assignment (x, sigma) = show x ++ ":" ++ show sigma

principalJudgement :: Term -> Maybe Judgement
principalJudgement m = mkJudgement <$> principalPair m
  where mkJudgement (gamma, tau) =
          let r = renaming $ tau : M.elems gamma
              in Judgement (M.map (substitute r) gamma, m, substitute r tau)

