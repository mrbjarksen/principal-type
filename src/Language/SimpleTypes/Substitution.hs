module Language.SimpleTypes.Substitution (Substitution, substitute, unifier, renaming, renaming') where

import Data.List (findIndex)
import Data.Map (Map)
import qualified Data.Map as M

import Control.Monad.State

import Language.SimpleTypes (Var, Type(..), hasVar)

type Substitution = Map Var Type

substitute :: Substitution -> Type -> Type
substitute s a'@(TypeVar a) = M.findWithDefault a' a s
substitute s (Fun sigma tau) = Fun (substitute s sigma) (substitute s tau)

unifier :: [Type] -> [Type] -> Maybe Substitution
unifier alpha beta 
  | length alpha /= length beta = Nothing
  | otherwise = execStateT completeUnifier M.empty
  where completeUnifier :: StateT Substitution Maybe ()
        completeUnifier = get >>= \s -> do
          let alphasub = map (substitute s) alpha
              betasub  = map (substitute s) beta
          case findIndex not $ zipWith (==) alphasub betasub of
             Nothing -> return ()
             Just i  -> addSub (nextSub (alphasub !! i) (betasub !! i)) >> completeUnifier
        addSub :: (Var, Type) -> StateT Substitution Maybe ()
        addSub (a, sigma) = do
          guard . not $ hasVar a sigma
          modify' $ M.insert a sigma . M.map (substitute $ M.singleton a sigma)
        nextSub :: Type -> Type -> (Var, Type)
        nextSub (TypeVar a) gamma2 = (a, gamma2)
        nextSub gamma1 (TypeVar a) = (a, gamma1)
        nextSub (Fun tau1 tau1') (Fun tau2 tau2')
          | tau1  /= tau2  = nextSub tau1  tau2
          | tau1' /= tau2' = nextSub tau1' tau2'

renaming' :: Var -> [Type] -> Substitution
renaming' start sigma = evalState (foldM setVar M.empty sigma) start
  where setVar :: Substitution -> Type -> State Var Substitution
        setVar s (TypeVar a) =
          if M.notMember a s
             then get >>= \a' -> put (succ a') >> return (M.insert a (TypeVar a') s)
             else return s
        setVar s (Fun alpha beta) = foldM setVar s [alpha, beta]

renaming :: [Type] -> Substitution
renaming = renaming' $ toEnum 0

