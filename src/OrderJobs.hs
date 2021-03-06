module OrderJobs
  ( Job
  , Dependency
  , sort
  , dependsOn
  , independend
  , (.=>)
  ) where


import           Control.Monad ((>=>))

import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Except as Ex
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State as St

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Data.Maybe (mapMaybe)

import           Data.Set (Set)
import qualified Data.Set as Set


type Job = String


data Dependency =
  Dependency
  { dependendJob :: Job
  , dependsOnJob :: Maybe Job
  }


independend :: Job -> Dependency
independend job = Dependency job Nothing


dependsOn :: Job -> Job -> Dependency
dependsOn a b = Dependency a (Just b)


(.=>) :: Job -> Job -> Dependency
(.=>) = dependsOn
infixl 9 .=>

  
sort :: [Dependency] -> Either String [Job]
sort deps =
  flip St.evalState Set.empty .
  flip R.runReaderT (associations deps) .
  Ex.runExceptT $
  sort'


----------------------------------------------------------------------


type Env a = Ex.ExceptT String (R.ReaderT Deps (St.State State)) a

type Deps  = ([Job], Map Job Job)
type State = Set String


sort' :: Env [Job]
sort' = jobs >>= foldr (prependDeps Set.empty) (return [])


prependDeps :: Set Job -> Job -> Env [Job] -> Env [Job]
prependDeps visited job cont
  | Set.notMember job visited = do
    dependsOn <- findDep job
    case dependsOn of
      Nothing ->
        prependNew job cont
      Just depJob
        | job == depJob ->
          Ex.throwE (job ++ " depends on itself")
        | otherwise ->
          prependDeps (Set.insert job visited) depJob (prependNew job cont)
  | otherwise =
    Ex.throwE "cycle found"


prependNew :: Job -> Env [Job] -> Env [Job]
prependNew job cont = do
  alreadyTaken <- lift . lift $ St.gets (Set.member job)
  if alreadyTaken
    then cont
    else do
    lift . lift $ St.modify (Set.insert job)
    (job :) <$> cont
  

findDep :: Job -> Env (Maybe Job)
findDep job = lift $ R.asks (Map.lookup job . snd)


jobs :: Env [Job]
jobs = lift $ R.asks fst


associations :: [Dependency] -> Deps
associations deps = (js, Map.fromList assocs)
  where
    js = map dependendJob  deps
    assocs = mapMaybe assoc deps
    assoc (Dependency _ Nothing) = Nothing
    assoc (Dependency job (Just on)) = Just (job, on)

