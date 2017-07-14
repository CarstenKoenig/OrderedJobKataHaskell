module OrderJobs
  ( Job
  , Dependency
  , sort
  , dependsOn
  , independend
  , (.=>)
  ) where


import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Except as Ex
import qualified Control.Monad.Trans.State as St

import           Data.Char

import           Data.Set (Set)
import qualified Data.Set as Set


sort :: [Dependency] -> Either String [Job]
sort deps = St.evalState (Ex.runExceptT (sort' deps)) Set.empty


----------------------------------------------------------------------


type Env a = Ex.ExceptT String (St.State State) a

type State = Set String


sort' :: [Dependency] -> Env [Job]
sort' [] = return []
sort' deps@(Dependency job Nothing : rest) =
  prependDeps deps Set.empty job $ sort' rest
sort' deps@(Dependency job (Just job') : rest)
  | job == job' =
    Ex.throwE (job ++ " depends on itself")
  | otherwise =
    prependDeps deps Set.empty job' $ prependNew job $ sort' rest


prependDeps :: [Dependency] -> Set Job -> Job -> Env [Job] -> Env [Job]
prependDeps deps visited job cont
  | Set.notMember job visited =
    case findDep deps job of
      Nothing ->
        prependNew job cont
      Just depJob ->
        prependDeps deps (Set.insert job visited)  depJob (prependNew job cont)
  | otherwise =
    Ex.throwE "cycle found"


prependNew :: Job -> Env [Job] -> Env [Job]
prependNew job cont = do
  alreadyTaken <- lift $ St.gets (Set.member job)
  if alreadyTaken
    then cont
    else do
    lift $ St.modify (Set.insert job)
    (job :) <$> cont
  

findDep :: [Dependency] -> Job -> Maybe Job
findDep [] _ = Nothing
findDep (Dependency depJob depOn : ds) job
  | depJob == job = depOn
  | otherwise = findDep ds job


----------------------------------------------------------------------

independend :: Job -> Dependency
independend job = Dependency job Nothing

dependsOn :: Job -> Job -> Dependency
dependsOn a b = Dependency a (Just b)


(.=>) :: Job -> Job -> Dependency
(.=>) = dependsOn
infixl 9 .=>

  
data Dependency =
  Dependency
  { dependendJob :: Job
  , dependsOnJob :: Maybe Job
  }


type Job = String


