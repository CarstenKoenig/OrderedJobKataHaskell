module OrderJobs
  ( Job
  , Dependency
  , sort
  , dependsOn
  , independend
  , (.=>)
  ) where


import qualified Control.Monad.Trans.State as St

import           Data.Char

import           Data.Set (Set)
import qualified Data.Set as Set


sort :: [Dependency] -> [Job]
sort deps = St.evalState (sort' deps) Set.empty


----------------------------------------------------------------------


type Env a = St.State State a

type State = Set String


sort' :: [Dependency] -> Env [Job]
sort' [] = return []
sort' deps@(Dependency job Nothing : rest) =
  prependDeps deps job $ sort' rest
sort' deps@(Dependency job (Just job') : rest) =
  prependDeps deps job' $ prependNew job $ sort' rest


prependDeps :: [Dependency] -> Job -> Env [Job] -> Env [Job]
prependDeps deps job cont =
  case findDep deps job of
    Nothing -> prependNew job cont
    Just depJob -> prependDeps deps depJob (prependNew job cont)


prependNew :: Job -> Env [Job] -> Env [Job]
prependNew job cont = do
  alreadyTaken <- St.gets (Set.member job)
  if alreadyTaken
    then cont
    else do
    St.modify (Set.insert job)
    (job :) <$> cont
  

findDep :: [Dependency] -> Job -> Maybe Job
findDep [] _ = Nothing
findDep ((Dependency depJob depOn) : ds) job
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


