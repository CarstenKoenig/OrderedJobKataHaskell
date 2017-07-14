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
sort' (Dependency job Nothing : rest) =
  prependNew job $ sort' rest
sort' (Dependency job (Just job') : rest) =
  prependNew job' $ prependNew job $ sort' rest


prependNew :: Job -> Env [Job] -> Env [Job]
prependNew job cont = do
  alreadyTaken <- St.gets (Set.member job)
  if alreadyTaken
    then cont
    else do
    St.modify (Set.insert job)
    (job :) <$> cont
  


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


