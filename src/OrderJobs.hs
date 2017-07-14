module OrderJobs
  ( Job
  , Dependency
  , sort
  , dependsOn
  , independend
  , (.=>)
  ) where

import Data.Char


sort :: [Dependency] -> [Job]
sort [] = []
sort (Dependency job Nothing : rest) = job : sort rest
sort (Dependency job (Just job') : rest) = job' : job : sort rest


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


