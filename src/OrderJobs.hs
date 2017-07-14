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
sort [Dependency job Nothing] = [job]


----------------------------------------------------------------------

independend :: Job -> Dependency
independend job = Dependency job Nothing

dependsOn :: Job -> Maybe Job -> Dependency
dependsOn = Dependency


(.=>) :: Job -> Maybe Job -> Dependency
(.=>) = dependsOn
infixl 9 .=>

  
data Dependency =
  Dependency
  { dependendJob :: Job
  , dependsOnJob :: Maybe Job
  }


type Job = String


