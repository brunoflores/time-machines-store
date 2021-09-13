{-# LANGUAGE ViewPatterns #-}

module Lib where

data Client
  = GovOrg {name :: String}
  | Company
      { name :: String,
        companyId :: Integer,
        person :: Person,
        duty :: String
      }
  | Individual
      { person :: Person,
        ads :: Bool
      }
  deriving (Show)

data Gender = Male | Female | Unknown
  deriving (Show)

data Person = Person
  { firstName :: String,
    lastName :: String,
    gender :: Gender
  }
  deriving (Show)

data Manufacturer = Manufacturer String
  deriving (Show)

data Features = Features Bool
  deriving (Show)

data TimeMachine = TimeMachine Integer String Float Manufacturer Features
  deriving (Show)

clientName :: Client -> String
clientName client = case client of
  GovOrg {name = n} -> n
  Company {name = n} -> n
  Individual {person = p} ->
    case p of
      Person {firstName = f, lastName = l} -> f ++ " " ++ l

data GenderStats = GenderStats Int Int Int

listGenders :: [Client] -> [Gender]
listGenders [] = []
listGenders ((Individual (Person _ _ Male) _) : xs) = [Male] ++ listGenders xs
listGenders ((Individual (Person _ _ Female) _) : xs) = [Female] ++ listGenders xs
listGenders ((Individual (Person _ _ Unknown) _) : xs) = [Unknown] ++ listGenders xs
listGenders (_ : xs) = listGenders xs

countGenders :: [Gender] -> GenderStats
countGenders = foldr calc (GenderStats 0 0 0)
  where
    calc Male (GenderStats m f u) = GenderStats (m + 1) f u
    calc Female (GenderStats m f u) = GenderStats m (f + 1) u
    calc Unknown (GenderStats m f u) = GenderStats m f (u + 1)

calcGenders :: [Client] -> GenderStats
calcGenders = countGenders . listGenders

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _ = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director") = True
specialClient _ = False
