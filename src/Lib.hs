module Lib where

data Client
  = GovOrg String
  | Company String Integer String String
  | Individual Person Bool
  deriving (Show)

data Gender = Male | Female | Unknown
  deriving (Show)

data Person = Person String String Gender
  deriving (Show)

data Manufacturer = Manufacturer String
  deriving (Show)

data Features = Features Bool
  deriving (Show)

data TimeMachine = TimeMachine Integer String Float Manufacturer Features
  deriving (Show)

clientName :: Client -> String
clientName client = case client of
  GovOrg name -> name
  Company name _ _ _ -> name
  Individual person _ ->
    case person of
      Person firstName lastName _ -> firstName ++ " " ++ lastName

data GenderCount = GenderCount Int Int Int

countGenders :: [Client] -> GenderCount -> GenderCount
countGenders [] stats = stats
countGenders ((Individual (Person _ _ Male) _) : xs) (GenderCount m f u) = countGenders xs (GenderCount (m + 1) f u)
countGenders ((Individual (Person _ _ Female) _) : xs) (GenderCount m f u) = countGenders xs (GenderCount m (f + 1) u)
countGenders ((Individual (Person _ _ Unknown) _) : xs) (GenderCount m f u) = countGenders xs (GenderCount m f (u + 1))
countGenders (_ : xs) stats = countGenders xs stats
