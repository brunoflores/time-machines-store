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
