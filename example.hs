{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Map.MultiKey
import Data.Typeable
import Prelude hiding (lookup, null)

data Record = Record
  { rIntKey :: Int
  , rStringKey :: String
  , rData :: String
  } deriving (Show, Typeable)

instance MultiKeyable Record where 
  empty = MultiKey [key rIntKey, key rStringKey]

records :: [Record]
records =
  [ Record 1 "key 1" "data 1"
  , Record 20 "key 20" "data 20"
  , Record 3 "key 3" "data 3"
  ]

mk :: MultiKey Record
mk = fromList records
