# data-map-multikey
Data.Map with lookups by multiple, unique keys.
Heavily inspired by `IxSet`.


## Example

```haskell
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

{-
> lookup (1::Int) mk
Just (Record {rIntKey = 1, rStringKey = "key 1", rData = "data 1"})
it :: Maybe Record

> lookup "key 20" mk
Just (Record {rIntKey = 20, rStringKey = "key 20", rData = "data 20"})
it :: Maybe Record

> lookup 2.0 mk
*** Exception: MultiKey: there is no key of type Double in MultiKey Record
-}
```


