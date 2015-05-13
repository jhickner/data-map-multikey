# data-map-multikey
Data.Map, but indexed by multiple, *unique* keys. 

This small project came out of exploring the code for `IxSet`. It's essentially `IxSet` without the `Set`s, and hence without the ability to support duplicate keys. On the upside, it's [small](https://github.com/jhickner/data-map-multikey/blob/master/Data/Map/MultiKey.hs) and easy to understand.

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


