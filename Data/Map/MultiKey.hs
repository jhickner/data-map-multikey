{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}

{- |
Data.Map with multiple, unique keys.
IxSet without the Sets.

    > {-# LANGUAGE DeriveDataTypeable #-}
    >
    > module Main where
    > 
    > import Data.Map.MultiKey
    > import Data.Typeable
    > import Prelude hiding (lookup, null)
    > 
    > data Record = Record
    >   { rIntKey :: Int
    >   , rStringKey :: String
    >   , rData :: String
    >   } deriving (Show, Typeable)
    > 
    > instance MultiKeyable Record where 
    >   empty = MultiKey [key rIntKey, key rStringKey]
    > 
    > records :: [Record]
    > records =
    >   [ Record 1 "key 1" "data 1"
    >   , Record 20 "key 20" "data 20"
    >   , Record 3 "key 3" "data 3"
    >   ]
    > 
    > mk :: MultiKey Record
    > mk = fromList records

    > > lookup (1::Int) mk
    > Just (Record {rIntKey = 1, rStringKey = "key 1", rData = "data 1"})
    > it :: Maybe Record

    > > lookup "key 20" mk
    > Just (Record {rIntKey = 20, rStringKey = "key 20", rData = "data 20"})
    > it :: Maybe Record

    > > lookup 2.0 mk
    > *** Exception: MultiKey: there is no key of type Double in MultiKey Record
-}

module Data.Map.MultiKey 
  ( MultiKey(..)
  , MultiKeyable(..)
  , delete
  , deleteKey
  , fromList
  , insert
  , insertList
  , key
  , lookup
  , null
  , toList
  , updateKey
  ) where

import           Data.List             (foldl')
import           Data.Map              (Map)
import qualified Data.Map       as     M
import           Data.Typeable
import           Prelude        hiding (lookup, null)

data Key a = forall k . (Typeable k, Ord k) => 
    Key (Map k a) (a -> k) deriving Typeable

data MultiKey a = MultiKey [Key a] deriving Typeable

class MultiKeyable a where
    empty :: MultiKey a

key :: (Typeable k, Ord k) => (a -> k) -> Key a
key f = Key M.empty f

lookup :: (Typeable a, Typeable k) => k -> MultiKey a -> Maybe a
lookup k mk@(MultiKey keys) = lookup' keys
  where
    lookup' [] = error $ "MultiKey: there is no key of type " 
                 ++ (show $ typeOf k) ++ " in " ++ (show $ typeOf mk)
    lookup' (Key m _:ks) = maybe (lookup' ks) (`M.lookup` m) $ cast k

insertIntoKey :: a -> Key a -> Key a
insertIntoKey x (Key m getIdx) = Key (M.insert (getIdx x) x m) getIdx

insert :: a -> MultiKey a -> MultiKey a
insert x (MultiKey indexes) = MultiKey $ map (insertIntoKey x) indexes

insertList :: MultiKeyable a => [a] -> MultiKey a -> MultiKey a
insertList xs (MultiKey keys) = MultiKey $ map f keys
  where
    f key = foldl' (flip insertIntoKey) key xs

deleteFromKey :: a -> Key a -> Key a
deleteFromKey x (Key m getIdx) = Key (M.delete (getIdx x) m) getIdx

delete :: a -> MultiKey a -> MultiKey a
delete x (MultiKey indexes) = MultiKey $ map (deleteFromKey x) indexes

updateKey :: (Typeable a, Typeable k) => k -> a -> MultiKey a -> MultiKey a
updateKey k v mk = insert v $ maybe mk (flip delete mk) $ lookup k mk

deleteKey :: (Typeable a, Typeable k) => k -> MultiKey a -> MultiKey a
deleteKey k mim = maybe mim (flip delete mim) $ lookup k mim

null :: MultiKey a -> Bool
null (MultiKey (Key m _:_)) = M.null m
null (MultiKey []) = True

fromList :: MultiKeyable a => [a] -> MultiKey a
fromList = flip insertList empty 

toList :: MultiKey a -> [a]
toList (MultiKey []) = []
toList (MultiKey (Key m _:_)) = M.elems m
