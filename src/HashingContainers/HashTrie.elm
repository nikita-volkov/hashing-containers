module HashingContainers.HashTrie exposing (..)

import Array exposing (Array)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import IntDict exposing (IntDict)
import Typeclasses.Extensions.List as List
import HashingContainers.Extensions.List as List


type alias HashTrie key entry =
  {
    equality : Equality key,
    hashing : Hashing key,
    entryToKey : entry -> key,
    data : IntDict (List entry)
  }

-- * Construction
-------------------------

empty : Equality key -> Hashing key -> (entry -> key) -> HashTrie key entry
empty equality hashing entryToKey = HashTrie equality hashing entryToKey IntDict.empty

fromList : Equality key -> Hashing key -> (entry -> key) -> List entry -> HashTrie key entry
fromList equality hashing entryToKey = List.foldl insert (empty equality hashing entryToKey)

-- * Tranformation
-------------------------

insert : entry -> HashTrie key entry -> HashTrie key entry
insert entry hashTrie =
  let
    key = hashTrie.entryToKey entry
    hash = hashTrie.hashing.hash key
    updateIntDict maybeList = case maybeList of
      Just list -> Just (entry :: list)
      Nothing -> Just (List.singleton entry)
    in { hashTrie | data = IntDict.update hash updateIntDict hashTrie.data }

remove : key -> HashTrie key entry -> HashTrie key entry
remove key hashTrie =
  { hashTrie |
    data =
      let
        hash = hashTrie.hashing.hash key
        eq = hashTrie.equality.eq
        entryToKey = hashTrie.entryToKey
        updateIntDict maybeList = case maybeList of
          Just list -> case List.findAndRemove (eq key << entryToKey) list of
            [] -> Nothing
            newList -> Just newList
          Nothing -> Nothing
        in IntDict.update hash updateIntDict hashTrie.data
  }

{-|
Caution! This function is not safe.
Your update to the entry must not affect its key projection.
-}
update : key -> (Maybe entry -> Maybe entry) -> HashTrie key entry -> HashTrie key entry
update key updateFn hashTrie =
  { hashTrie |
    data =
      let
        hash = hashTrie.hashing.hash key
        eq = hashTrie.equality.eq
        entryToKey = hashTrie.entryToKey
        updateIntDict maybeList = case maybeList of
          Just list -> case List.findAndUpdate (eq key << entryToKey) (updateFn << Just) list of
            [] -> Nothing
            newList -> Just newList
          Nothing -> case updateFn Nothing of
            Just newEntry -> Just (List.singleton newEntry)
            Nothing -> Nothing
        in IntDict.update hash updateIntDict hashTrie.data
  }

-- * Access
-------------------------

lookup : key -> HashTrie key entry -> Maybe entry
lookup key hashTrie =
  let
    hash = hashTrie.hashing.hash key
    eq = hashTrie.equality.eq
    entryToKey = hashTrie.entryToKey
    in case IntDict.get hash hashTrie.data of
      Just entryList -> List.find (eq key << entryToKey) entryList
      Nothing -> Nothing

isEmpty : HashTrie key entry -> Bool
isEmpty = .data >> IntDict.isEmpty
