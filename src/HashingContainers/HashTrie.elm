module HashingContainers.HashTrie exposing (..)

import Array exposing (Array)
import Typeclasses.Classes.Equality exposing (Equality)
import Typeclasses.Classes.Hashing exposing (Hashing)
import IntDict exposing (IntDict)
import Typeclasses.Extensions.List as List


type alias HashTrie entry =
  {
    equality : Equality entry,
    hashing : Hashing entry,
    data : IntDict (List entry)
  }

-- * Construction
-------------------------

empty : Equality entry -> Hashing entry -> HashTrie entry
empty equality hashing = HashTrie equality hashing IntDict.empty

fromList : Equality entry -> Hashing entry -> List entry -> HashTrie entry
fromList = Debug.todo ""

-- * Tranformation
-------------------------

insert : entry -> HashTrie entry -> HashTrie entry
insert entry hashTrie =
  let
    hash = hashTrie.hashing.hash entry
    update maybeList = case maybeList of
      Just list -> Just (entry :: list)
      Nothing -> Just (List.singleton entry)
    in { hashTrie | data = IntDict.update hash update hashTrie.data }

remove : entry -> HashTrie entry -> HashTrie entry
remove entry hashTrie =
  { hashTrie |
    data =
      let
        hash = hashTrie.hashing.hash entry
        update maybeList = case maybeList of
          Just list -> case List.remove hashTrie.equality entry list of
            [] -> Nothing
            newList -> Just newList
          Nothing -> Nothing
        in IntDict.update hash update hashTrie.data
  }
