module HashingContainers.HashSet exposing (..)

import Array exposing (Array)
import Typeclasses.Classes.Equality exposing (Equality)
import Typeclasses.Classes.Hashing exposing (Hashing)
import IntDict exposing (IntDict)
import Typeclasses.Extensions.List as List


type alias HashSet entry =
  {
    equality : Equality entry,
    hashing : Hashing entry,
    data : IntDict (List entry)
  }

-- * Construction
-------------------------

empty : Equality entry -> Hashing entry -> HashSet entry
empty equality hashing = HashSet equality hashing IntDict.empty

fromList : Equality entry -> Hashing entry -> List entry -> HashSet entry
fromList = Debug.todo ""

-- * Tranformation
-------------------------

insert : entry -> HashSet entry -> HashSet entry
insert entry hashSet =
  let
    hash = hashSet.hashing.hash entry
    update maybeList = case maybeList of
      Just list -> Just (entry :: list)
      Nothing -> Just (List.singleton entry)
    in { hashSet | data = IntDict.update hash update hashSet.data }

remove : entry -> HashSet entry -> HashSet entry
remove entry hashSet =
  { hashSet |
    data =
      let
        hash = hashSet.hashing.hash entry
        update maybeList = case maybeList of
          Just list -> case List.remove hashSet.equality entry list of
            [] -> Nothing
            newList -> Just newList
          Nothing -> Nothing
        in IntDict.update hash update hashSet.data
  }
