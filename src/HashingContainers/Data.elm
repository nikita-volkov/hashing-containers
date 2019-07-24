module HashingContainers.Data exposing (..)

import IntDict exposing (IntDict)
import HashingContainers.Extensions.List as List


type alias Data entry = IntDict (List entry)

-- * Construction
-------------------------

empty : Data entry
empty = IntDict.empty

fromFoldable :
  ((entry -> Data entry -> Data entry) -> Data entry -> foldable -> Data entry) ->
  (entry -> Int) ->
  (entry -> entry -> Bool) ->
  foldable ->
  Data entry
fromFoldable foldFn entryHash entryEq =
  foldFn (\ entry -> insert entry (entryHash entry) (entryEq entry))
    empty

-- * Tranformation
-------------------------

insert : entry -> Int -> (entry -> Bool) -> Data entry -> Data entry
insert entry hash entryPredicate =
  let
    intDictUpdate listMaybe = case listMaybe of
      Just list ->
        let
          updateFn _ = Just entry
          in List.findAndUpdate entryPredicate updateFn list |> Just
      Nothing -> Just (List.singleton entry)
    in IntDict.update hash intDictUpdate

remove : Int -> (entry -> Bool) -> Data entry -> Data entry
remove hash entryPredicate =
  let
    intDictUpdate listMaybe = case listMaybe of
      Just list -> case List.findAndRemove entryPredicate list of
        [] -> Nothing
        newList -> Just newList
      Nothing -> Nothing
    in IntDict.update hash intDictUpdate

{-|
Caution! This function is not safe.
Your update to the entry must not affect its key projection.
-}
update : (Maybe entry -> Maybe entry) -> Int -> (entry -> Bool) -> Data entry -> Data entry
update updateFn hash entryPredicate =
  let
    intDictUpdate listMaybe = case listMaybe of
      Just list -> case List.findAndUpdate entryPredicate updateFn list of
        [] -> Nothing
        newList -> Just newList
      Nothing -> case updateFn Nothing of
        Just newEntry -> Just (List.singleton newEntry)
        Nothing -> Nothing
    in IntDict.update hash intDictUpdate

-- * Access
-------------------------

lookup : Int -> (entry -> Bool) -> Data entry -> Maybe entry
lookup hash entryPredicate data =
  case IntDict.get hash data of
    Just entryList -> List.find entryPredicate entryList
    Nothing -> Nothing

isEmpty : Data entry -> Bool
isEmpty = IntDict.isEmpty

foldl : (entry -> folding -> folding) -> folding -> Data entry -> folding
foldl step = IntDict.foldl (\ _ entryList innerFolding -> List.foldl step innerFolding entryList)

toList : Data entry -> List entry
toList =
  let
    step hash entryList list = entryList ++ list
    in IntDict.foldl step []
