module HashingContainers.HashDict exposing (..)

import Array exposing (Array)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import HashingContainers.Data as Data exposing (Data)


type alias HashDict key value =
  {
    equality : Equality key,
    hashing : Hashing key,
    data : Data (key, value)
  }

empty : Equality key -> Hashing key -> HashDict key value
empty equality hashing = HashDict equality hashing Data.empty

fromFoldable :
  (((key, value) -> Data (key, value) -> Data (key, value)) -> Data (key, value) -> foldable -> Data (key, value)) ->
  Equality key -> Hashing key ->
  foldable -> HashDict key value
fromFoldable fold equality hashing foldable =
  HashDict equality hashing
    (Data.fromFoldable
      fold
      (Hashing.map Tuple.first hashing).hash
      (Equality.map Tuple.first equality).eq
      foldable)

fromList : Equality key -> Hashing key -> List (key, value) -> HashDict key value
fromList = fromFoldable List.foldl

fromArray : Equality key -> Hashing key -> Array (key, value) -> HashDict key value
fromArray = fromFoldable Array.foldl

accessDataAtKey : (Int -> ((key, value) -> Bool) -> Data (key, value) -> result) -> key -> HashDict key value -> result
accessDataAtKey fn key hashDict =
  let
    hash = hashDict.hashing.hash key
    eq = hashDict.equality.eq
    in fn hash (Tuple.first >> eq key) hashDict.data

mapDataAtKey : (Int -> ((key, value) -> Bool) -> Data (key, value) -> Data (key, value)) -> key -> HashDict key value -> HashDict key value
mapDataAtKey fn key hashDict =
  { hashDict | data = accessDataAtKey fn key hashDict }

insert : key -> value -> HashDict key value -> HashDict key value
insert key value = mapDataAtKey (Data.insert (key, value)) key

remove : key -> HashDict key value -> HashDict key value
remove = mapDataAtKey Data.remove

update : key -> (Maybe value -> Maybe value) -> HashDict key value -> HashDict key value
update key updateFn = mapDataAtKey (Data.update (Maybe.map Tuple.second >> updateFn >> Maybe.map (Tuple.pair key))) key

lookup : key -> HashDict key value -> Maybe value
lookup key = accessDataAtKey Data.lookup key >> Maybe.map Tuple.second

isEmpty : HashDict key value -> Bool
isEmpty = .data >> Data.isEmpty

foldl : ((key, value) -> folding -> folding) -> folding -> HashDict key value -> folding
foldl step folding = .data >> Data.foldl step folding

toList : HashDict key value -> List (key, value)
toList = .data >> Data.toList
