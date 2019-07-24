module HashingContainers.HashSet exposing (..)

import Array exposing (Array)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import HashingContainers.Data as Data exposing (Data)
import HashingContainers.Extensions.Maybe as Maybe


type alias HashSet value =
  {
    equality : Equality value,
    hashing : Hashing value,
    data : Data value
  }

empty : Equality value -> Hashing value -> HashSet value
empty equality hashing = HashSet equality hashing Data.empty

fromFoldable :
  ((value -> Data value -> Data value) -> Data value -> foldable -> Data value) ->
  Equality value -> Hashing value ->
  foldable -> HashSet value
fromFoldable fold equality hashing foldable =
  HashSet equality hashing
    (Data.fromFoldable fold hashing.hash equality.eq foldable)

fromList : Equality value -> Hashing value -> List value -> HashSet value
fromList = fromFoldable List.foldl

fromArray : Equality value -> Hashing value -> Array value -> HashSet value
fromArray = fromFoldable Array.foldl

accessDataAtKey : (Int -> (value -> Bool) -> Data value -> result) -> value -> HashSet value -> result
accessDataAtKey fn value hashSet =
  let
    hash = hashSet.hashing.hash value
    eq = hashSet.equality.eq
    in fn hash (eq value) hashSet.data

mapDataAtKey : (Int -> (value -> Bool) -> Data value -> Data value) -> value -> HashSet value -> HashSet value
mapDataAtKey fn value hashSet =
  { hashSet | data = accessDataAtKey fn value hashSet }

insert : value -> HashSet value -> HashSet value
insert value = mapDataAtKey (Data.insert value) value

remove : value -> HashSet value -> HashSet value
remove = mapDataAtKey Data.remove

member : value -> HashSet value -> Bool
member value = accessDataAtKey Data.lookup value >> Maybe.match False (always True)

isEmpty : HashSet value -> Bool
isEmpty = .data >> Data.isEmpty

foldl : (value -> folding -> folding) -> folding -> HashSet value -> folding
foldl step folding = .data >> Data.foldl step folding

toList : HashSet value -> List value
toList = .data >> Data.toList
