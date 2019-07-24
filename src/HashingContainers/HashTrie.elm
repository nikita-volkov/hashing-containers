module HashingContainers.HashTrie exposing (..)

import Array exposing (Array)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import HashingContainers.HashTrie.Data as Data exposing (Data)


type alias HashTrie key entry =
  {
    equality : Equality key,
    hashing : Hashing key,
    entryToKey : entry -> key,
    data : Data entry
  }

-- * Construction
-------------------------

empty : Equality key -> Hashing key -> (entry -> key) -> HashTrie key entry
empty equality hashing entryToKey = HashTrie equality hashing entryToKey Data.empty

fromFoldable :
  ((entry -> Data entry -> Data entry) -> Data entry -> foldable -> Data entry) ->
  Equality key -> Hashing key -> (entry -> key) ->
  foldable -> HashTrie key entry
fromFoldable fold equality hashing entryToKey foldable =
  HashTrie equality hashing entryToKey
    (Data.fromFoldable
      fold
      (Hashing.map entryToKey hashing).hash
      (Equality.map entryToKey equality).eq
      foldable)

fromList : Equality key -> Hashing key -> (entry -> key) -> List entry -> HashTrie key entry
fromList = fromFoldable List.foldl

fromArray : Equality key -> Hashing key -> (entry -> key) -> Array entry -> HashTrie key entry
fromArray = fromFoldable Array.foldl

-- * Tranformation
-------------------------

insert : entry -> HashTrie key entry -> HashTrie key entry
insert entry hashTrie =
  { hashTrie |
    data =
      let
        key = hashTrie.entryToKey entry
        hash = hashTrie.hashing.hash key
        eq = hashTrie.equality.eq
        entryToKey = hashTrie.entryToKey
        in Data.insert entry hash (eq key << entryToKey) hashTrie.data
  }

remove : key -> HashTrie key entry -> HashTrie key entry
remove key hashTrie =
  { hashTrie |
    data =
      let
        hash = hashTrie.hashing.hash key
        eq = hashTrie.equality.eq
        entryToKey = hashTrie.entryToKey
        in Data.remove hash (eq key << entryToKey) hashTrie.data
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
        in Data.update updateFn hash (eq key << entryToKey) hashTrie.data
  }

-- * Access
-------------------------

lookup : key -> HashTrie key entry -> Maybe entry
lookup key hashTrie =
  let
    hash = hashTrie.hashing.hash key
    eq = hashTrie.equality.eq
    entryToKey = hashTrie.entryToKey
    in Data.lookup hash (eq key << entryToKey) hashTrie.data

isEmpty : HashTrie key entry -> Bool
isEmpty = .data >> Data.isEmpty

foldl : (entry -> folding -> folding) -> folding -> HashTrie key entry -> folding
foldl step folding = .data >> Data.foldl step folding

toList : HashTrie key entry -> List entry
toList = .data >> Data.toList
