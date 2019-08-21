module HashingContainers.HashSet exposing
  (
    HashSet,
    empty,
    fromList,
    fromArray,
    insert,
    remove,
    member,
    isEmpty,
    size,
    foldl,
    toList
  )
{-|
HashSet API.

@docs HashSet

# Construction

All construction functions require you to provide
instances for
[`Equality`](/packages/nikita-volkov/typeclasses/1.5.0/Typeclasses-Classes-Equality)
and
[`Hashing`](/packages/nikita-volkov/typeclasses/1.5.0/Typeclasses-Classes-Hashing)
explicit typeclasses.

For general introduction to explicit typeclasses,
see [the readme of the "typeclasses" library](/packages/nikita-volkov/typeclasses/1.5.0).

@docs empty, fromList, fromArray

# Transformation

@docs insert, remove

# Access

@docs member, isEmpty, size, foldl, toList
-}

import Array exposing (Array)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import HashingContainers.HashTrie as HashTrie exposing (HashTrie)
import HashingContainers.Extensions.Maybe as Maybe


{-|
Generic set of hashable elements.

Unlike `Set` from the "core" library it is not
limited to any specific set of supported types for elements.
-}
type HashSet value =
  HashSet (Equality value) (Hashing value) (HashTrie value)

{-|
Construct an empty HashSet,
providing the required instances.
-}
empty : Equality value -> Hashing value -> HashSet value
empty equality hashing = HashSet equality hashing HashTrie.empty

fromFoldable :
  ((value -> HashTrie value -> HashTrie value) -> HashTrie value -> foldable -> HashTrie value) ->
  Equality value -> Hashing value ->
  foldable -> HashSet value
fromFoldable fold equality hashing foldable =
  HashSet equality hashing
    (HashTrie.fromFoldable fold hashing.hash equality.eq foldable)

{-|
Construct from a list of values,
providing the required instances.
-}
fromList : Equality value -> Hashing value -> List value -> HashSet value
fromList = fromFoldable List.foldl

{-|
Construct from an array of values,
providing the required instances.
-}
fromArray : Equality value -> Hashing value -> Array value -> HashSet value
fromArray = fromFoldable Array.foldl

accessHashTrieAtKey : (Int -> (value -> Bool) -> HashTrie value -> result) -> value -> HashSet value -> result
accessHashTrieAtKey fn value (HashSet equality hashing trie) =
  let
    hash = hashing.hash value
    eq = equality.eq
    in fn hash (eq value) trie

mapHashTrieAtKey : (Int -> (value -> Bool) -> HashTrie value -> HashTrie value) -> value -> HashSet value -> HashSet value
mapHashTrieAtKey fn value (HashSet equality hashing trie) =
  let
    hash = hashing.hash value
    eq = equality.eq
    newTrie = fn hash (eq value) trie
    in HashSet equality hashing newTrie

{-|
Insert a value into a set.
-}
insert : value -> HashSet value -> HashSet value
insert value = mapHashTrieAtKey (HashTrie.insert value) value

{-|
Remove a value from a set. If the value is not found, no changes are made.
-}
remove : value -> HashSet value -> HashSet value
remove = mapHashTrieAtKey HashTrie.remove

{-|
Determine if a value is in a set.
-}
member : value -> HashSet value -> Bool
member value = accessHashTrieAtKey HashTrie.get value >> Maybe.match False (always True)

{-|
Determine if a set is empty.
-}
isEmpty : HashSet value -> Bool
isEmpty (HashSet _ _ trie) = trie |> HashTrie.isEmpty

{-|
_O(n)_. Count the elements of the set.
Same as `foldl (\ _ x -> x + 1) 0`.
-}
size : HashSet value -> Int
size = foldl (\ _ x -> x + 1) 0

{-|
Fold over the values in a set.
-}
foldl : (value -> folding -> folding) -> folding -> HashSet value -> folding
foldl step folding (HashSet _ _ trie) = trie |> HashTrie.foldl step folding

{-|
Convert a set into a list.
-}
toList : HashSet value -> List value
toList (HashSet _ _ trie) = trie |> HashTrie.toList
