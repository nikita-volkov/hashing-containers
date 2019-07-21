module HashingContainers.Extensions.List exposing (..)

import List exposing (..)


maybe : Maybe a -> List a
maybe x = case x of
  Just y -> [y]
  _ -> []

mapHeadAndTail : (a -> b) -> (a -> b) -> List a -> List b
mapHeadAndTail headFn tailFn list = case list of
  head :: tail -> headFn head :: map tailFn tail
  _ -> []

mapTail : (a -> a) -> List a -> List a
mapTail mapping list = case list of
  head :: tail -> head :: map mapping tail
  _ -> []

mapInit : (a -> a) -> List a -> List a
mapInit mapping =
  let
    build list = case list of
      head :: tail -> buildTail head [] tail
      _ -> []
    buildTail previous newList list = case list of
      head :: tail -> buildTail head (mapping previous :: newList) tail
      _ -> previous :: newList
    in build >> reverse

find : (a -> Bool) -> List a -> Maybe a
find predicate =
  let
    loop list = case list of
      head :: tail -> if predicate head
        then Just head
        else loop tail
      [] -> Nothing
    in loop

{-| *O(n)*. Remove the first occurrence of matching element. -}
findAndRemove : (a -> Bool) -> List a -> List a
findAndRemove predicate list =
  let
    loop precedingList currentList = case currentList of
      head :: tail -> if predicate head
        then prependReversed precedingList tail
        else loop (head :: precedingList) tail
      _ -> list
    in loop [] list

{-| *O(n)*. Modify the first occurrence of matching element. -}
findAndModify : (a -> Bool) -> (a -> a) -> List a -> List a
findAndModify predicate modify list =
  let
    loop precedingList currentList = case currentList of
      head :: tail -> if predicate head
        then prependReversed precedingList (modify head :: tail)
        else loop (head :: precedingList) tail
      _ -> list
    in loop [] list

{-| *O(n)*. Update the first occurrence of matching element. -}
findAndUpdate : (a -> Bool) -> (a -> Maybe a) -> List a -> List a
findAndUpdate predicate update list =
  let
    loop precedingList currentList = case currentList of
      head :: tail -> if predicate head
        then case update head of
          Just newHead -> prependReversed precedingList (newHead :: tail)
          Nothing -> prependReversed precedingList tail
        else loop (head :: precedingList) tail
      _ -> list
    in loop [] list

prependReversed : List a -> List a -> List a
prependReversed left = case left of
  head :: tail -> prependReversed tail << (::) head
  _ -> identity
