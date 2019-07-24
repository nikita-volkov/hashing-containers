module HashingContainers.Extensions.Maybe exposing (..)

import Maybe exposing (..)


match : b -> (a -> b) -> Maybe a -> b
match b aToB x = case x of
  Just a -> aToB a
  _ -> b
