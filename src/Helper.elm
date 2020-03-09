module Helper exposing(..)


-- Retrieve the last element in a list
last : List a -> Maybe a
last list =
  case list of
    [] ->
      Nothing

    [x] ->
      Just x

    (x :: xs) ->
      last xs


-- Generate a suffix list for a given list
tails : List a -> List (a, List a)
tails xs =
  case xs of
    [] ->
      []

    (x::rest) ->
      (x, rest) :: tails rest