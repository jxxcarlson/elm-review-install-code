module Foo exposing (..)

f : Maybe Int -> Int
f mi =
    case mi of
        Just i -> i
        Nothing -> 0