module Util exposing
    ( pipeMaybe
    , applyHandler
    , applyHandler2
    , applyHandler3
    , last
    )

import List

pipeMaybe : (m, Cmd a) -> Maybe a' -> (a' -> m -> (m, Cmd a)) -> (m, Cmd a)
pipeMaybe (model, fx) act f =
    case act of
        Just a ->
            let
                (model', fx') = f a model
            in
                (model', Cmd.batch [fx', fx])
        Nothing ->
            (model, fx)

applyHandler : Maybe (a -> b) -> a -> Maybe b
applyHandler f x =
    case f of
        Just f' ->
            Just (f' x)
        Nothing ->
            Nothing

applyHandler2 : Maybe (a -> b -> c) -> a -> b -> Maybe c
applyHandler2 f x y =
    case f of
        Just f' ->
            Just (f' x y)
        Nothing ->
            Nothing

applyHandler3 : Maybe (a -> b -> c -> d) -> a -> b -> c -> Maybe d
applyHandler3 f x y z =
    case f of
        Just f' ->
            Just (f' x y z)
        Nothing ->
            Nothing

last : List a -> Maybe a
last = List.head << List.reverse
