module Util where

import Effects exposing (Effects)

pipeMaybe : (m, Effects a) -> Maybe a' -> (a' -> m -> (m, Effects a)) -> (m, Effects a)
pipeMaybe (model, fx) act f =
    case act of
        Just a ->
            let
                (model', fx') = f a model
            in
                (model', Effects.batch [fx', fx])
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
