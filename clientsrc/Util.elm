module Util exposing
    ( pipeMaybe
    , applyHandler
    , applyHandler2
    )

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
