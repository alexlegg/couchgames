module CouchGames.Game
    ( GameAPI(..)
    ) where

import CouchGames.Player

data GameAPI m conf gam act pub hid = GameAPI {
      initGame :: conf -> [Player] -> m (Either String gam)
    , storeGame :: gam -> m ()
    , gameState :: gam -> m pub
    , playerState :: gam -> m hid
    , gameAction :: Player -> act -> gam -> m (Either String gam)
    }
