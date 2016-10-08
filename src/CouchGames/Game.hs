module CouchGames.Game
    ( Game(..)
    ) where

import CouchGames.Player

data Game m conf gam act pub hid = Game {
      initGame :: conf -> [Player] -> m (Either String gam)
    , storeGame :: gam -> m ()
    , gameState :: gam -> m pub
    , playerState :: gam -> m hid
    , gameAction :: Player -> act -> gam -> m (Either String gam)
    }
