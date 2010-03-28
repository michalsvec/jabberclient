--
-- Project: jabclient
-- Author:  XXX
--

-- | Main project module.
module Main where

import Qtc.Classes.Qccs
import Qtc.Classes.Gui
import Qtc.Gui.Base
import Qtc.Gui.QApplication
import Qtc.Gui.QPushButton

import Control.Concurrent
import IO
--import XMPPConnection
import XMPP
import XMPPLight

-- The bot's JID is "bot@example.com"
botUsername = "jab_pavel"
botServer = "jabber.cz"
botPassword = "jab_pavel"

main :: IO ()-- Int
main = do
  qApplication ()
  hello <- qPushButton "Hello qtHaskell World"
  resize hello (200::Int, 60::Int)
  qshow hello ()

  -- Connect to server...
  c <- openStream botServer
  getStreamStart c

--  async_rcv c
  forkIO (async_rcv c)

  qApplicationExec ()
  closeConnection c

