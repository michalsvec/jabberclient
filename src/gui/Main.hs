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

--import Control.Concurrent
import IO
import XMPPLight
--import XMPPXML

-- The bot's JID is "bot@example.com"
server :: [Char]
server   = "jabber.cz"
username :: [Char]
username = "jab_pavel"
passwd :: [Char]
passwd = "jab_pavel"

main :: IO ()-- Int
main = do
  qApplication ()
  hello <- qPushButton "Hello qtHaskell World"
  resize hello (200::Int, 60::Int)
  qshow hello ()

  -- Connect to server...
  connection <- connectToServer server

--  async_rcv c
--  forkIO (async_rcv c)

  --startAuth botUsername botServer botPassword

  qApplicationExec ()
  closeConnection connection 

