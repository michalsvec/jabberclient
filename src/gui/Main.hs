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

import Control.Parallel
import XMPP
import Network
import IO

-- The bot's JID is "bot@example.com"
botUsername = "jab_pavel"
botServer = "jabber.cz"
botPassword = "jab_pavel"

main :: IO Int
main = do
  qApplication ()
  hello <- qPushButton "Hello qtHaskell World"
  resize hello (200::Int, 60::Int)
  qshow hello ()




  rrr `par` qApplicationExec ()


rrr = do 
  -- Connect to server...
  c <- openStream botServer
  getStreamStart c

  runXMPP c $ do
    -- ...authenticate...
    startAuth botUsername botServer botPassword
    sendPresence
    -- ...and do something.
    run
  
-- nejake funkce
run :: XMPP ()
run = do
  -- Wait for an incoming message...
  
  msg <- waitForStanza (isChat `conj` hasBody)
  let sender = maybe "" id (getAttr "from" msg)
      len = length $ maybe "" id (getMessageBody msg)
  -- ...answer...
  sendMessage sender ("Your message was "++(show len)++" characters long.")
  -- ...and repeat.
  
--  run

-- End of file
