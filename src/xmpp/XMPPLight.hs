--
-- Project: jabclient
-- Author:  pavel Srb
--

-- | Main project module.
module XMPPLight where

--import Control.Concurrent
--import Network
--import IO

{-
import Network
import System.IO
import Data.IORef
import Control.Monad
import XMPP
import XMPPMonad
-}

--import XMLParse
import XMPPConnection
import TCPConnection

async_rcv :: TCPConnection -> IO ()
async_rcv c = do
  getStanzas c
  return ()


{-
main :: IO ()-- Int
main = do
  qApplication ()
  hello <- qPushButton "Hello qtHaskell World"
  resize hello (200::Int, 60::Int)
  qshow hello ()

  -- Connect to server...
  c <- openStream botServer
  getStreamStart c

  --parseBuffered c deepTags
  getStanzas c

--  print (ssender)

--  forkIO (getStanzas c)

--  print ssender

  qApplicationExec ()
--  print ssender
  closeConnection c

-}


