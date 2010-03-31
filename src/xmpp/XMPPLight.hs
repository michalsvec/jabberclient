--
-- Project: jabclient
-- Author:  pavel Srb
--

-- | Main project module.
module XMPPLight where

import Control.Concurrent
--import Network
--import IO

{-
import Network
import System.IO
import Data.IORef
import XMPP
import XMPPMonad
-}

--import Control.Monad

--import Network
--import System.IO
--import Data.IORef

import TCPConnection 
import XMPPConnection hiding ( closeConnection )
import qualified XMPPConnection

import XMPPXML

--import XMPP

--import Control.Monad.State
import XMLParse
import System.Random
import Maybe

--import XMPPMonad hiding ( sendStanza )

--import Qtc.ClassTypes.Core
import Qtc.ClassTypes.Gui

connectToServer :: String -> IO TCPConnection
connectToServer server = do
  c <- openStream server
  getStreamStart c
  return c

closeConnection :: TCPConnection -> IO ()
closeConnection c = XMPPConnection.closeConnection c

login :: TCPConnection -> String -> String -> String -> IO ()
login c username server password = do
  response <- sendIqWait c server "get" [XML "query"
                                          [("xmlns","jabber:iq:auth")]
                                          [XML "username"
                                            []
                                            [CData username]
                                          ]
                                        ]

--  print $ response
  case xmlPath ["query","password"] response of
    Nothing -> error "plaintext authentication not supported by server"
    Just _ -> do
      response' <- sendIqWait c server "set" [XML "query"
                                            [("xmlns","jabber:iq:auth")]
                                            [XML "username" []
                                                     [CData username],
                                             XML "password" []
                                                     [CData password],
                                             XML "resource" []
                                                     [CData "hsXmpp"]]]
      case getAttr "type" response' of
        Just "result" -> putStrLn "Authentication succeeded"
        _ -> error "Authentication failed"



-- |Send an IQ request and wait for the response, with blocking other activity.
-- JID of recipient
-- Type of IQ, either \"get\" or \"set\"
-- Payload elements
-- Response stanza
sendIqWait :: TCPConnection -> String -> String -> [XMLElem] -> IO XMLElem
sendIqWait c to iqtype payload = do
  iqid <- sendIq c to iqtype payload
  waitForStanza c (100::Int) $ (hasNodeName "iq") `conj` (attributeMatches "id" (==iqid))
--  return $ fromMaybe (XML "iq" [] []) xxx --  return $ XML "iq" [] []


-- |Send an IQ request, returning the randomly generated ID.
-- ^JID of recipient
-- ^Type of IQ, either \"get\" or \"set\"
-- ^Payload elements
-- ^ID of sent stanza
sendIq :: TCPConnection -> String -> String -> [XMLElem] -> IO String
sendIq c to iqtype payload = do
  iqid <- randomIO::IO Int
  sendStanza c $ XML "iq"
                     [("to", to),
                      ("type", iqtype),
                      ("id", show iqid)]
                     payload
  return $ show iqid

waitForStanza :: TCPConnection -> Int -> (XMLElem -> Bool) -> IO XMLElem --(Maybe XMLElem)
waitForStanza c tryout predic = do
  if tryout < 1
    then do error "authentication: timeout reachet - no response from server"
    else do allStanzas <- getStanzas c
            matchPred allStanzas
            where matchPred (stanza:stanzas) = do
                    if predic stanza
                      then return stanza -- $ Just stanza
                      else matchPred stanzas
                  matchPred ([]) = do
                    threadDelay (10000)
                    waitForStanza c (tryout-1) predic
-- jestli to dojde sem znamena to ze sme cekali moc malo 
-- stalo by zato znova pockat :D aspon este malou chvili
-- -}

sendPresence :: TCPConnection -> IO ()
sendPresence c = sendStanza c $ XML "presence" [] []

       
{-
-- |Send a response to a received IQ stanza.
sendIqResponse :: XMLElem       -- ^Original stanza, from which id and
                                -- recipient are taken
               -> String        -- ^Type of response, either
                                -- \"result\" or \"error\"
               -> [XMLElem]     -- ^Payload elements
               -> XMPP (Maybe ())   -- ^Just () if original stanza had
                                    -- a \"from\" attribute
sendIqResponse inResponseTo iqtype payload =
      case getAttr "from" inResponseTo of
        Nothing ->
            -- "from" attribute missing?
            return Nothing
        Just sender ->
            let iqid = maybe "" id (getAttr "id" inResponseTo)
            in do
                sendStanza $ XML "iq"
                               [("to", sender),
                                ("type", iqtype),
                                ("id", iqid)]
                               payload
                return $ Just ()
-}

processStanzas :: TCPConnection -> [ QWidget () ] -> IO ()
processStanzas c l = do
  stanzas <- getStanzas c
  processStanza stanzas
    where 
     processStanza (x:xs)
                | isMessage x       = do
                                    -- jedna se o zpravu
                                    -- potreba vlozit nekam kde si toho uzivatel vsimne
                                    print $ " message stanza received: " ++ fromMaybe "" (getMessageBody x) ++ "]..."
                                    processStanza xs
                | isPresence x      = do
                                    -- jedna se o presence zpravu
                                    -- do kontakt listu poznacim se se uzivatel prihlasil
                                    print $ " presence stanza received..."
                                    processStanza xs
                | otherwise     = do
                                    -- jedna se o iq stanzu
                                    -- nevim co s tim 
                                    print $ " some stanza received..."
                                    processStanza xs
     processStanza [] = do 
                            print $ " list of stanzas processed..."

async_rcv :: TCPConnection -> IO ()
async_rcv c = do
  getStanzas c
  return ()

