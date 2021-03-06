-----------------------------------------------------------------------------
{-| Project   : Varianta - Fun: Jabber client
    Authors   : Pavel Srb (xsrbpa00)
--}
------------------------------------------------------------------------------

module XMPPLight where

import Control.Concurrent
import TCPConnection 
import XMPPConnection hiding ( closeConnection, getStanzas )
import qualified XMPPConnection
import Control.Monad.State
import XMLParse
import System.Random
import Maybe

import XMPPXML



connectToServer :: String -> IO TCPConnection
connectToServer server = do
  c <- openStream server
  getStreamStart c
  return c

emptyConnection :: IO TCPConnection
emptyConnection = newStream

closeConnection :: TCPConnection -> IO ()
closeConnection c = XMPPConnection.closeConnection c

login :: TCPConnection -> String -> String -> String -> IO ()
login c server username password = do
  response <- sendIqWait c server "get" [XML "query"
                                          [("xmlns","jabber:iq:auth")]
                                          [XML "username"
                                            []
                                            [CData username]
                                          ]
                                        ]
  case xmlPath ["query","password"] response of
    Nothing -> ioError $ userError "plaintext authentication not supported by server"
    Just _ -> do
      response' <- sendIqWait c server "set" [XML "query"
                                            [("xmlns","jabber:iq:auth")]
                                            [XML "username" []
                                                     [CData username],
                                             XML "password" []
                                                     [CData password],
                                             XML "resource" []
                                                     [CData "hasq-e jabber client"]]]
      case getAttr "type" response' of
        Just "result" -> return () --putStrLn "Authentication succeeded"
        _ -> ioError $ userError "Authentication failed"


sendIqWait :: TCPConnection -> String -> String -> [XMLElem] -> IO XMLElem
sendIqWait c to iqtype payload = do
  iqid <- sendIq c to iqtype payload
  waitForStanza c (100::Int) $ (hasNodeName "iq") `conj` (attributeMatches "id" (==iqid))


sendIq :: TCPConnection -> String -> String -> [XMLElem] -> IO String
sendIq c to iqtype payload = do
  iqid <- randomIO::IO Int
  sendStanza c $ XML "iq"
                     [("to", to),
                      ("type", iqtype),
                      ("id", show iqid)]
                     payload
  return $ show iqid

waitForStanza :: TCPConnection -> Int -> (XMLElem -> Bool) -> IO XMLElem
waitForStanza c tryout predic = do
  if tryout < 1
    then do ioError $ userError "authentication: timeout reachet - no response from server"
    else do allStanzas <- getStanzas c
            matchPred allStanzas
            where matchPred (stanza:stanzas) = do
                    if predic stanza
                      then return stanza 
                      else matchPred stanzas
                  matchPred ([]) = do
                    threadDelay (10000)
                    waitForStanza c (tryout-1) predic


getContactList :: TCPConnection -> IO [(String,String)]
getContactList c = do
    iqid <- randomIO::IO Int
    sendStanza c $ XML "iq"
                       [("type", "get"),
                        ("id", show iqid)]
                       [XML "query" [("xmlns", "jabber:iq:roster")] []]
    (XML _ _ body) <- waitForStanza c (100::Int) $ (hasNodeName "iq") `conj` (attributeMatches "id" (==(show iqid)))
    let (XML _ _ body2) = (body!!0)
    -- print $ show body2
    return $ getContact body2 []
        where
            getContact :: [XMLElem] -> [(String,String)] -> [(String,String)]
            getContact (x:xs) list = do
                                        let jid = fromMaybe "--err:jid--" (getAttr "jid" x)
                                        let name = fromMaybe jid (getAttr "name" x)
                                        getContact xs list ++ [ (name,jid) ]
            getContact [] list = list


sendPresence :: TCPConnection -> IO ()
sendPresence c = sendStanza c $ XML "presence" [] []

sendMessage :: TCPConnection -> String -> String -> IO ()
sendMessage c target text = do
  sendStanza c $ XML "message"
                   [("to", target),
                    ("type", "chat")]
                   [XML "body" []
                        [CData text]]

getStanzas :: TCPConnection -> IO [XMLElem]
getStanzas c = XMPPConnection.getStanzas c


