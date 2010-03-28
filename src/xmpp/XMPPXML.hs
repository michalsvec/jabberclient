--
-- Project: jabclient
-- Author:  Jirka a Pavel
--

-- | Main project module.
module XMPPXML where

import Stanzas hiding (
                 sendIq
--               , sendIqWait
--               , hasBody
               , getMessageBody
--               , sendMessage
--               , sendPresence
--               , conj
--               , attributeMatches
               , isMessage
               , isPresence
               , isIq
--               , isChat
--               , isFrom
--               , iqXmlns
--               , iqGet
--               , iqSet
--               , handleVersion
               )
import qualified Stanzas
import XMLParse

getMessageBody :: XMLElem -> Maybe String
getMessageBody stanza = Stanzas.getMessageBody stanza

createMessage :: String -> String -> XMLElem
createMessage to body = XML "message"
                          [("to", to),
                           ("type", "chat")]
                          [XML "body" []
                               [CData body]]

isMessage :: XMLElem -> Bool
isMessage stanza = Stanzas.isMessage stanza

isPresence :: XMLElem -> Bool
isPresence stanza = Stanzas.isPresence stanza

isIq :: XMLElem -> Bool
isIq stanza = Stanzas.isIq stanza


{-
getMessageBody' :: XMLElem -> Maybe String
getMessageBody' stanza =
     do
       bodyTag <- xmlPath ["body"] stanza
       getCdata bodyTag

createMessage' :: String -> String -> XMLElem
createMessage' to body = XML "message"
                          [("to", to),
                           ("type", "chat")]
                          [XML "body" []
                               [CData body]]

isMessage' :: XMLElem -> Bool
isMessage' stanza = hasNodeName' "message" stanza

isPresence' :: XMLElem -> Bool
isPresence' stanza = hasNodeName' "presence" stanza

isIq' :: XMLElem -> Bool
isIq' stanza = hasNodeName' "iq" stanza

hasNodeName' :: String -> XMLElem -> Bool
hasNodeName' name ( XML name' _ _ ) = name == name'

-}