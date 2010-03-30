--
-- Project: jabclient
-- Author:  Jirka a Pavel
--

-- | Main project module.
module XMPPXML where

import Stanzas hiding (
                hasBody
               , getMessageBody
               , conj
               , isMessage
               , isPresence
               , isIq
               , isChat
               , isFrom
               , iqXmlns
               , iqGet
               , iqSet
               , handleVersion
               )
import qualified Stanzas
import XMLParse

type StanzaPredicate = (XMLElem -> Bool)

-- |Return true if the message stanza has body text.
hasBody :: StanzaPredicate
hasBody = Stanzas.hasBody 

-- |Get the body text of the message stanza, if any.
getMessageBody :: XMLElem -> Maybe String
getMessageBody stanza = Stanzas.getMessageBody stanza


--- Stanza predicates ---

-- |Conjunction (\"and\") of two predicates.
conj :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
conj = Stanzas.conj


--- The three basic stanza types ---

-- |Return true if the tag is a message stanza.
isMessage :: StanzaPredicate
isMessage = Stanzas.isMessage

-- |Return true if the tag is a presence stanza.
isPresence :: StanzaPredicate
isPresence = Stanzas.isPresence

-- |Return true if the tag is an IQ stanza.
isIq :: StanzaPredicate
isIq = Stanzas.isIq

-- |Return true if the tag is a chat message.
isChat :: StanzaPredicate
isChat = Stanzas.isChat

-- |Return true if the stanza is from the given JID.
isFrom :: String -> StanzaPredicate
isFrom = Stanzas.isFrom 

-- |Return true if the stanza is an IQ stanza in the given namespace.
iqXmlns :: String -> StanzaPredicate
iqXmlns = Stanzas.iqXmlns

-- |Return true if the stanza is a \"get\" request in the given namespace.
iqGet :: String -> StanzaPredicate
iqGet = Stanzas.iqGet

-- |Return true if the stanza is a \"set\" request in the given namespace.
iqSet :: String -> StanzaPredicate
iqSet = Stanzas.iqSet

