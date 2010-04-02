{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
{-| Program   : client.hs
    Copyright : tvrdaci 2010
    Project   : FPR
    Version   : 0
    Modified  : non-stop
--}
-----------------------------------------------------------------------------

module Main where


import Qtc.ClassTypes.Gui
import Qtc.Classes.Qccs
import Qtc.Classes.Gui
import Qtc.Enums.Classes.Core
import Qtc.Core.Base
import Qtc.Gui.Base
import Qtc.Core.QCoreApplication
import Qtc.Gui.QApplication
import Qtc.Gui.QDialog
import Qtc.Gui.QMenuBar
import Qtc.Gui.QMenu
import Qtc.Gui.QLayout
import Qtc.Gui.QGridLayout
import Qtc.Gui.QTextEdit
import Qtc.Gui.QLineEdit
import Qtc.Gui.QLabel
import Qtc.Gui.QWidget
import Qtc.Gui.QPushButton
import Qtc.Gui.QMessageBox
import Qtc.Gui.QListView
import Qtc.Gui.QFont
import Qtc.Gui.QListWidgetItem
import Qtc.Classes.Gui
import Qtc.Core.QTimer
import Qtc.Enums.Gui.QDialogButtonBox
import Qtc.Gui.QDialogButtonBox
import Qtc.Gui.QListWidget

import Qtc.Enums.Gui.QDialog --eRejected :: DialogCode eAccepted :: DialogCode
import System.Exit

import XMPPLight
import XMPPXML
import XMLParse
import Control.Monad.State

import Maybe

import Global

type MyQDialog = QWidgetSc (CMyQDialog)
data CMyQDialog = CMyQDialog

myQDialog :: IO (MyQDialog)
myQDialog = qSubClass $ qWidget ()

type MyQPushButton = QPushButtonSc (CMyQPushButton)
data CMyQPushButton = CMyQPushButton

myQPushButton :: String -> IO (MyQPushButton)
myQPushButton t = qSubClass $ qPushButton t

server :: [Char]
server   = "jabber.cz"
username :: [Char]
username = "jab_pavel"
passwd :: [Char]
passwd = "jab_pavel"

main :: IO ()
main = do
  app <- qApplication ()
  dialog <- myQDialog
  mb <- qMessageBox dialog

  envRefConn <- nullEnvTCPConnection
  envCurrentContactRef <- nullEnvCurrentContact
  envTCPConnection <- nullEnvTCPConnection

  -- zobrazeni uvodniho dialogu pro pripojeni na server
  connDialog <- qDialog dialog
  connLayout <- qGridLayout ()
  setModal connDialog True
  
  userInput <- qLineEdit () 
  passwordInput <- qLineEdit () 
  serverInput <- qLineEdit () 

  labInfo <- qLabel "Connect pls"
  lab1 <- qLabel "Server: "
  lab2 <- qLabel "User: "
  lab3 <- qLabel "Password: "
  
  addWidget connLayout (labInfo, 0::Int, 0::Int, 1::Int, 2::Int)
  addWidget connLayout (lab1, 1::Int, 0::Int, 1::Int, 1::Int)
  addWidget connLayout (serverInput, 1::Int, 1::Int, 1::Int, 1::Int)
  addWidget connLayout (lab2, 2::Int, 0::Int, 1::Int, 1::Int)
  addWidget connLayout (userInput, 2::Int, 1::Int, 1::Int, 1::Int)
  addWidget connLayout (lab3, 3::Int, 0::Int, 1::Int, 1::Int)
  addWidget connLayout (passwordInput, 3::Int, 1::Int, 1::Int, 1::Int)
  
  -- tlacitka pro potverzeni - v pripade odmitnuti zavre aplikaci, 
  -- v pripade potvrzeni vola funkci, ktera se prihlasi a zmizi okenko
  acceptButton <- myQPushButton $ "Connect me, bro"
  rejectButton <- myQPushButton $ "Piss off"

  envRefConn <- nullEnvTCPConnection
  envCurrentContactRef <- nullEnvCurrentContact
  envContactList <- nullEnvContactList

  connectSlot acceptButton "clicked()" acceptButton "click()" $ on_conn_accepted envRefConn labInfo userInput passwordInput serverInput connDialog 
  connectSlot rejectButton "clicked()" rejectButton "click()" $ on_conn_rejected connDialog

  addWidget connLayout (acceptButton, 5::Int, 0::Int, 1::Int, 1::Int)
  addWidget connLayout (rejectButton, 5::Int, 1::Int, 1::Int, 1::Int)
  
  setLayout connDialog connLayout


	-- ------------------------------------------------------------------------------------------------------------------------------
  -- HLAVNI PROGRAM! 
  
  setVarCurrentContact envCurrentContactRef "jirkamelich@njs.netlab.cz"
  
  -- Definice jednotlivych widgetu v programu
  -- tlacitko
  sendButton <- myQPushButton $ "Odeslat"
  contactButton <- myQPushButton $ "Kontaktovat"
  messageBox <- qLineEdit ()
  setText messageBox "tady pises zpravy bracho"
  conversationBox <- qTextEdit ()
  setPlainText conversationBox "tohle jsme si uz vsechno napsali - cool ne? :)"

 -- Definice jednotlivych widgetu v programu
  -- spojeni slotu a signalu 
  connectSlot sendButton "clicked()" sendButton "click()" $ on_button_clicked envRefConn envCurrentContactRef conversationBox messageBox
  connectSlot messageBox "returnPressed()" sendButton "click()" $ on_button_clicked envRefConn envCurrentContactRef conversationBox messageBox
 -- defunice layoutu aplikace
  mainLayout <- qGridLayout ()

  menuBar <- qMenuBar ()
  fileMenu <- qMenu ("&File", dialog)
  --connectAction <- addAction fileMenu "&Connect"
  exitAction <- addAction fileMenu "E&xit"
  addMenu menuBar fileMenu
  connectSlot exitAction "triggered()" app "quit()" ()
  setMenuBar mainLayout menuBar

  contactList <- qListWidget ()

  -- pridani vsech widgetu do aplikace
  addWidget mainLayout (conversationBox, 0::Int, 0::Int, 1::Int, 2::Int)
  addWidget mainLayout (messageBox, 1::Int, 0::Int, 1::Int, 2::Int)
  addWidget mainLayout (sendButton, 1::Int, 1::Int, 1::Int, 1::Int)
  addWidget mainLayout (contactList, 0::Int, 2::Int, 1::Int, 1::Int)
  addWidget mainLayout (contactButton, 1::Int, 2::Int, 1::Int, 1::Int)

  setColumnMinimumWidth mainLayout (0::Int, 300::Int)
  
  -- nastaveni layoutu
  setLayout dialog mainLayout
  
  -- nastaveni timeru
{-
  timer <- qTimer ()
  connectSlot timer "timeout()" sendButton "timerEvent()" $ on_timer_event conversationBox messageBox
  start timer (1000::Int)
-}  
  
  setWindowTitle dialog "Jabber client 3000"
  qshow dialog ()

  retDialogCode <- exec connDialog ()
  if retDialogCode == 0
    then exitWith (ExitFailure 1)
    else print "nic slepa vetev"

  -- vytvoreni pripojeni na server       
  connection <- getVarTCPConnection envRefConn "connection"

  -- odeslani infa o tom ze jsem se pripojil
  sendPresence connection

  -- nacteni kontaktu do contact listu
  jid_name_list <- getContactList connection
  setup_contact_list envContactList jid_name_list contactList
      
  --mapM_ (mapContactList contactList) ["jirik", "misa", "paja"]
  --nastaveni signalu na oznaceni prvku
  connectSlot contactList "itemDoubleClicked(QListWidgetItem*)" dialog "click(QListWidgetItem*)" $ on_contact_clicked envCurrentContactRef envContactList conversationBox contactList



  -- nastaveni timeru

  timer <- qTimer ()
  connectSlot timer "timeout()" sendButton "timerEvent()" $ on_timer_event envRefConn envCurrentContactRef envContactList conversationBox
  start timer (1000::Int)

  ok <- qApplicationExec ()
--  return ()
  closeConnection connection 

setup_contact_list :: EnvContactList -> [(String,String)] -> QListWidget() -> IO ()
setup_contact_list envContactList jid_name_list list
  = do loop jid_name_list 0
    where 
        loop :: [(String,String)] -> Int -> IO ()
        loop ((a, b):xs) i = do 
                              setVarEnvContactList envContactList (show i) b
                              setVarEnvContactList envContactList ((show i)++"n") a
                              addItem list a
                              loop xs (i+1)
        loop [] _ = return ()

on_contact_clicked :: EnvCurrentContact -> EnvContactList -> QTextEdit() -> QListWidget() -> QWidget() -> QListWidgetItem() -> IO ()
on_contact_clicked envCurrentContact envContactList cBox list this item
 = do
  append cBox "tralala"
  sss <- currentRow list ()
  current_contact_jid <- getVarEnvContactList envContactList ( show sss )
  print $ show current_contact_jid
  setVarCurrentContact envCurrentContact current_contact_jid
  print sss
  return ()


mapContactList :: QListWidget () -> String -> IO ()
mapContactList contactList jid
  = do
    --user <- jid
    addItem contactList jid

on_button_clicked :: EnvTCPConnection -> EnvCurrentContact -> QTextEdit () -> QLineEdit () -> MyQPushButton -> IO ()
on_button_clicked envRefConn envRef cBox mBox this 
 = do
  msg <- text mBox ()
  append cBox msg
  setText mBox ""
  -- vytahnu si aktualni kontakt se kterym si pisu
  current_contact_jid <- getVarCurrentContact envRef
  -- vytahnu si aktualni pripojeni 
  tcp_connection <- getVarTCPConnection envRefConn "connection"
  -- zpravu mu odeslu
  sendMessage tcp_connection current_contact_jid msg
  return ()


on_timer_event :: EnvTCPConnection -> EnvCurrentContact -> EnvContactList -> QTextEdit () -> QListWidget() -> MyQPushButton -> IO ()
on_timer_event envRefConn envRef evnContactList cBox contactList this
   = do
    current_contact_jid <- getVarCurrentContact envRef
    tcp_connection <- getVarTCPConnection envRefConn "connection"
    stanzas <- getStanzas tcp_connection
    print $ show stanzas
    print ""
    print ""    
    print $ "Current contact jid:" ++ current_contact_jid
    print ""
    processStanza stanzas current_contact_jid
      where 
      processStanza (x:xs) current_contact_jid
                 | isMessage x = do
                                     print $ show x
                                     let sw_from = isFrom current_contact_jid x
                                     print $ "is from current:" ++ (show sw_from)
                                     -- jedna se o zpravu
                                     -- potreba vlozit nekam kde si toho uzivatel vsimne
                                     let msg  = fromMaybe "---" (getMessageBody x)
                                     -- print $ (" message stanza received: " ++ msg ++ "]...")
                                     append cBox msg
                                     processStanza xs current_contact_jid
                 | isPresence x = do
                                     -- jedna se o presence zpravu
                                     -- do kontakt listu poznacim se se uzivatel prihlasil
                                     boldFont <- qFont ()
                                     
                                     item_count <- count contactList ()
                                     
                                     let index = getContactIndex evnContactList x item_count 0
                                     
                                     listItem <- item contactList (0::Int)

                                     setBold boldFont True 
                                     setFont listItem boldFont
                                     
                                     print $ "\n\n presence stanza received... \n\n"
                                     processStanza xs current_contact_jid
                 | otherwise = do
                                     -- jedna se o iq stanzu
                                     -- nevim co s tim 
                                     print $ " some stanza received..."
                                     processStanza xs current_contact_jid
      processStanza [] _ = do 
                             print $ " list of stanzas processed..." 


getContactIndex :: EnvContactList -> XMLElem -> Int -> Int -> IO (Int)
getContactIndex envContactList stanza limit cur = do 
                                                        if limit >= cur 
                                                            then do 
                                                                return (-1)
                                                            else do 
                                                                temp <- getVarEnvContactList envContactList ( show cur )
                                                                let is_from_cur_index = isFrom temp stanza
                                                                if is_from_cur_index 
                                                                    then return cur
                                                                    else do
                                                                        getContactIndex envContactList stanza limit (cur+1)
                                                        
{-
                                                | limit >= cur                      = return (-1)
                                                | limit < cur && is_from_cur_index  = return cur
                                                | otherwise                         = 
                                                where 
                                                        is_from_cur_index :: Bool
                                                        is_from_cur_index = do 
                                                                                
-}
{-
= do
                                                        is_from_cur_index <- 
                                                        if limit>= cur
                                                                return -1
                                                        else 
                                                                return getContactIndex envContactList stanza limit (cur+1)   

-}

on_conn_accepted :: EnvTCPConnection -> QLabel () -> QLineEdit () -> QLineEdit () -> QLineEdit () -> QDialog () -> MyQPushButton -> IO ()
on_conn_accepted envRefConn labInfo userInput passwordInput serverInput connDialog this = do
  loginErr <- nullEnvInt
  setVarInt loginErr "connect" 0
  setVarInt loginErr "auth" 0
  server   <- text serverInput ()
  username <- text userInput ()
  passwd   <- text passwordInput ()
  if (server == "" || username == "" || passwd == "")
    then do setText labInfo $ "Error: Each field is required"
            return () 
    else do connection <- connectToServer server `catch` (\e -> do
              setText labInfo $ "Error: Can't connect to server"
              setVarInt loginErr "connect" 1
              emptyConnection)
            code <- getVarInt loginErr "connect"
            if code == 1
              then return () 
              else do login connection server username passwd `catch` (\e -> do
                        setText labInfo $ "Error: Bad login or password"
                        setVarInt loginErr "auth" 1
                        return ())
                      code <- getVarInt loginErr "auth"
                      if code == 1
                        then return () 
                        else do setVarTCPConnection envRefConn "connection" connection
                                accept connDialog ()

{-    
    user <- text userInput ()
    if user == "michalek"
      then do hide connDialog ()
      else do setText labInfo "Login incorrect"
-}        
--    accept connDialog ()

on_conn_rejected :: QDialog () -> MyQPushButton -> IO ()
on_conn_rejected connDialog this = do
  reject connDialog ()

{-
 UZITECNY FICURKY
 
 addWidget do GridLayoutu
 void QGridLayout::addWidget ( QWidget * widget, int fromRow, int fromColumn, int rowSpan, int columnSpan, Qt::Alignment alignment = 0 )


-}
