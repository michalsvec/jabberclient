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
import Qtc.Core.QTimer
import Qtc.Enums.Gui.QDialogButtonBox
import Qtc.Gui.QDialogButtonBox
import Qtc.Gui.QListWidget

import Qtc.Enums.Gui.QDialog --eRejected :: DialogCode eAccepted :: DialogCode
import System.Exit

import XMPPLight
import XMPPXML

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

  -- nacteni kontaktu do contact listu
  mapM_ (mapContactList contactList) ["jirik", "misa", "paja"]
  --nastaveni signalu na oznaceni prvku
  -- shity
  --mouseDoubleClickEvent contactList (on_contact_clicked contactList conversationBox messageBox)::QMouseEvent
  --connectSlot contactButton "clicked()" contactButton "on_contact_clicked()" $ on_contact_clicked contactList conversationBox messageBox

  connectSlot contactList "itemDoubleClicked(QListWidgetItem*)" dialog "click(QListWidgetItem*)" $ on_contact_clicked conversationBox

  -- odeslani infa o tom ze jsem se pripojil
  sendPresence connection

  -- nastaveni timeru

  timer <- qTimer ()
  connectSlot timer "timeout()" sendButton "timerEvent()" $ on_timer_event envRefConn envCurrentContactRef conversationBox
  start timer (1000::Int)

  ok <- qApplicationExec ()
--  return ()
  closeConnection connection 
{-
tmpShit :: [QListWidgetItem] -> QTextEdit() -> IO ()
tmpShit x cBox
 = do
  label <- text x ()
  append cBox label
  tmpShit x cBox
  return ()
-}

on_contact_clicked :: QTextEdit() -> QDialog() -> QListWidgetItem() -> IO ()
on_contact_clicked cBox this item
 = do
  append cBox "tralala"


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


on_timer_event :: EnvTCPConnection -> EnvCurrentContact -> QTextEdit () -> MyQPushButton -> IO ()
on_timer_event envRefConn envRef cBox this
   = do
    current_contact_jid <- getVarCurrentContact envRef
    tcp_connection <- getVarTCPConnection envRefConn "connection"
    stanzas <- getStanzas tcp_connection
    print $ "Current contact jid:" ++ current_contact_jid
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
                 | isPresence x      = do
                                     -- jedna se o presence zpravu
                                     -- do kontakt listu poznacim se se uzivatel prihlasil
                                     print $ " presence stanza received..."
                                     processStanza xs current_contact_jid
                 | otherwise     = do
                                     -- jedna se o iq stanzu
                                     -- nevim co s tim 
                                     print $ " some stanza received..."
                                     processStanza xs current_contact_jid
      processStanza [] _ = do 
                             print $ " list of stanzas processed..." 





on_conn_accepted :: EnvTCPConnection -> QLabel () -> QLineEdit () -> QLineEdit () -> QLineEdit () -> QDialog () -> MyQPushButton -> IO ()
on_conn_accepted envRefConn labInfo userInput passwordInput serverInput connDialog this = do
{-    server <- text serverInput ()
    connection <- connectToServer server `catch` (\e -> do let fuck = True ) -- (\e -> do setText labInfo $ "Login incorrect " ++ show e)
    prin
    return ()
-}
  connection <- connectToServer server
  login connection username server passwd
  setVarTCPConnection envRefConn "connection" connection
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
