{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
{-| Program   : client.hs
    Copyright : tvrdaci 2010
    Project   : FPR
    Version   : 0
    Modified  : non-stop
--}
------------------------------------------------------------------------------

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
import Qtc.Gui.QHBoxLayout
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
import Qtc.Enums.Gui.QLineEdit	-- ePassword
import Qtc.Gui.QDialogButtonBox
import Qtc.Gui.QListWidget
import Qth.ClassTypes.Core.Size

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
server   = "njs.netlab.cz"
username :: [Char]
username = "jirkamelich"
passwd :: [Char]
passwd = "abx4C82abx4C82"

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
  serverInput <- qLineEdit () 
  passwordInput <- qLineEdit () 
  setEchoMode passwordInput (ePassword::EchoMode)
  
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
  acceptButton <- myQPushButton $ "Connect"
  rejectButton <- myQPushButton $ "Quit"

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
  sendButton <- myQPushButton $ "Odeslat"
  messageBox <- qLineEdit ()
  --setText messageBox "tady pises zpravy bracho"
  conversationBox <- qTextEdit ()
  setText conversationBox ""

  conversationFont <- qFont ()
  setPixelSize conversationFont (11::Int)
  setFont conversationBox conversationFont

  contactList <- qListWidget ()

  setReadOnly conversationBox True

 -- Definice jednotlivych widgetu v programu
  -- spojeni slotu a signalu 
  connectSlot sendButton "clicked()" sendButton "click()" $ on_button_clicked envRefConn envCurrentContactRef envContactList messageBox conversationBox contactList messageBox
  connectSlot messageBox "returnPressed()" sendButton "click()" $ on_button_clicked envRefConn envCurrentContactRef envContactList messageBox conversationBox contactList messageBox

 -- defunice layoutu aplikace
  mainLayout <- qGridLayout ()

  menuBar <- qMenuBar ()
  fileMenu <- qMenu ("&File", dialog)
  helpMenu <- qMenu ("&Help", dialog)
  exitAction <- addAction fileMenu "E&xit"
  aboutAction <- addAction helpMenu "A&bout"
  addMenu menuBar fileMenu
  addMenu menuBar helpMenu
  connectSlot exitAction "triggered()" app "quit()" ()
  connectSlot aboutAction "triggered()" dialog "click()" $ on_about_clicked
  setMenuBar mainLayout menuBar

  connectSlot acceptButton "clicked()" acceptButton "click()" $ on_conn_accepted envRefConn labInfo userInput passwordInput serverInput connDialog 

  -- pridani vsech widgetu do aplikace
  labChatingWith <- qLabel "Chatting with:"
  labChatingName <- qLabel "Nobody"
  addWidget mainLayout (labChatingWith, 0::Int, 0::Int, 1::Int, 1::Int)
  addWidget mainLayout (labChatingName, 0::Int, 1::Int, 1::Int, 1::Int)
  addWidget mainLayout (conversationBox, 1::Int, 0::Int, 1::Int, 3::Int)
  addWidget mainLayout (messageBox, 2::Int, 0::Int, 1::Int, 2::Int)
  addWidget mainLayout (sendButton, 2::Int, 2::Int, 1::Int, 1::Int)
  addWidget mainLayout (contactList, 0::Int, 3::Int, 3::Int, 1::Int)


  setColumnMinimumWidth mainLayout (0::Int, 100::Int)
  setColumnMinimumWidth mainLayout (1::Int, 150::Int)
  setColumnMinimumWidth mainLayout (2::Int, 100::Int)
  setColumnMinimumWidth mainLayout (3::Int, 150::Int)
  
  -- nastaveni layoutu
  setLayout dialog mainLayout
  
  -- nastaveni rozmeru
  resize dialog (500::Int, 400::Int)

  
  setWindowTitle dialog "hasq-e jabber client"
  qshow dialog ()

  retDialogCode <- exec connDialog ()
  if retDialogCode == 0
    then exitWith (ExitFailure 1)
    else print "nic slepa vetev"

  -- vytvoreni pripojeni na server       
  connection <- getVarTCPConnection envRefConn "connection"

  -- nacteni kontaktu do contact listu
  jid_name_list <- getContactList connection
  setup_contact_list envContactList jid_name_list contactList

  --nastaveni signalu na oznaceni prvku
  connectSlot contactList "itemDoubleClicked(QListWidgetItem*)" dialog "click(QListWidgetItem*)" $ on_contact_clicked envCurrentContactRef envContactList labChatingName conversationBox contactList

  -- nastaveni timeru
  timer <- qTimer ()
  connectSlot timer "timeout()" sendButton "timerEvent()" $ on_timer_event envRefConn envCurrentContactRef envContactList conversationBox contactList
  start timer (1000::Int) 

  -- odeslani infa o tom ze jsem se pripojil
  sendPresence connection

  ok <- qApplicationExec ()
--  return ()
  closeConnection connection 





on_about_clicked :: MyQPushButton -> IO ()
on_about_clicked layout
 =do
  -- help -> abou
  print "aboutclicked"
  helpDialog <- myQDialog
  labAbout <- qLabel "<center><b>jabber client 3000</b><br><br>Jiří Melichar, xmelic04<br>Pavel Srb, xsrbpa00<br>Michal Švec, xsvecm07<br><br>FIT VUT<br>FPR (c)2010</center>"
  helpLayout <- qHBoxLayout ()
  addWidget helpLayout labAbout
  setLayout helpDialog helpLayout
  resize helpDialog (300::Int, 300::Int)
  qshow helpDialog ()
  return ()

setup_contact_list :: EnvContactList -> [(String,String)] -> QListWidget() -> IO ()
setup_contact_list envContactList jid_name_list list
  = do  print $ show jid_name_list
        loop jid_name_list 0
        where 
            loop :: [(String,String)] -> Int -> IO ()
            loop ((a, b):xs) i = do 
                              setVarEnvContactList envContactList (show i) b
                              setVarEnvContactList envContactList ((show i)++"n") a
                              print "JID:"
                              print b
                              print "NAME:"
                              print a 
                              addItem list a
                              loop xs (i+1)
            loop [] _ = return ()

on_contact_clicked :: EnvCurrentContact -> EnvContactList -> QLabel() -> QLineEdit () -> QTextEdit() -> QListWidget() -> QWidget() -> QListWidgetItem() -> IO ()
on_contact_clicked envCurrentContact envContactList current_contact_label mBox cBox list this item
 = do
  sss <- currentRow list ()
  current_contact_jid <- getVarEnvContactList envContactList ( show sss )
  current_contact_name <- getVarEnvContactList envContactList ((show sss)++"n")
  setText current_contact_label current_contact_name
  setVarCurrentContact envCurrentContact current_contact_jid
  setFocus mBox ()
  print sss
  return ()


mapContactList :: QListWidget () -> String -> IO ()
mapContactList contactList jid
  = do
    --user <- jid
    addItem contactList jid

on_button_clicked :: EnvTCPConnection -> EnvCurrentContact -> EnvContactList -> QTextEdit () -> QListWidget() -> QLineEdit () -> MyQPushButton -> IO ()
on_button_clicked envRefConn envRef evnContactList cBox contactList mBox this 
 = do
  -- vytahnu si aktualni kontakt se kterym si pisu
  current_contact_jid <- getVarCurrentContact envRef
  if current_contact_jid == ""
    then return()
    else do msg <- text mBox ()
            
            item_count <- count contactList ()
            index <- getContactIndex_from_jid evnContactList current_contact_jid item_count 0
            to_contact <- getVarEnvContactList evnContactList ( (show index) ++ "n")
            append cBox ("<font color='"++ (get_color_from_array index) ++"'><b>" ++ to_contact ++ "</b></font> &lt;&lt; " ++ "<font color='#a1a1a1'>" ++ msg  ++ "</font>" )
  
            setText mBox ""
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
                                     item_count <- count contactList ()
                                     index <- getContactIndex evnContactList x item_count 0
                                     from_contact <- getVarEnvContactList evnContactList ( (show index) ++ "n")
                                     append cBox ("<font color='"++ (get_color_from_array index) ++"'><b>" ++ from_contact ++ "</b></font> &gt;&gt; " ++ msg )
                                     processStanza xs current_contact_jid
                 | isPresence x = do
                                     -- jedna se o presence zpravu
                                     -- do kontakt listu poznacim se se uzivatel prihlasil
                                     typeFont <- qFont ()
                              
                                     item_count <- count contactList ()
                                     -- presenceType <- show "unavailable"                                     
                                     index <- getContactIndex evnContactList x item_count 0
                                     print $ "Index:" ++ ( show index )
                                     let presenceType = getType x
                                     
                                     if ( index >= 0 ) 
                                        then do
                                            if ( presenceType == "unavailable" )
                                                then do 
                                                    setBold typeFont False
                                                else do
                                                    setBold typeFont True

                                            listItem <- item contactList index
                                            setFont listItem typeFont
                                        else do 
                                            setBold typeFont True 
                                                                          
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
                                                        if cur >= limit 
                                                            then do 
                                                                return (-1)
                                                            else do 
                                                                temp <- getVarEnvContactList envContactList ( show cur )
                                                                let is_from_cur_index = isFrom temp stanza
                                                                if is_from_cur_index 
                                                                    then do
                                                                        print "Temp:"
                                                                        print temp
                                                                        return cur
                                                                    else do
                                                                        getContactIndex envContactList stanza limit (cur+1)
                                                        

getContactIndex_from_jid :: EnvContactList -> String -> Int -> Int -> IO (Int)
getContactIndex_from_jid envContactList jid limit cur = do 
                                                        if cur >= limit 
                                                            then do 
                                                                return (-1)
                                                            else do 
                                                                temp <- getVarEnvContactList envContactList ( show cur )
                                                                if temp == jid 
                                                                    then do
                                                                        print "Temp:"
                                                                        print temp
                                                                        return cur
                                                                    else do
                                                                        getContactIndex_from_jid envContactList jid limit (cur+1)



on_conn_accepted :: EnvTCPConnection -> QLabel () -> QLineEdit () -> QLineEdit () -> QLineEdit () -> QDialog () -> MyQPushButton -> IO ()
on_conn_accepted envRefConn labInfo userInput passwordInput serverInput connDialog this = do
  loginErr <- nullEnvInt
  setVarInt loginErr "connect" 0
  setVarInt loginErr "auth" 0
  --server   <- text serverInput ()
  --username <- text userInput ()
  --passwd   <- text passwordInput ()
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



get_color_from_array :: Int -> String
get_color_from_array index
 = do
  let colors = ["#6B6C42","#6F613C","#657F7C","#FF5864","#DE7E0E","#B4C533","#ABACC1","#110757","#3C5B16","#660C2B","#567184","#C9D411","#D3A67C","#38176A","#877D95","#E17709"]
  (colors !! (index `mod` (length colors)))

{-
 UZITECNY FICURKY
 
 addWidget do GridLayoutu
 void QGridLayout::addWidget ( QWidget * widget, int fromRow, int fromColumn, int rowSpan, int columnSpan, Qt::Alignment alignment = 0 )


-}
