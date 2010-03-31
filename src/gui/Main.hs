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

import Qtc.Enums.Gui.QDialog --eRejected :: DialogCode eAccepted :: DialogCode
import System.Exit

import XMPPLight

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

  connectSlot acceptButton "clicked()" acceptButton "click()" $ on_conn_accepted envRefConn labInfo userInput passwordInput serverInput connDialog 
  connectSlot rejectButton "clicked()" rejectButton "click()" $ on_conn_rejected connDialog

  addWidget connLayout (acceptButton, 5::Int, 0::Int, 1::Int, 1::Int)
  addWidget connLayout (rejectButton, 5::Int, 1::Int, 1::Int, 1::Int)
  
  setLayout connDialog connLayout

  -- HLAVNI PROGRAM! 
  
  -- Definice jednotlivych widgetu v programu
  -- tlacitko
  sendButton <- myQPushButton $ "Odeslat"
  messageBox <- qLineEdit ()
  setText messageBox "tady pises zpravy bracho"
  conversationBox <- qTextEdit ()
  setPlainText conversationBox "tohle jsme si uz vsechno napsali - cool ne? :)"

 -- Definice jednotlivych widgetu v programu
  -- spojeni slotu a signalu 
  connectSlot sendButton "clicked()" sendButton "click()" $ on_button_clicked conversationBox messageBox

 -- defunice layoutu aplikace
  mainLayout <- qGridLayout ()

  menuBar <- qMenuBar ()
  fileMenu <- qMenu ("&File", dialog)
  --connectAction <- addAction fileMenu "&Connect"
  exitAction <- addAction fileMenu "E&xit"
  addMenu menuBar fileMenu
  connectSlot exitAction "triggered()" app "quit()" ()
  setMenuBar mainLayout menuBar

  contactList <- qListView ()

  -- pridani vsech widgetu do aplikace
  addWidget mainLayout (conversationBox, 0::Int, 0::Int, 1::Int, 2::Int)
  addWidget mainLayout (messageBox, 1::Int, 0::Int, 1::Int, 2::Int)
  addWidget mainLayout (sendButton, 1::Int, 1::Int, 1::Int, 1::Int)
  addWidget mainLayout (contactList, 0::Int, 2::Int, 2::Int, 1::Int)

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
    else print ""--sendPresence connection

  connection <- connectToServer server
  login connection username server passwd

  sendPresence connection

  res <- result connDialog ()

  print res



  ok <- qApplicationExec ()
  closeConnection connection 


on_button_clicked :: QTextEdit () -> QLineEdit () -> MyQPushButton -> IO ()
on_button_clicked cBox mBox this
 = do
  msg <- text mBox ()
  append cBox msg
  return ()

on_timer_event :: QTextEdit () -> QLineEdit () -> MyQPushButton -> IO ()
on_timer_event cBox mBox this
 = do 
  append cBox "a"
  return ()


on_conn_accepted :: EnvTCPConnection -> QLabel () -> QLineEdit () -> QLineEdit () -> QLineEdit () -> QDialog () -> MyQPushButton -> IO ()
on_conn_accepted envRefConn labInfo userInput passwordInput serverInput connDialog this
  = do
    user <- text userInput ()
    if user == "michalek"
      then do hide connDialog ()
      else do setText labInfo "Login incorrect"
    accept connDialog ()

on_conn_rejected :: QDialog () -> MyQPushButton -> IO ()
on_conn_rejected connDialog this = do
  reject connDialog ()

{-
 UZITECNY FICURKY
 
 addWidget do GridLayoutu
 void QGridLayout::addWidget ( QWidget * widget, int fromRow, int fromColumn, int rowSpan, int columnSpan, Qt::Alignment alignment = 0 )


-}
