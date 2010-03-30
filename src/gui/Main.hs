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

import Qtc.ClassTypes.Core
import Qtc.ClassTypes.Gui
import Qtc.Classes.Base
import Qtc.Classes.Qccs
import Qtc.Classes.Qccs_h
import Qtc.Classes.Core
import Qth.ClassTypes.Core
import Qth.Core.Size
import Qth.Core.Rect
import Qtc.Core.QSize
import Qtc.Classes.Gui
import Qtc.Enums.Base
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

import XMPPLight

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

{-
  -- nepovedene shitties s vyskakovacim prihlasovaicm oknem
  connDialog1 <- myQDialog
  connDialog <- qMessageBox connDialog1
  connLayout <- qGridLayout ()

  hostInput <- qLineEdit () 
  userInput <- qLineEdit () 
  passwordInput <- qLineEdit () 
  portInput <- qLineEdit () 
  serverInput <- qLineEdit () 

  lab1 <- qLabel "labelka"
  addWidget connLayout (lab1, 0::Int, 0::Int, 1::Int, 1::Int)
  addWidget connLayout (hostInput, 0::Int, 1::Int, 1::Int, 1::Int)
  setLayout connDialog connLayout
  qshow connDialog1 ()
-}

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
  connectAction <- addAction fileMenu "&Connect"
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
  timer <- qTimer ()
-- !!!!!!!!!!!!!!11
-- tady je spatny slot, musi se to posilat do jineho slotu  
--  connectSlot timer "timeout()" sendButton "click()" $ on_timer_event conversationBox messageBox
  start timer (1000::Int)
  
  
  setWindowTitle dialog "Jabber client 3000"
  qshow dialog ()

  connection <- connectToServer server
  login connection username server passwd

  sendPresence connection

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

{-
 UZITECNY FICURKY
 
 addWidget do GridLayoutu
 void QGridLayout::addWidget ( QWidget * widget, int fromRow, int fromColumn, int rowSpan, int columnSpan, Qt::Alignment alignment = 0 )


-}
