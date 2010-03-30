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
import Qtc.Gui.QGroupBox
import Qtc.Gui.QDialogButtonBox
import Qtc.Enums.Gui.QDialogButtonBox
import Qtc.Gui.QVBoxLayout
import Qtc.Gui.QHBoxLayout
import Qtc.Gui.QGridLayout
import Qtc.Gui.QTextEdit
import Qtc.Gui.QLineEdit
import Qtc.Gui.QLabel
import Qtc.Gui.QWidget
import Qtc.Gui.QPushButton
import Qtc.Gui.QMessageBox

type MyQDialog = QWidgetSc (CMyQDialog)
data CMyQDialog = CMyQDialog

myQDialog :: IO (MyQDialog)
myQDialog = qSubClass $ qWidget ()

type MyQPushButton = QPushButtonSc (CMyQPushButton)
data CMyQPushButton = CMyQPushButton

myQPushButton :: String -> IO (MyQPushButton)
myQPushButton t = qSubClass $ qPushButton t

main :: IO ()
main
	= do
		app <- qApplication ()
		dialog <- myQDialog
		mb <- qMessageBox dialog
		mainLayout <- qVBoxLayout ()
	
	--	Definice jednotlivych widgetu v programu
	
		-- tlacitko
		tbutton <- myQPushButton $ "Buttonek"
	
		-- velky textedit
		bigEditor <- qTextEdit ()
		setPlainText bigEditor "velkej editor, kde se ukazuje konverzace"
	
		-- maly textedit
		smallEditor <- qTextEdit ()
		setPlainText smallEditor "malej editurek, kam se muze psat taky treba"
	
	--	Definice jednotlivych widgetu v programu
		-- spojeni slotu a signalu 
		connectSlot tbutton "clicked()" tbutton "click()" $ on_button_clicked bigEditor smallEditor
	
		-- pridani widgetu do aplikace
		addWidget mainLayout tbutton
		addWidget mainLayout bigEditor
		addWidget mainLayout smallEditor
	
		-- nastaveni layoutu
		setLayout dialog mainLayout
		
		setWindowTitle dialog "Jabber client 3000"
		qshow dialog ()
		ok <- qApplicationExec ()
		return()


on_button_clicked :: QTextEdit () -> QTextEdit () -> MyQPushButton -> IO ()
on_button_clicked bEdit sEdit this
	= do
		smallEdit <- toPlainText bEdit ()
		setText sEdit smallEdit
		return ()

