module GUI (
      startGUI
    ) where

import Graphics.UI.Gtk hiding (disconnect)
import Graphics.UI.Gtk.Glade
import Manager
import Control.Concurrent (forkIO)

data GUI = GUI {
      mainWindow :: Window,
      startButton :: Button,
      directoryChooser :: FileChooserButton,
      statusBar :: Statusbar
      }

startGUI :: IO ()
startGUI = initGUI >> loadGlade "gui.glade" >>= prepareGUI >> mainGUI

prepareGUI gui =
    do
    onDestroy (mainWindow gui) mainQuit
    onClicked (startButton gui) $ processClick gui
    where
        processClick gui = fileChooserGetFilename (directoryChooser gui) >>= (\path -> forkIO $ startImport path) >> return ()
            where
            startImport (Just path) = do
                widgetSetSensitivity (mainWindow gui) False
                context <- statusbarGetContextId (statusBar gui) "import"
                statusbarPush (statusBar gui) context "Import in progress..."
                processDirectory path
                statusbarPush (statusBar gui) context "Import finished!"
                widgetSetSensitivity (mainWindow gui) True
            startImport Nothing = return ()

loadGlade gladePath =
    do 
    Just xml <- xmlNew gladePath
       
    mainWindow <- xmlGetWidget xml castToWindow "mainWindow"
    startButton <- xmlGetWidget xml castToButton "startButton"
    directoryChooser <- xmlGetWidget xml castToFileChooserButton "directoryChooser"
    statusBar <- xmlGetWidget xml castToStatusbar "statusBar"
    
    context <- statusbarGetContextId statusBar "begin"
    statusbarPush statusBar context "Ready!"
    
    return $ GUI mainWindow startButton directoryChooser statusBar
