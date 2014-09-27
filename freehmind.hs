module Main where

import Control.Monad.Trans (liftIO)
import "gtk" Graphics.UI.Gtk
import Diagrams.Backend.Cairo
import Diagrams.Prelude
import Diagrams.Backend.Gtk

main :: IO ()
main = do
  initGUI
  window <- windowNew
  canvas <- drawingAreaNew
  canvas `on` sizeRequest $ return (Requisition 256 256)
  set window [ containerBorderWidth := 10,
               containerChild := canvas ]
  canvas `on` exposeEvent $ renderFigure
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI


renderFigure :: EventM EExpose Bool
renderFigure = do
   win <- eventWindow
   liftIO $ renderToGtk win $ toGtkCoords figure
   return True


figure :: Diagram Cairo R2
figure =  unitCircle # scaleX 0.5 # rotateBy (1/6) # scale 50 # fc red  
