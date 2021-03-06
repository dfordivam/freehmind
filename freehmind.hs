{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Monad.Trans (liftIO)
import "gtk" Graphics.UI.Gtk
import Diagrams.Backend.Cairo
import Diagrams.Prelude
import Diagrams.Backend.Gtk
import Diagrams.TwoD.Size

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
   liftIO $ renderToGtk win $ toGtkCoords (renderMindMap sampleMM)
   return True


figure :: Diagram Cairo R2
figure =  unitCircle # scaleX 0.5 # rotateBy (1/6) # scale 50 # fc red  


-- View' Data Structure
--
data FhmNode = FhmNode String [ FhmNode ]

sampleMM :: FhmNode
sampleMM = FhmNode "Root" [(leafN "hello"), (leafN "world")]

leafN txt = FhmNode txt []

-- Rendering Algorithm
-- The root node is kept at the middle and its children nodes are rendered
-- around it, even numbered nodes on right, odd numbered on left.
--
-- All the children nodes are at a fixed vertical distance from its parent node.
-- Each children node also has its own horizontal size and 
-- therefore it determines its horizontal offset
--
-- Algorithm: 
-- Horizontal size is computed recursively
-- Horizontal offset is propotional to the size.
--

horizontalSize :: FhmNode -> Int
horizontalSize node = sum (mySize: (map horizontalSize children) )
    where mySize   = 50
          children = (\(FhmNode _ x) -> x) node

renderMindMap :: FhmNode -> Diagram Cairo R2
renderMindMap root = (renderNode root) ||| (renderChildren children)
    where children = (\(FhmNode _ x) -> x) root

renderNode :: FhmNode -> Diagram Cairo R2
renderNode node = text txt # fontSizeN 0.1 <> square 1 # scale 50
    where value = (\(FhmNode x _) -> x) node
          size  = Diagrams.TwoD.Size.size2D emptyNode
          txt     = value

emptyNode :: Diagram Cairo R2
emptyNode = square 1 # scale 50

renderChildren :: [FhmNode] -> Diagram Cairo R2
renderChildren [] = circle 1
renderChildren (x:xs) = renderNode x === renderChildren xs

