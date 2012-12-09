{-# LANGUAGE TupleSections #-}
module Main where

--import Control.Applicative
--import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust)
import Data.IORef
import Data.List
import Data.Ord
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Types
--import Graphics.Rendering.Pango
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC (gcNew, gcGetValues, gcSetValues, foreground, lineWidth)
import Graphics.UI.Gtk.Poppler.Document
import Graphics.UI.Gtk.Poppler.Page
--import Graphics.UI.Gtk.Poppler.Annot
--import Graphics.UI.Gtk.Windows.Window
import System.Console.GetOpt
import System.Glib
import Data.Char
--import System.Process

-- IF OLD GHC
atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef ref a = do
    x <- atomicModifyIORef ref (\_ -> (a, ()))
    x `seq` return ()

-- Would be useful to have in GHC
modifyIORef' ref f = do
  writeIORef ref =<< f =<< readIORef ref


-- --help

-- Test multi-monitor on Xfce4

{-
reload program
stretch vs scale
page_get_mapping_annotation
-}

data Config = Config {
  {-
  duration
  last-minutes
  caching
  compression
  flash
  -}
}

data TimerState = Counting Integer{-end time-} | Paused Integer{-seconds-}

data Blanking = BlankNone | BlankBlack | BlankWhite
data State = State
 { timer :: IORef TimerState -- 'Maybe' for "dont" display or haven't started counting yet? Or as a seperate flag?
 , blanking :: IORef Blanking
 , documentURL :: IORef String -- TODO: Maybe
 , warningTime :: IORef Integer{-microseconds-}
 -- split ratio
 -- switch screens
 , views :: IORef (Map.Map Widget (Int{-width-}, Int{-height-}))
 , document :: IORef (Maybe Document)
 , pages :: IORef (Map.Map (Int{-width-}, Int{-height-}) (Map.Map Int{-page number-} Pixmap{-cached page-}))
 , pageAdjustment :: Adjustment {- immutable -}
 , topNotebook :: Notebook
 , presenterPaned :: HPaned
}

data GuiState = GuiState {}

options = []

defaultConfig = Config {}

main :: IO ()
main = do
  -- Get program arguments.
  args <- initGUI
  case getOpt Permute options args of
    (o, [file], []) -> return (foldl (flip ($)) defaultConfig o) >> guiMain file
    (_, _, errors) -> ioError (userError (concat errors ++ usageInfo header options))
  where header = "Usage: PdfViewer [OPTION...] file"

guiMain :: FilePath -> IO ()
guiMain file = do
  -- Shared state
  state <- return State
    `ap` newIORef (Paused (6 * 1000 * 1000))
    `ap` newIORef BlankNone
    `ap` newIORef ("file://"++file)
    `ap` newIORef (3 * 1000 * 1000)
    `ap` newIORef Map.empty {-views-}
    `ap` newIORef Nothing {-document-}
    `ap` newIORef Map.empty {-pages-}
    `ap` adjustmentNew 0 0 0 1 10 1 {-pageAdjustment-}
    `ap` notebookNew {-topNotebook-}
    `ap` hPanedNew {-presenterPaned-}

  -------------------
  -- Audience window
  -------------------
  do window <- makeWindow state
     view <- makeView state id False
     window `containerAdd` view

  -------------------
  -- Presenter window
  -------------------
  do window <- makeWindow state
     windowBox <- vBoxNew False 0
     window `containerAdd` windowBox
     topNotebook state `set` [notebookShowBorder := False, notebookShowTabs := False]
     boxPackStart windowBox (topNotebook state) PackGrow 0

     -- Views
     do -- Thumbnails
        do scrolled <- scrolledWindowNew Nothing Nothing
           layout <- layoutNew Nothing Nothing
           notebookInsertPage (topNotebook state) scrolled "3" 1
           scrolled `containerAdd` layout
           oldWidth <- newIORef 0
           heightRef <- newIORef 0
           let columns = 4

           -- Recompute when the view size changes
           let recomputeViews (Graphics.UI.Gtk.Rectangle _ _ newWidth _) = do
                 oldWidth' <- readIORef oldWidth
                 when (oldWidth' /= newWidth) $ do -- Keep from looping forever
                   containerForeach layout (containerRemove layout)
                   numPages <- pageAdjustment state `get` adjustmentUpper
                   let width = newWidth `div` columns
                       height = width * 3 `div` columns
                       numRows = round numPages `div` columns + 1
                   putStrLn $ "RECOMPUTING THUMBS" ++ show (numPages, newWidth, width, height, numRows)
                   layoutSetSize layout newWidth (height*numRows)
                   () <- sequence_ [ do
                     let page = columns*row+col+1
                     view <- makeView state (const page) True
                     widgetSetSizeRequest view width height
                     layoutPut layout view (col*width) (row*height)
                     view `widgetAddEvents` [ButtonPressMask, ButtonReleaseMask]
                     view `on` buttonReleaseEvent $ tryEvent $
                          liftIO $ pageAdjustment state `set` [adjustmentValue := fromIntegral page]
                     | row <- [0..numRows-1], col <- [0..3]]
                   writeIORef heightRef height
                   widgetShowAll layout
                   writeIORef oldWidth newWidth
                   return ()
           layout `onSizeAllocate` recomputeViews
           --onZoom $ recocmputeViews

           -- Scroll to keep the current slide visible
           adjustment <- scrolledWindowGetVAdjustment scrolled
           let update = do p <- liftM round $ get (pageAdjustment state) adjustmentValue
                           let row = (p-1) `div` columns
                           height <- readIORef heightRef
                           adjustmentClampPage adjustment (fromIntegral $ row*height) (fromIntegral $ row*height+height)
           (pageAdjustment state) `onValueChanged` update
           (pageAdjustment state) `onAdjChanged` update

        -- Previews
        do notebookAppendPage (topNotebook state) (presenterPaned state) "1"
           view1 <- makeView state id False
           view2 <- makeView state (+1) False
           panedPack1 (presenterPaned state) view1 True True
           panedPack2 (presenterPaned state) view2 True True

        return ()

     -- Meta-data
     do metaBox <- hBoxNew False 0
        boxPackEnd windowBox metaBox PackNatural 0

        -- Current time
        do time <- labelNew Nothing
           timeoutAdd (readIORef (timer state) >>= displayTime time state >> return True) 100
           boxPackStart metaBox time PackRepel 0

        -- Current slide number
        do slideNum <- labelNew Nothing
           set slideNum [labelAttributes := [AttrSize 0 (negate 1) 60,
                                             AttrForeground 0 (negate 1) (Color 0xffff 0xffff 0xffff)]]
           eventBox <- eventBoxNew
           eventBox `set` [eventBoxVisibleWindow := False]
           eventBox `containerAdd` slideNum
           boxPackStart metaBox eventBox PackNatural 0
           eventBox `on` buttonPressEvent $ tryEvent $
             do DoubleClick <- eventClick; liftIO $ gotoSlideDialog state
           let update = do p <- liftM round $ get (pageAdjustment state) adjustmentValue
                           n <- liftM round $ get (pageAdjustment state) adjustmentUpper
                           slideNum `set` [labelText := "\t" ++ show p ++ "/" ++ show n]
           pageAdjustment state `onValueChanged` update
           pageAdjustment state `onAdjChanged` update

  -- Show windows, fork the rendering thread, and start main loop
  windowListToplevels >>= mapM_ widgetShowAll
  liftIO $ forkIO $ renderThread state
  mainGUI

gotoSlideDialog state = do
  -- TODO: (Just window) -- Without this it isn't model
  dialog <- messageDialogNew Nothing [DialogModal, DialogDestroyWithParent] MessageQuestion ButtonsOkCancel ""
  box <- dialogGetUpper dialog
  pageNum <- pageAdjustment state `get` adjustmentValue
  pageMax <- pageAdjustment state `get` adjustmentUpper
  entry <- spinButtonNewWithRange 1 pageMax 1
  entry `set` [entryAlignment := 1, spinButtonValue := pageNum]
  text <- labelNew (Just $ "Goto slide (1-" ++ show (round pageMax) ++ "):")
  messageDialogSetImage dialog text
  entry `on` entryActivate $ dialogResponse dialog ResponseOk
  boxPackStart box entry PackNatural 0
  widgetShowAll dialog
  r <- dialogRun dialog
  value <- entry `get` spinButtonValue
  when (r == ResponseOk) $ (pageAdjustment state) `set` [adjustmentValue := value]
  putStrLn (show r)
  widgetDestroy dialog

makeWindow state = do
  window <- windowNew
  windowFullscreen window
  widgetModifyBg window StateNormal (Color 0 0 0)
  window `onDestroy` mainQuit
  pageAdjustment state `onValueChanged` widgetQueueDraw window
  pageAdjustment state `onAdjChanged` widgetQueueDraw window -- ensures page counter is updated
  window `on` keyPressEvent $ do
     mods <- eventModifier
     name <- eventKeyName
     liftIO $ handleKey mods (map toLower name)
  return window where
    -- TODO: left and right mouse click for forward and backward
    handleResponce dialog ResponseOk = do
      (uri : _) <- fileChooserGetURIs dialog
      -- Note ordering to maintain thread safety
      () <- atomicWriteIORef (documentURL state) uri
      () <- atomicWriteIORef (document state) Nothing
      -- Pages will be reloaded by render thread
      widgetDestroy dialog
    handleResponce dialog _ = widgetDestroy dialog
    handleKey :: [Modifier] -> String -> IO Bool
    handleKey [Graphics.UI.Gtk.Control] "q" = mainQuit >> return True
    handleKey [] key | key `elem` ["Left", "Up", "Page_Up", "BackSpace"] =
      pageAdjustment state `set` [adjustmentValue :~ (+(negate 1))] >> return True
    handleKey [] key | key `elem` ["Right", "Down", "Page_Down", "Right", "space", "Return"] =
      pageAdjustment state `set` [adjustmentValue :~ (+1)] >> return True
    handleKey [] "home" = pageAdjustment state `set` [adjustmentValue := 0] >> return True
    handleKey [] "end" = pageAdjustment state `get` adjustmentUpper >>= \p -> pageAdjustment state `set` [adjustmentValue := p] >> return True
    handleKey [] "tab" = topNotebook state `set` [notebookCurrentPage :~ (`mod` 2) . (+ 1)] >> return True
    handleKey [] "r" = atomicWriteIORef (document state) Nothing >>= \() -> return True
    handleKey [] "bracketleft" = presenterPaned state `set` [panedPosition :~ max 0 . (+(negate 20))] >> return True
    handleKey [] "bracketright" = presenterPaned state `set` [panedPosition :~ (+20)] >> return True
    handleKey [] "p" = modifyIORef' (timer state) toggleTimer >> return True
    handleKey [] "pause" = modifyIORef' (timer state) toggleTimer >> return True
    handleKey [Shift] "braceleft" = presenterPaned state `set` [panedPosition :~ max 0 . (+(negate 1))] >> return True
    handleKey [Shift] "braceright" = presenterPaned state `set` [panedPosition :~ (+1)] >> return True
    handleKey [Graphics.UI.Gtk.Control] "o" = do
        dialog <- fileChooserDialogNew Nothing Nothing FileChooserActionOpen [(stockOpen, ResponseOk), (stockCancel, ResponseCancel)]
        dialog `on` response $ handleResponce dialog
        widgetShowAll dialog
        return True
    handleKey [Graphics.UI.Gtk.Control] "g" = gotoSlideDialog state >> return True
--fileChooserAddFilter :: FileChooserClass self => self -> FileFilter -> IO ()
    handleKey [] "x" = do doc <- readIORef (document state)
                          case doc of
                            Nothing -> return True
                            Just doc -> panedStops (presenterPaned state) doc >> return True
    handleKey mods name = (putStrLn $ "KeyEvent:"++show mods ++ name) >> return False

panedStops paned document = do
  maxPos <- liftM fromIntegral $ paned `get` panedMaxPosition
  (_, panedHeight) <- widgetGetSize paned
  page <- documentGetPage document 0
  (pageWidth, pageHeight) <- pageGetSize page
  let newPos = (fromIntegral panedHeight) * pageWidth / pageHeight
  paned `set` [panedPosition := maxPos - round newPos]

makeView :: State -> (Int -> Int) -> Bool -> IO DrawingArea
makeView state offset border = do
  area <- drawingAreaNew
  widgetModifyBg area StateNormal (Color 0 0 0)
  area `on` exposeEvent $ tryEvent $ do
    n <- liftIO $ liftM round $ pageAdjustment state `get` adjustmentUpper
    selectedPage <- liftIO $ liftM round $ pageAdjustment state `get` adjustmentValue
    let p = offset selectedPage
    when (p <= n && p >= 1) $ do
      drawWindow <- eventWindow
      size <- liftIO $ widgetGetSize area
      cache' <- liftIO $ readIORef (pages state)
      gc <- liftIO $ gcNew drawWindow
      case Map.lookup p $ Map.findWithDefault Map.empty size cache' of
        Nothing -> do
          pc <- liftIO $ widgetGetPangoContext area
          doc <- liftIO $ readIORef (document state)
          layout <- liftIO $ layoutText pc $
            case doc of
              Nothing -> "Loading file..." -- TODO: filename
              Just _  -> "Rendering slide "++show p++" of "++show n++"..."
          liftIO $ layoutSetAttributes layout [AttrForeground 0 (negate 1) (Color 0xffff 0xffff 0xffff),
                                               AttrWeight 0 (negate 1) WeightBold,
                                               AttrSize 0 (negate 1) 24]
          liftIO $ drawLayout drawWindow gc 0 0 {-x y-} layout
          return ()
        Just pixmap -> do
          (width, height) <- liftIO $ drawableGetSize drawWindow
          (width', height') <- liftIO $ drawableGetSize pixmap
          liftIO $ drawDrawable drawWindow gc pixmap 0 0 ((width - width') `div` 2) ((height - height') `div` 2) width height
          color <- liftIO $ widgetGetStyle area >>= flip styleGetBackground StateSelected
          liftIO $ gcGetValues gc >>= \v -> gcSetValues gc (v { foreground = color, lineWidth = 6 })
          liftIO $ when (border && p == selectedPage) $
                 drawRectangle drawWindow gc False 0 0 width height
  area `on` configureEvent $ tryEvent $ do
    (width, height) <- eventSize
    () <- liftIO $ atomicModifyIORef (views state) (\x -> (Map.insert (castToWidget area) (width, height) x, ()))
    return ()
  return area

------------------------
-- Timer Handling
------------------------

startTimer t@(Counting _) = return $ t
startTimer t@(Paused _) = toggleTimer t

toggleTimer (Counting microseconds) = liftM (Paused . (microseconds-)) getMicroseconds
toggleTimer (Paused microseconds) = liftM (Counting . (microseconds+)) getMicroseconds

getMicroseconds :: IO Integer
getMicroseconds = do
  GTimeVal { gTimeValSec = sec, gTimeValUSec = usec } <- gGetCurrentTime
  return (fromIntegral sec * 1000 * 1000 + fromIntegral usec)

displayTime label state (Paused microseconds) = displayTime' label state True microseconds
displayTime label state (Counting microseconds) = getMicroseconds >>= displayTime' label state False . (microseconds-)
displayTime' :: Label -> State -> Bool -> Integer -> IO ()
displayTime' label state paused microseconds = do
  warning <- readIORef (warningTime state)
  let color | microseconds < 0 = Color 0x8888 0x3333 0xffff {-purple-}
            | microseconds < warning = Color 0xffff 0x0000 0x0000 {-red-}
            | otherwise = Color 0xffff 0xffff 0xffff
  set label [labelText := formatTime microseconds ++ (if paused then " (paused)" else ""),
             labelAttributes := [AttrSize 0 (negate 1) 60, AttrForeground 0 (negate 1) color]]

formatTime :: Integer -> String
formatTime microseconds =
  (if microseconds < 0 then "-" else "") ++
  pad hours ++ ":" ++ pad minutes ++ ":" ++ pad seconds {-++ "." ++ show tenths-} where
    ((((hours, minutes), seconds), tenths), _) = id // 60 // 60 // 10 // (100*1000) $ abs microseconds
    (//) f base val = (f q, r) where (q, r) = val `quotRem` base
    pad x = case show x of
      [c]     -> ['0', c]
      ['-',c] -> ['-', '0', c]
      cs       -> cs

------------------------
-- Render thread
------------------------

-- GTK on windows requires that all GUI operations be done in the GUI thread.
-- We could use an "idle" function for this, but if we don't cancel the
-- function, CPU usage goes to 100%.  Thus we use
-- a separate thread, but still call back to the main thread
-- with postGUISync.

-- TODO: factor out into "PDFDocumentRenderer", .getView, .startRendering, .setDocumentURL, etc.
--       The key being that things that interact with renderThread get encapsulated

renderThread state = sequence_ $ repeat loop where
  loop = do
    threadDelay 1000 -- Keeps the GUI responsive
    documentURL <- readIORef (documentURL state)
    oldDoc <- readIORef (document state)

    -- Note that we do exactly one postGUISync in each branch.  This may improve GUI responsiveness
    (doc, currPage, numPages) <- case oldDoc of
      Just doc -> do (currPages, numPages) <- postGUISync $ do
                       n <- pageAdjustment state `get` adjustmentUpper
                       c <- pageAdjustment state `get` adjustmentValue
                       return (round c, round n)
                     return (doc, currPages, numPages)
      Nothing -> do doc <- catchGError (documentNewFromFile documentURL Nothing)
                             (\x -> putStrLn ("---"++(show x)) >> return Nothing)
                    case doc of
                      Nothing -> error "Exit"
                      Just doc -> do
                        title <- doc `get` documentTitle
                        numPages <- documentGetNPages doc
                        currPage <- postGUISync $ do
                          -- TODO: URL basename if title is empty
                          windowListToplevels >>= mapM (flip windowSetTitle (title ++ " - PDF Presenter"))
                          pageAdjustment state `set` [adjustmentLower := 1, adjustmentUpper := fromIntegral numPages, adjustmentValue :~ (+0)]
                          liftM round $ pageAdjustment state `get` adjustmentValue
                        () <- atomicWriteIORef (pages state) Map.empty
                        () <- atomicWriteIORef (document state) (Just doc)
                        return (doc, currPage, numPages)

    -- Sizes of the views
    views <- readIORef (views state)
    cache <- liftM (Map.filterWithKey (\k a -> k `elem` Map.elems views)) $ readIORef (pages state)
    case [(size,page) |
          page <- sortBy (comparing (\i -> max (i - currPage) (2 * (currPage - i)))) [1..numPages],
          size <- sortBy (flip compare) $ nub $ Map.elems views,
          page `Map.notMember` (Map.findWithDefault Map.empty size cache)] of
      [] -> threadDelay (100 * 1000 {-microseconds-}) -- Avoid a tight idle loop if all pages rendered
      ((width, height), pageNum) : _ -> do -- Render a page
        page <- documentGetPage doc (pageNum-1)
        (docWidth, docHeight) <- pageGetSize page
        postGUISync $ do
          let scaleFactor = min (fromIntegral width  / docWidth)
                                (fromIntegral height / docHeight)
          pixmap <- pixmapNew (Nothing :: Maybe Drawable)
                    (round $ scaleFactor * docWidth) (round $ scaleFactor * docHeight) (Just 24)
          renderWithDrawable pixmap $ do
            scale scaleFactor scaleFactor
            setSourceRGB 1.0 1.0 1.0 >> paint -- draw page background
            pageRender page -- draw page
          -- TODO: filter out unused sizes
          () <- atomicModifyIORef (pages state) (\x -> (Map.unionWith Map.union (Map.singleton (width,height) (Map.singleton pageNum pixmap)) x, ()))
          windowListToplevels >>= mapM_ widgetQueueDraw
