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


-- --help

-- Text multi-monitor on Xfce4

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

data TimerState = Counting GTimeVal | Paused Int{-seconds-}

data Blanking = BlankNone | BlankBlack | BlankWhite
data State = State
 { endTime :: IORef GTimeVal -- 'Maybe' for "dont" display or haven't started counting yet? Or as a seperate flag?
 , blanking :: IORef Blanking
 , documentURL :: IORef String -- TODO: Maybe
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
  state <- do
    page <- adjustmentNew 0 0 0 1 10 1
    top <- notebookNew -- TODO: belongs in state
    presenter <- hPanedNew -- TODO: belongs in state
    blankingRef <- newIORef BlankNone
    endTimeRef <- newIORef =<< gGetCurrentTime
    documentURLRef <- newIORef ("file://"++file)
    viewsRef <- newIORef Map.empty
    documentRef <- newIORef Nothing
    pagesRef <- newIORef Map.empty
    return $ State { blanking = blankingRef, endTime = endTimeRef, documentURL = documentURLRef
                   , views = viewsRef, document = documentRef, pages = pagesRef
                   , pageAdjustment = page, topNotebook = top, presenterPaned = presenter }

{-
  Just display <- displayGetDefault
  nscreens <- displayGetNScreens display
  putStrLn $ "NScreens:"++show nscreens
  Just screen <- screenGetDefault
  nmonitors <- screenGetNMonitors screen
  putStrLn $ "NMonitors:"++show nmonitors
  --Rectangle x y width height <- screenGetMonitorGeometry screen 1
-}

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
-- NOTE: for some odd reason, this crashes if we add the scrolled second
     do do scrolled <- scrolledWindowNew Nothing Nothing
           --windowBox `containerAdd` scrolled
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

        do --presenter <- vBoxNew False 0
           --boxPackStart windowBox presenter PackGrow 0
           notebookAppendPage (topNotebook state) (presenterPaned state) "1"
           view1 <- makeView state id False
           view2 <- makeView state (+1) False
           panedPack1 (presenterPaned state) view1 True True
           panedPack2 (presenterPaned state) view2 True True

        return ()

     -- Meta-data
     do metaBox <- hBoxNew False 0
        boxPackEnd windowBox metaBox PackNatural 0

        -- Current time
        time <- labelNew Nothing
        set time [labelAttributes := [AttrSize 0 (negate 1) 40]]
        timeoutAdd (readIORef (endTime state) >>= displayTime time >> return True) 100
        boxPackStart metaBox time PackRepel 0

        -- Current slide number
        slideNum <- entryNew
        slideNum `set` [entryHasFrame := False]
        pc <- widgetGetPangoContext slideNum
        fd <- contextGetFontDescription pc
        fontDescriptionSetSize fd 40
        widgetModifyFont slideNum (Just fd)
        widgetModifyBase slideNum StateNormal (Color 0 0 0)
        entrySetAlignment slideNum 1
        --entrySetNumeric slideNum True
        widgetModifyBg slideNum StateNormal (Color 0 0 0)
        boxPackStart metaBox slideNum PackNatural 0
        --page `onValueChanged` (get page adjustmentValue >>= entrySetText slideNum . show . round)
        slideNum `set` [widgetCanFocus := False, entryEditable := False]
        --slideNum `onEditableChanged` (slideNum `set` [entryText :~ filter (isDigit)])
{-
        slideNum `onInsertText` \s p -> if False && all isDigit s
                                        then editableInsertText slideNum s p >> return (length s + p)
                                        else return p
-}
        slideNum `on` buttonReleaseEvent $ lift $ do
          dialog <- messageDialogNew (Just window) [DialogModal, DialogDestroyWithParent] MessageQuestion ButtonsOkCancel ""
          box <- dialogGetUpper dialog
          entry <- spinButtonNewWithRange 1 95 1 -- TODO: right align
          text <- labelNew (Just "Goto slide (of 95):") -- TODO: "of"
          messageDialogSetImage dialog text
          entry `on` entryActivate $ dialogResponse dialog ResponseOk
          boxPackStart box entry PackNatural 0
          widgetShowAll dialog
          r <- dialogRun dialog
          value <- entry `get` spinButtonValue
          putStrLn $ "V:" ++ show value
          when (r == ResponseOk) $ (pageAdjustment state) `set` [adjustmentValue := value]
          putStrLn (show r)
          widgetDestroy dialog
          return False
        
        let update = do p <- liftM round $ get (pageAdjustment state) adjustmentValue
                        n <- liftM round $ get (pageAdjustment state) adjustmentUpper
                        slideNum `set` [entryText := show p ++ "/" ++ show n]
          in do (pageAdjustment state) `onValueChanged` update
                (pageAdjustment state) `onAdjChanged` update

  -- Show windows, fork the rendering thread, and start main loop
  windowListToplevels >>= mapM_ widgetShowAll
  liftIO $ forkIO $ renderThread state
  mainGUI

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
     liftIO $ handleKey mods name
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
    handleKey [] "Home" = pageAdjustment state `set` [adjustmentValue := 0] >> return True
    handleKey [] "End" = pageAdjustment state `get` adjustmentUpper >>= \p -> pageAdjustment state `set` [adjustmentValue := p] >> return True
    handleKey [] "Tab" = topNotebook state `set` [notebookCurrentPage :~ (`mod` 2) . (+ 1)] >> return True
    handleKey [] "r" = atomicWriteIORef (document state) Nothing >>= \() -> return True
    handleKey [] "bracketleft" = presenterPaned state `set` [panedPosition :~ max 0 . (+(negate 20))] >> return True
    handleKey [] "bracketright" = presenterPaned state `set` [panedPosition :~ (+20)] >> return True
    handleKey [Shift] "braceleft" = presenterPaned state `set` [panedPosition :~ max 0 . (+(negate 1))] >> return True
    handleKey [Shift] "braceright" = presenterPaned state `set` [panedPosition :~ (+1)] >> return True
    handleKey [Graphics.UI.Gtk.Control] "o" = do
        dialog <- fileChooserDialogNew Nothing Nothing FileChooserActionOpen [(stockOpen, ResponseOk), (stockCancel, ResponseCancel)]
        dialog `on` response $ handleResponce dialog
        widgetShowAll dialog
--fileChooserAddFilter :: FileChooserClass self => self -> FileFilter -> IO ()
        return True
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
          liftIO $ layoutSetAttributes layout [AttrForeground 0 (negate 1) (Color 65535 65535 65535),
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

formatTime :: GTimeVal -> GTimeVal -> String
formatTime (GTimeVal { gTimeValSec = sec1, gTimeValUSec = usec1 })
           (GTimeVal { gTimeValSec = sec2, gTimeValUSec = usec2 }) =
  (if time1 - time2 < 0 then "-" else "") ++
  pad hours ++ ":" ++ pad minutes ++ ":" ++ pad seconds ++ "." ++ show tenths where
    time1 = sec1 * 1000 * 1000 + usec1
    time2 = sec2 * 1000 * 1000 + usec2
    ((((hours, minutes), seconds), tenths), _) = id // 60 // 60 // 10 // (100*1000) $ abs (time1 - time2)
    (//) f base val = (f q, r) where (q, r) = val `quotRem` base
    pad x = case show x of
      [c]     -> ['0', c]
      ['-',c] -> ['-', '0', c]
      cs       -> cs

--displayTime label {-Nothing-} = set label [labelText := "00:00:00"]
displayTime label ({-Just-} time) = do
  time' <- gGetCurrentTime
  --when gTimeValAdd time (5*60) > time' $ orange
  --when time > time' $ red -- flashing?
  set label [labelText := formatTime time time'
             --labelAttributes := [pangoattrib]
            ]

-- GTK on windows requires that all GUI operations be done in the GUI thread.
-- We could use an "idle" function for this, but if we don't cancel the
-- function, CPU usage goes to 100%.  Thus we use
-- a separate thread, but still call back to the main thread
-- with postGUISync.

--renderThread :: Adjustment -> HPaned -> ThreadingInfo -> IORef State -> IO ()
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

    --postGUISync $ panedStops paned doc
    --x <- widgetGetSize t
    --putStrLn $ "FOO:" ++ show x

    -- Sizes of the views
    views <- readIORef (views state)
    cache <- liftM (Map.filterWithKey (\k a -> k `elem` Map.elems views)) $ readIORef (pages state)
    case [(size,page) |
          page <- sortBy (comparing (\i -> max (i - currPage) (2 * (currPage - i)))) [1..numPages],
          size <- sortBy (flip compare) $ nub $ Map.elems views,
          page `Map.notMember` (Map.findWithDefault Map.empty size cache)] of
      [] -> threadDelay (100 * 1000) -- Avoid a tight idle loop if all pages rendered
      ((width, height), pageNum) : _ -> do -- Render a page
        page <- documentGetPage doc (pageNum-1)
        (docWidth, docHeight) <- pageGetSize page
        --putStrLn "ANNOTS"
        --annotMappings <- pageGetAnnotMapping page
        --mapM (\(PopplerAnnotMapping area annot) -> do
        --        putStrLn $ show area
        --        putStrLn "BEGIN"
        --        text <- annotGetContents annot
        --        putStrLn "MID"
        --        putStrLn $ show text
        --        putStrLn "END"
        --        ) annotMappings

        postGUISync $ do
          let scaleFactor = min (fromIntegral width  / docWidth)
                                (fromIntegral height / docHeight)
          pixmap <- pixmapNew (Nothing :: Maybe Drawable)
                    (round $ scaleFactor * docWidth) (round $ scaleFactor * docHeight) (Just 24)
          renderWithDrawable pixmap $ do
            scale scaleFactor scaleFactor
            setSourceRGB 1.0 1.0 1.0 >> paint -- page background
            pageRender page -- draw page
          time' <- gGetCurrentTime
          --putStrLn $ "SETTING:"++show(width,height,pageNum, formatTime time' (endTime state'))
          -- TODO: filter out unused sizes
          () <- atomicModifyIORef (pages state) (\x -> (Map.unionWith Map.union (Map.singleton (width,height) (Map.singleton pageNum pixmap)) x, ()))
          windowListToplevels >>= mapM_ widgetQueueDraw


--windowPresent :: WindowClass self => self -> IO ()
--windowSetScreen
--windowScreen :: WindowClass self => Attr self Screen
  --size <- drawableGetSize (castToDrawable window)
  --Just screen <- screenGetDefault
  --Rectangle _ _ width height <- screenGetMonitorGeometry screen 0
--widgetSetSizeRequest area (truncate width) (truncate height)
--windowSetPosition window WinPosCenter
