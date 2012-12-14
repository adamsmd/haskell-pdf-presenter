{-# LANGUAGE TupleSections, TemplateHaskell #-}
module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Char (toLower)
import Data.IORef
import Data.List (sortBy, nub)
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Graphics.Rendering.Cairo
--import Graphics.Rendering.Cairo.Types
--import Graphics.Rendering.Pango
import Graphics.UI.Gtk
--import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Poppler.Document
import Graphics.UI.Gtk.Poppler.Page
--import Graphics.UI.Gtk.Poppler.Annot
import System.Console.GetOpt
import System.Exit
import System.Glib
--import System.Process
import Language.Haskell.TH
import System.FilePath (takeFileName)

guiData = $(liftM (LitE . StringL) (runIO $ readFile "presenter.glade"))

-- IF OLD GHC
atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef ref a = do
    x <- atomicModifyIORef ref (\_ -> (a, ()))
    x `seq` return ()

-- Would be useful to have in GHC
modifyIORef' ref f = do
  writeIORef ref =<< f =<< readIORef ref

-- Test multi-monitor on Xfce4

data TimerState = Counting Integer{-ending microseconds-} | Paused Integer{-remaining microseconds-} | Stopped Integer{-remaining microseconds-}
data VideoMuteState = MuteOff | MuteBlack | MuteWhite deriving (Eq)

data State = State
 {
-- Settable on command line
   duration :: IORef Integer{-microseconds-}
 , warning :: IORef Integer{-microseconds-}
 , documentURL :: IORef String -- TODO: Maybe

-- Dynamic State
 , timer :: IORef TimerState -- 'Maybe' for "dont" display or haven't started counting yet? Or as a seperate flag?
 , videoMute :: IORef VideoMuteState
 , fullscreen :: IORef Bool -- TODO: partial full screen? full screen on other?

-- UI State
 , builder :: Builder

-- Render State
 , views :: IORef (Map.Map Widget (Int{-width-}, Int{-height-}))
 , document :: IORef (Maybe Document)
 , pages :: IORef (Map.Map (Int{-width-}, Int{-height-}) (Map.Map Int{-page number-} Pixmap{-cached page-}))
 , pageAdjustment :: Adjustment {- immutable -}
}

setOption opt val = NoArg $ \state -> writeIORef (opt state) val
boolOption opt = OptArg (\val state -> writeIORef (opt state) (read $ fromMaybe "True" val)) -- TODO: yes/no
argOption opt f = ReqArg (\val state -> writeIORef (opt state) (f val))

header = "Usage: PdfViewer [OPTION...] file"
options = [
--  Option "f" ["fullscreen"] (setOption fullScreen True) "Enable full screen"
--, Option "f" ["no-fullscreen"] (setOption fullScreen False) "Enable full screen"
   Option "" ["duration"] (argOption duration (round . (*(60*1000*1000)) . read) "MINUTES") "Timer duration (default 20)" -- TODO: Minute second notation
 , Option "w" ["warning"] (argOption warning (round . (*(60*1000*1000)) . read) "MINUTES") "Warning duration in minutes (default 5)"
 , Option "h?" ["help"] (NoArg (const $ putStr (usageInfo header options) >> exitSuccess)) "Display usage message"
-- TODO: nice message on malformed durration
 , Option "f" ["fullscreen"] (boolOption fullscreen "True/False") "Full screen on startup (default off)"
 , Option "" ["mute-black"] (setOption videoMute MuteBlack) "Start with mute to black"
 , Option "" ["mute-white"] (setOption videoMute MuteBlack) "Start with mute to white"
 , Option "" ["mute-off"] (setOption videoMute MuteBlack) "Start with no muting (default)"
-- overtime, warning, and normal colors
-- metadata font size
-- swap screens
-- show clock
-- show elapsed time
-- show remaining time
-- start record
-- note mode
-- +prerender size (zero means no bound)
-- version
-- preview percentage
-- +columns in preview
-- timer stopped or playing (instead of paused) (what does this do for timer reset?)
 , Option "" ["initial-slide"] (ReqArg (\val state -> pageAdjustment state `set` [adjustmentUpper := read val, adjustmentValue := read val]) "INT") "Initial slide (default 1)"
-- stretch vs scale
 ]
-- TODO: on full screen, pull windows to front

-- TODO: when repaint window, need to repaint all windows (store list of windows in State)

main :: IO ()
main = do
  -- Get program arguments.
  args <- initGUI
  -- Shared state
  state <- return State
    `ap` newIORef (20 * 60 * 1000 * 1000) {-duration-}
    `ap` newIORef (5 * 60 * 1000 * 1000) {-warning-}
    `ap` newIORef (error "internal error: undefined document url")

    `ap` newIORef (error "internal error: undefined timer state")
    `ap` newIORef MuteOff
    `ap` newIORef False

    `ap` builderNew

    `ap` newIORef Map.empty {-views-}
    `ap` newIORef Nothing {-document-}
    `ap` newIORef Map.empty {-pages-}
    `ap` adjustmentNew 0 0 0 1 10 1 {-pageAdjustment-}
  case getOpt Permute options args of
    (opts, [file], []) -> mapM_ ($ state) opts >> guiMain state file
    (_, _, errors) -> putStr (unlines errors) >> putStr (usageInfo header options)

-- ioError (userError "Message")

timerDialog state = do
  dialog <- builderGetObject (builder state) castToDialog "timerDialog"
  [remainingTime, totalTime, warningTime] <-
    mapM (builderGetObject (builder state) castToEntry)
    ["remainingTimeEntry", "totalTimeEntry", "warningTimeEntry"]
  [stopped, paused, running] <-
    mapM (builderGetObject (builder state) castToRadioButton)
    ["timerStatusStoppedRadio", "timerStatusPausedRadio", "timerStatusRunningRadio"]

  duration' <- readIORef (duration state)
  warning' <- readIORef (warning state)
  oldTimer' <- readIORef (timer state)

  -- TODO: huh, the dialog doesn't pop up unless we are paused (I have no idea why)
  modifyTimerState state pauseTimer
  timer' <- readIORef (timer state)
  let timeRemaining = case timer' of Paused t -> t; Stopped t -> t
  remainingTime `set` [entryText := formatTime timeRemaining]
  totalTime `set` [entryText := formatTime duration']
  warningTime `set` [entryText := formatTime warning']
  (case oldTimer' of Stopped _ -> stopped; Paused _ -> paused; Counting _ -> running) `set` [toggleButtonActive := True]
  widgetShowAll dialog
  widgetGrabFocus remainingTime
  r <- dialogRun dialog
  --putStrLn (show r)
  -- TODO: GOOD LORD, this is ugly
  case r of
    ResponseOk -> do
      timer'' <- remainingTime `get` entryText
      duration'' <- totalTime `get` entryText
      warning'' <- warningTime `get` entryText
      case (parseTime timer'', parseTime duration'', parseTime warning'') of
        (Nothing, _, _) -> putStrLn $ "ERR:"++timer''
        (_, Nothing, _) -> putStrLn $ "ERR:"++duration''
        (_, _, Nothing) -> putStrLn $ "ERR:"++warning''
        (Just remain, Just dur, Just warn) -> do
           writeIORef (duration state) dur
           writeIORef (warning state) warn
           [s,p,r] <- mapM (`get` toggleButtonActive) [stopped, paused, running]
           when s $ modifyTimerState state (const $ return $ Stopped remain)
           when p $ modifyTimerState state (const $ return $ Paused remain)
           when r $ modifyTimerState state (const $ return $ Paused remain) >> modifyTimerState state startTimer
    _ -> case oldTimer' of
           Counting _ -> modifyTimerState state startTimer
           _ -> return ()
  widgetHide dialog

-- TODO: ugh, I'd really prefer to use standard code for this, but I couldn't find any
parseTime :: String -> Maybe Integer{-microseconds-}
parseTime ('-' : str) = liftM negate (parseTime str)
parseTime str = time where
  time | Just h <- read' hr, Just m <- read' min, Just s <- read' sec
       = Just $ 1000 * 1000 * (s + 60 * (m + 60 * h))
       | otherwise = Nothing
  read' x | [(n, "")] <- reads x = Just n
          | otherwise = Nothing
  (sec : min : hr : _) = reverse (splitOn ":" str) ++ repeat "0"

mainWindows state =
  mapM (builderGetObject (builder state) castToWindow) ["audienceWindow", "presenterWindow"]

guiMain :: State -> FilePath -> IO ()
guiMain state file = do
  -- Load and setup the GUI
  --builderAddFromFile (builder state) "presenter.glade"
  builderAddFromString (builder state) guiData

  -- Finish setting up the state and configure gui to match state
  writeIORef (documentURL state) ("file://"++file)
  modifyTimerState state (const $ liftM Paused (readIORef (duration state)))
  modifyVideoMute state return

  -- Connect the widgets to their events
  -- * Audience Window
  do window <- builderGetObject (builder state) castToWindow "audienceWindow"
     --configWindow state window
     view <- makeView state id (postDrawVideoMute state)
     window `containerAdd` view
     --widgetShowAll window

  -- * Presenter window
  -- ** Views
  -- *** Previews
  do paned <- builderGetObject (builder state) castToPaned "previewPaned"
     view1 <- makeView state id postDrawNone
     view2 <- makeView state (+1) postDrawNone
     panedPack1 paned view1 True True
     panedPack2 paned view2 True True

  -- *** Thumbnails
  do oldWidth <- newIORef 0
     layout <- builderGetObject (builder state) castToLayout "thumbnailsLayout"
     -- TODO: why are thumbnail renderings delayed?
     layout `onSizeAllocate` recomputeViews oldWidth state
     -- TODO: recomputeViews oldWidth state
     --onZoom $ recocmputeViews

     -- Scroll to keep the current slide visible
     -- TODO: something is wrong with the adjustment
     scrolled <- builderGetObject (builder state) castToScrolledWindow "thumbnailsScrolledWindow"
     adjustment <- scrolledWindowGetVAdjustment scrolled
     let update = do p <- liftM round $ pageAdjustment state `get` adjustmentValue
                     let row = (p-1) `div` columns
                     height <- liftM (heightFromWidth . (`div` columns)) $ readIORef oldWidth
                     adjustmentClampPage adjustment (fromIntegral $ row*height) (fromIntegral $ row*height+height)
     pageAdjustment state `onValueChanged` update
     pageAdjustment state `onAdjChanged` update

  -- ** Meta-data
  -- *** Current time
  do time <- builderGetObject (builder state) castToLabel "timeLabel"
     eventBox <- builderGetObject (builder state) castToEventBox "timeEventBox"
     timeoutAdd (readIORef (timer state) >>= displayTime state >> return True) 100 -- Really this should be run on redraw, but because the computation triggers more redraws, we have it here.  This may result in a 1/10 second lag before the label resizes.
     eventBox `on` buttonPressEvent $ tryEvent $
       do DoubleClick <- eventClick; liftIO $ timerDialog state

  -- *** Current slide number
  do slideNum <- builderGetObject (builder state) castToLabel "slideLabel"
     eventBox <- builderGetObject (builder state) castToEventBox "slideEventBox"
     eventBox `on` buttonPressEvent $ tryEvent $
       do DoubleClick <- eventClick; liftIO $ gotoSlideDialog state
     let update = do p <- liftM round $ pageAdjustment state `get` adjustmentValue
                     n <- liftM round $ pageAdjustment state `get` adjustmentUpper
                     slideNum `set` [labelText := show p ++ "/" ++ show n,
                                     labelAttributes := [AttrSize 0 (negate 1) 60, -- TODO: we have to do this to get the right font???
                                                         AttrForeground 0 (negate 1) white]]
     pageAdjustment state `onValueChanged` update
     pageAdjustment state `onAdjChanged` update

  -- Setup the top-level windows
  do let setupWindow window = do
           --windowFullscreen window
           widgetModifyBg window StateNormal (Color 0 0 0)
           window `onDestroy` mainQuit
           window `on` keyPressEvent $ do
              mods <- eventModifier
              name <- eventKeyName
              b <- liftIO $ handleKey state mods (map toLower name)
              liftIO $ mapM_ widgetQueueDraw =<< mainWindows state
              return b
           -- | Overkill but most of the window has to be redrawn anyway
           pageAdjustment state `onValueChanged` widgetQueueDraw window
           pageAdjustment state `onAdjChanged` widgetQueueDraw window
           widgetShowAll window
     mapM_ setupWindow =<< mainWindows state

  -- Fork the rendering thread, and start main loop
  liftIO $ forkIO $ renderThread state
  mainGUI

-- TODO: remove
heightFromWidth w = w * 3 `div` 4
columns = 4

-- Recompute when the view size changes
recomputeViews oldWidth state (Graphics.UI.Gtk.Rectangle _ _ newWidth _) = do
  oldWidth' <- readIORef oldWidth
  putStrLn "RECOMPUTING VIEWS"
  when (oldWidth' /= newWidth) $ do -- Keep from looping forever
    layout <- builderGetObject (builder state) castToLayout "thumbnailsLayout"
    containerForeach layout (containerRemove layout)
    numPages <- pageAdjustment state `get` adjustmentUpper
    let width = newWidth `div` columns
        height = heightFromWidth width
        numRows = round numPages `div` columns + 1
    putStrLn $ "RECOMPUTING THUMBS" ++ show (numPages, newWidth, width, height, numRows)
    layoutSetSize layout newWidth (height*numRows)
    () <- sequence_ [ do
      let page = columns*row+col+1
      view <- makeView state (const page) (postDrawBorder page state)
      widgetSetSizeRequest view width height
      layoutPut layout view (col*width) (row*height)
      view `widgetAddEvents` [ButtonPressMask, ButtonReleaseMask]
      view `on` buttonReleaseEvent $ tryEvent $
           liftIO $ pageAdjustment state `set` [adjustmentValue := fromIntegral page]
      | row <- [0..numRows-1], col <- [0..3]]
    widgetShowAll layout
    writeIORef oldWidth newWidth
    return ()

gotoSlideDialog state = do
  dialog <- builderGetObject (builder state) castToDialog "pageDialog"
  entry <- builderGetObject (builder state) castToSpinButton "pageDialogSpinButton"
  adjustment <- builderGetObject (builder state) castToAdjustment "pageDialogAdjustment"
  label <- builderGetObject (builder state) castToLabel "pageDialogLabel"
  pageNum <- pageAdjustment state `get` adjustmentValue
  pageMax <- pageAdjustment state `get` adjustmentUpper
  adjustment `set` [adjustmentValue := pageNum, adjustmentUpper := pageMax]
  label `set` [labelText := "Go to slide (1-" ++ show (round pageMax) ++ "):"]
  widgetShowAll dialog
  widgetGrabFocus entry
  r <- dialogRun dialog
  value <- entry `get` spinButtonValue
  when (r == ResponseOk) $ pageAdjustment state `set` [adjustmentValue := value]
  putStrLn (show r)
  widgetHide dialog

-- TODO: left and right mouse click for forward and backward
handleKey :: State -> [Modifier] -> String -> IO Bool
handleKey state [Graphics.UI.Gtk.Control] "q" = mainQuit >> return True
handleKey state [] key | key `elem` ["left", "up", "page_up", "backspace"] =
  pageAdjustment state `set` [adjustmentValue :~ (+(negate 1))] >> return True
handleKey state [] key | key `elem` ["right", "down", "page_down", "right", "space", "return"] =
  pageAdjustment state `set` [adjustmentValue :~ (+1)] >> return True
handleKey state [] "home" = pageAdjustment state `set` [adjustmentValue := 0] >> return True
handleKey state [] "end" = pageAdjustment state `get` adjustmentUpper >>= \p -> pageAdjustment state `set` [adjustmentValue := p] >> return True
handleKey state [] "tab" = builderGetObject (builder state) castToNotebook "presenterWindowNotebook" >>= (`set` [notebookCurrentPage :~ (`mod` 2) . (+ 1)]) >> return True
handleKey state [] "r" = atomicWriteIORef (document state) Nothing >>= \() -> return True
handleKey state [] "p" = modifyTimerState state toggleTimer >> return True
handleKey state [] "pause" = modifyTimerState state toggleTimer >> return True
{-
handleKey state [] "bracketleft" = presenterPaned state `set` [panedPosition :~ max 0 . (+(negate 20))] >> return True
handleKey state [] "bracketright" = presenterPaned state `set` [panedPosition :~ (+20)] >> return True
handleKey state [Shift] "braceleft" = presenterPaned state `set` [panedPosition :~ max 0 . (+(negate 1))] >> return True
handleKey state [Shift] "braceright" = presenterPaned state `set` [panedPosition :~ (+1)] >> return True
-}
handleKey state [Graphics.UI.Gtk.Control] "o" = do
    --dialog <- builderGetObject (builder state) castToFileChooserDialog "fileOpenDialog"
    dialog <- fileChooserDialogNew Nothing Nothing FileChooserActionOpen [(stockOpen, ResponseOk), (stockCancel, ResponseCancel)]
    --dialog `on` response $ handleResponce dialog
    fileChooserSetURI dialog =<< readIORef (documentURL state)
--fileChooserAddFilter
    widgetShowAll dialog
    r <- dialogRun dialog
    when (r == ResponseOk) $ do
      (uri : _) <- fileChooserGetURIs dialog
      -- Note ordering to maintain thread safety
      () <- atomicWriteIORef (documentURL state) uri
      () <- atomicWriteIORef (document state) Nothing
      -- Pages will be reloaded by render thread
      return ()
    widgetDestroy dialog
    return True
handleKey state [Graphics.UI.Gtk.Control] "g" = gotoSlideDialog state >> return True
handleKey state [] "b" = do modifyVideoMute state (\b -> return $ if b == MuteBlack then MuteOff else MuteBlack)
                            return True
handleKey state [] "w" = do modifyVideoMute state (\b -> return $ if b == MuteWhite then MuteOff else MuteWhite)
                            return True
handleKey state [] "f" = do f <- readIORef (fullscreen state)
                            mapM_ (if f then windowUnfullscreen else windowFullscreen) =<< mainWindows state
                            -- TODO: mixed fullscreen?
                            writeIORef (fullscreen state) $ not f
                            return True
handleKey state [] "t" = do t <- readIORef (duration state)
                            modifyTimerState state (const $ return $ Paused t)
                            return True
{-
handleKey state [] "equal" = do doc <- readIORef (document state)
                                case doc of
                                  Nothing -> return True
                                  Just doc -> panedStops (presenterPaned state) doc >> return True
-}
handleKey state mods name = (putStrLn $ "KeyEvent:"++show mods ++ name) >> return False

panedStops paned document = do
  maxPos <- liftM fromIntegral $ paned `get` panedMaxPosition
  (_, panedHeight) <- widgetGetSize paned
  page <- documentGetPage document 0
  (pageWidth, pageHeight) <- pageGetSize page
  let newPos = (fromIntegral panedHeight) * pageWidth / pageHeight
  paned `set` [panedPosition := maxPos - round newPos]

makeView :: State -> (Int -> Int) -> (DrawWindow -> GC -> DrawingArea -> EventM EExpose ()) -> IO DrawingArea
makeView state offset postDraw = do
  area <- drawingAreaNew
  widgetModifyBg area StateNormal black
  area `on` exposeEvent $ tryEvent $ do
    n <- liftIO $ liftM round $ pageAdjustment state `get` adjustmentUpper
    p <- liftIO $ liftM (offset . round) $ pageAdjustment state `get` adjustmentValue
    when (p <= n && p >= 1) $ do -- Check ensures that preview is no rendered when on last slide
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
      postDraw drawWindow gc area
  area `on` configureEvent $ tryEvent $ do
    (width, height) <- eventSize
    () <- liftIO $ atomicModifyIORef (views state) (\x -> (Map.insert (castToWidget area) (width, height) x, ()))
    return ()
  return area

postDrawNone _ _ _ = return ()
postDrawBorder p state drawWindow gc area = do
  p' <- liftIO $ liftM round $ pageAdjustment state `get` adjustmentValue
  when (p == p') $ liftIO $ do
    color <- widgetGetStyle area >>= flip styleGetBackground StateSelected
    gcGetValues gc >>= \v -> gcSetValues gc (v { foreground = color, lineWidth = 6 })
    (width, height) <- drawableGetSize drawWindow
    drawRectangle drawWindow gc False 0 0 width height
postDrawVideoMute state drawWindow gc area = do
  videoMute' <- liftIO $ readIORef (videoMute state)
  case videoMute' of
    MuteOff -> return ()
    MuteBlack -> liftIO $ renderWithDrawable drawWindow (setSourceRGB 0.0 0.0 0.0 >> paint)
    MuteWhite -> liftIO $ renderWithDrawable drawWindow (setSourceRGB 1.0 1.0 1.0 >> paint)

------------------------
-- Timer Handling
------------------------

startTimer t@(Counting _) = return $ t
startTimer t@(Paused _) = toggleTimer t

toggleTimer (Counting microseconds) = liftM (Paused . (microseconds-)) getMicroseconds
toggleTimer (Paused microseconds) = liftM (Counting . (microseconds+)) getMicroseconds
toggleTimer (Stopped microseconds) = liftM (Counting . (microseconds-)) getMicroseconds

pauseTimer t@(Counting _) = toggleTimer t
pauseTimer t = return t

getMicroseconds :: IO Integer
getMicroseconds = do
  GTimeVal { gTimeValSec = sec, gTimeValUSec = usec } <- gGetCurrentTime
  return (fromIntegral sec * 1000 * 1000 + fromIntegral usec)

displayTime state (Paused microseconds) = displayTime' state True microseconds
displayTime state (Counting microseconds) = getMicroseconds >>= displayTime' state False . (microseconds-)
displayTime' :: State -> Bool -> Integer -> IO ()
displayTime' state paused microseconds = do
  warning' <- readIORef (warning state)
  let color | microseconds < 0 = overtimeColor
            | microseconds < warning' = warningColor
            | otherwise = white
  -- Set the hidden label so we maintain a minimum size
  do label2 <- builderGetObject (builder state) castToLabel "timeLabel2"
     set label2 [labelText := formatTime microseconds,
                 labelAttributes := [AttrSize 0 (negate 1) 60, --(fromIntegral minHeight),
                                     AttrForeground 0 (negate 1) color]]
-- TODO: Why is AttrSize a different font than what the GUI builder makes?

  -- Initially set the size based on height
  label <- builderGetObject (builder state) castToLabel "timeLabel"
  (w, h) <- widgetGetSize label
  set label [labelText := formatTime microseconds,
             labelAttributes := [AttrAbsSize 0 (negate 1) (fromIntegral h),
                                 AttrForeground 0 (negate 1) color]]

  -- Then rescale the text to ensure it is not too wide
  (_, PangoRectangle _ _ iw _) <- layoutGetExtents =<< labelGetLayout label
  let h' = h `min` (w * h `div` round iw)
  set label [labelText := formatTime microseconds,
             labelAttributes := [AttrAbsSize 0 (negate 1) (fromIntegral h'),
                                 AttrForeground 0 (negate 1) color]]

overtimeColor = Color 0x8888 0x3333 0xffff {-purple-}
warningColor = Color 0xffff 0x0000 0x0000 {-red-}
white = Color 0xffff 0xffff 0xffff
black = Color 0x0000 0x0000 0x0000

formatTime :: Integer -> String
formatTime microseconds =
  (if microseconds < 0 then "-" else "") ++
  show hours ++ ":" ++ pad minutes ++ ":" ++ pad seconds {-++ "." ++ show tenths-} where
    ((((hours, minutes), seconds), tenths), _) = id // 60 // 60 // 10 // (100*1000) $ abs microseconds
    (//) f base val = (f q, r) where (q, r) = val `quotRem` base
    pad x = case show x of
      [c]     -> ['0', c]
      ['-',c] -> ['-', '0', c]
      cs       -> cs

-- TODO: we are we recomputing even when the sizes didn't change?
-- TODO: why do thumnails recompute late?
modifyVideoMute state f = do
  mute <- f =<< readIORef (videoMute state)
  builderGetObject (builder state) castToLabel "videoMuteStatusLabel" >>= (`set` case mute of
    MuteOff -> [labelText := "\x2600",
                labelAttributes := [AttrSize 0 (negate 1) 30, AttrForeground 0 (negate 1) black]]
    MuteWhite -> [labelText := "\x2600",
                  labelAttributes := [AttrSize 0 (negate 1) 30, AttrForeground 0 (negate 1) white]]
    MuteBlack -> [labelText := "\x263C",
                  labelAttributes := [AttrSize 0 (negate 1) 30, AttrForeground 0 (negate 1) white]])
  writeIORef (videoMute state) mute

modifyTimerState state f = do
  timerState <- f =<< readIORef (timer state)
  builderGetObject (builder state) castToLabel "timerStatusLabel" >>= (`set` case timerState of
    Paused _ -> [labelText := "\x25AE\x25AE",
                 labelAttributes := [AttrSize 0 (negate 1) 30, AttrForeground 0 (negate 1) white]]
    Counting _ -> [labelText := "\x25B6",
                   labelAttributes := [AttrSize 0 (negate 1) 30, AttrForeground 0 (negate 1) black]])
  writeIORef (timer state) timerState

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
    threadDelay 1000{-microseconds-} -- Keeps the GUI responsive
    documentURL <- readIORef (documentURL state)
    oldDoc <- readIORef (document state)

    -- Note that in an effort to improve GUI responsiveness, we do exactly one postGUISync in each branch.
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
                          windowListToplevels >>= mapM (flip windowSetTitle ((if null title then takeFileName documentURL else title) ++ " - PDF Presenter"))
                          pageAdjustment state `set` [adjustmentLower := 1, adjustmentUpper := fromIntegral numPages, adjustmentValue :~ (+0)]
                          liftM round $ pageAdjustment state `get` adjustmentValue
                        () <- atomicWriteIORef (pages state) Map.empty
                        () <- atomicWriteIORef (document state) (Just doc)
                        return (doc, currPage, numPages)

    -- Sizes of the views
    views <- readIORef (views state)
    cache <- liftM (Map.filterWithKey (\k _ -> k `elem` Map.elems views)) $ readIORef (pages state)
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
          mainWindows state >>= mapM_ widgetQueueDraw
