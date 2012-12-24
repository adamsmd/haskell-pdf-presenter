{-# LANGUAGE TupleSections, TemplateHaskell #-}
module Main where

import Control.Monad
import Data.Char (toLower)
import Data.IORef
import Data.List (sort, sortBy, nub)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Poppler.Document
import Graphics.UI.Gtk.Poppler.Page
import Language.Haskell.TH
import System.Console.GetOpt
import System.Exit (exitSuccess)
import System.Glib
import System.FilePath (takeFileName)

import Foreign.Ptr (castPtr)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Unsafe as BSU
import Codec.Compression.Zlib.Raw

guiData = $(liftM (LitE . StringL) (runIO $ readFile "presenter.glade"))

-- Test multi-monitor on Xfce4

data TimerState = Counting Integer{-ending microseconds-} | Paused Integer{-remaining microseconds-} | Stopped Integer{-remaining microseconds-}
data VideoMuteState = MuteOff | MuteBlack | MuteWhite deriving (Eq)

data State = State
 {
-- Settable on command line
   duration :: IORef Integer{-microseconds-}
 , warning :: IORef Integer{-microseconds-}
 , documentURL :: IORef String -- TODO: Maybe
 , compression :: IORef Int

-- Dynamic State
 , timer :: IORef TimerState -- 'Maybe' for "dont" display or haven't started counting yet? Or as a seperate flag?
 , videoMute :: IORef VideoMuteState
 , fullscreen :: IORef Bool -- TODO: partial full screen? full screen on other?
 , mouseTimeout :: IORef Integer

-- UI State
 , builder :: Builder

-- Render State
 , views :: IORef (Map.Map Widget (Int{-width-}, Int{-height-}))
 , document :: IORef (Maybe Document)
 , pages :: IORef (Map.Map (Int{-page number-}, Int{-width-}, Int{-height-})
                          (BSL.ByteString, Int{-width-}, Int{-height-}, Int{-stride-}))
 , pageAdjustment :: Adjustment
}

setOption opt val = NoArg $ \state -> writeIORef (opt state) val
boolOption opt = OptArg (\val state -> writeIORef (opt state) (read $ fromMaybe "True" val)) -- TODO: yes/no
argOption opt f = ReqArg (\val state -> writeIORef (opt state) (f val))

header = "Usage: PdfViewer [OPTION...] file"
options = [
   Option "" ["duration"] (argOption duration (round . (*(60*1000*1000::Double)) . read) "MINUTES") "Timer duration (default 0)" -- TODO: Minute second notation
 , Option "w" ["warning"] (argOption warning (round . (*(60*1000*1000::Double)) . read) "MINUTES") "Warning duration in minutes (default 5)"
 , Option "h?" ["help"] (NoArg (const $ putStr (usageInfo header options) >> exitSuccess)) "Display usage message"
-- TODO: nice message on malformed durration
 , Option "f" ["fullscreen"] (boolOption fullscreen "True/False") "Full screen on startup (default off)"
 , Option "" ["mute-black"] (setOption videoMute MuteBlack) "Start with mute to black"
 , Option "" ["mute-white"] (setOption videoMute MuteBlack) "Start with mute to white"
 , Option "" ["mute-off"] (setOption videoMute MuteBlack) "Start with no muting (default)"
{- TODO:
 , Option "" ["timer-paused"] (setOption timer (Paused (error "internal error: paused"))) "Start with timer paused (default)"
 , Option "" ["timer-running"] (setOption timer (Stopped (error "internal error: paused"))) "Start with timer running"
 , Option "" ["timer-stopped"] (setOption timer (Counting (error "internal error: paused"))) "Start with timer stopped"
-}
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
 , Option "" ["compression"] (ReqArg (\val state -> writeIORef (compression state) (read val) {-TODO: 0-9-}) "INT") "Compression level, 0=none,1=fastest,9=best (default 1)"
-- stretch vs scale
-- ++, Option "" ["anti-screensaver"] "always", "never", "fullscreen-only" (default = fullscreen only)
 ]
-- TODO: on full screen, pull windows to front
-- TODO: option parsing (e.g. read shouldn't exit the program when can't parse)
-- TODO: when repaint window, need to repaint all windows (store list of windows in State)

main :: IO ()
main = do
  -- Get program arguments.
  args <- initGUI
  -- Shared state
  state <- return State
    `ap` newIORef (0) {-20 * 60 * 1000 * 1000-} {-duration-}
    `ap` newIORef (5 * 60 * 1000 * 1000) {-warning-}
    `ap` newIORef "" {-file uri-}
    `ap` newIORef 1 {-compression-}

    `ap` newIORef (error "internal error: undefined timer state")
    `ap` newIORef MuteOff {-videoMute-}
    `ap` newIORef False {-fullscreen-}
    `ap` newIORef 0 {-mouseTimeout-}

    `ap` builderNew

    `ap` newIORef Map.empty {-views-}
    `ap` newIORef Nothing {-document-}
    `ap` newIORef Map.empty {-pages-}
    `ap` adjustmentNew 0 0 0 1 10 1 {-pageAdjustment-}
  case getOpt Permute options args of
    (opts, [], []) -> mapM_ ($ state) opts >> guiMain state
    (opts, [file], []) -> mapM_ ($ state) opts >> postGUIAsync (openDoc state ("file://"++file) >> return ()) >> guiMain state
    (_, [_,_], []) -> putStrLn ("Error: Multiple files on command line") >> putStr (usageInfo header options)
    (_, _, errors) -> putStr (unlines errors) >> putStr (usageInfo header options)

guiMain :: State -> IO ()
guiMain state = do
  -- Load and setup the GUI
  builderAddFromString (builder state) guiData

  -- Finish setting up the state and configure gui to match state
  modifyTimerState state (const $ liftM Paused (readIORef (duration state)))
  modifyVideoMute state return

  -- Connect the widgets to their events
  -- * Audience Window
  do window <- builderGetObject (builder state) castToWindow "audienceWindow"
     view <- makeView state id (postDrawVideoMute state)
     window `containerAdd` view

  -- * Presenter window
  -- ** Views
  -- *** Previews
  do paned <- builderGetObject (builder state) castToPaned "preview.paned"
     view1 <- makeView state id postDrawNone
     view2 <- makeView state (+1) postDrawNone
     panedPack1 paned view1 True True
     panedPack2 paned view2 True True

  -- *** Thumbnails
  do oldWidth <- newIORef 0
     layout <- builderGetObject (builder state) castToLayout "thumbnails.layout"
     layout `onSizeAllocate` (\(Graphics.UI.Gtk.Rectangle _ _ newWidth _) -> do
       oldWidth' <- readIORef oldWidth
       writeIORef oldWidth newWidth
       when (oldWidth' /= newWidth) $ recomputeViews state newWidth)
     pageAdjustment state `onAdjChanged` (readIORef oldWidth >>= recomputeViews state)
     --onZoom $ recocmputeViews

     -- Scroll to keep the current slide visible
     -- TODO: something is wrong with the adjustment
     scrolled <- builderGetObject (builder state) castToScrolledWindow "thumbnails.scrolled"
     adjustment <- scrolledWindowGetVAdjustment scrolled
     let update = do p <- liftM round $ pageAdjustment state `get` adjustmentValue
                     let row = (p-1) `div` columns
                     height <- liftM (heightFromWidth . (`div` columns)) $ readIORef oldWidth
                     adjustmentClampPage adjustment (fromIntegral $ row*height) (fromIntegral $ row*height+height)
     pageAdjustment state `onValueChanged` update
     pageAdjustment state `onAdjChanged` update

  -- ** Meta-data
  -- *** Current time
  do eventBox <- builderGetObject (builder state) castToEventBox "timeEventBox"
     timeoutAdd (readIORef (timer state) >>= displayTime state >> return True) 100{-milliseconds-} -- Really this should be run on redraw, but because the computation triggers more redraws, we have it here.  This may result in a 1/10 second lag before the label resizes.
     eventBox `on` buttonPressEvent $ tryEvent $
       do DoubleClick <- eventClick; liftIO $ timerDialog state

  -- *** Current slide number
  do slideNum <- builderGetObject (builder state) castToLabel "slideLabel"
     eventBox <- builderGetObject (builder state) castToEventBox "slideEventBox"
     eventBox `on` buttonPressEvent $ tryEvent $
       do DoubleClick <- eventClick; liftIO $ gotoSlideDialog state
     let update = do p <- liftM round $ pageAdjustment state `get` adjustmentValue
                     n <- liftM round $ pageAdjustment state `get` adjustmentUpper
                     slideNum `set` [labelText := show (p :: Integer) ++ "/" ++ show (n :: Integer),
                                     labelAttributes := [AttrSize 0 (negate 1) 60, -- TODO: we have to do this to get the right font???
                                                         AttrForeground 0 (negate 1) white]]
     pageAdjustment state `onValueChanged` update
     pageAdjustment state `onAdjChanged` update

  -- Setup the top-level windows
  do let setupWindow window = do
           f <- readIORef (fullscreen state)
           when f $ windowFullscreen window
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

  -- Setup about dialog
  do d <- builderGetObject (builder state) castToAboutDialog "aboutDialog"
     d `on` response $ const $ widgetHideAll d
     d `on` deleteEvent $ liftIO (widgetHideAll d) >> return True

  -- Make sure the screen saver doesn't start by moving the cursor
  -- Since we move the mouse by zero pixels, this shouldn't effect the user
  w <- builderGetObject (builder state) castToWindow "audienceWindow"
  c <- cursorNew BlankCursor
  dw <- widgetGetDrawWindow w
  timeoutAdd (do d <- screenGetDisplay =<< windowGetScreen  w
                 (screen, _, x, y) <- displayGetPointer d
                 displayWarpPointer d screen x y
                 t <- readIORef (mouseTimeout state)
                 t' <- getMicroseconds
                 f <- readIORef (fullscreen state)
                 when (f && t' > t) $ dw `drawWindowSetCursor` (Just c)
                 return True) (5000{-milliseconds-})
  w `widgetAddEvents` [PointerMotionMask]
  oldCoordRef <- newIORef (0, 0)
  let f = do dw `drawWindowSetCursor` Nothing
             writeIORef (mouseTimeout state) =<< liftM (+ 3 * 1000 * 1000) getMicroseconds
  w `on` enterNotifyEvent $ liftIO f >> return False
  w `on` motionNotifyEvent $ do
        coord <- eventRootCoordinates
        oldCoord <- liftIO $ readIORef oldCoordRef
        liftIO $ writeIORef oldCoordRef coord
        when (oldCoord /= coord) $ liftIO f
        return False

  -- Fork the rendering thread, and start main loop
  --liftIO $ forkIO $ renderThread state
  renderThreadSoon state --timeoutAdd (renderThread' state >> return False) 1{-milliseconds-}
  mainGUI

-- Recompute when the view size changes
recomputeViews state newWidth = do -- Keep from looping forever
  layout <- builderGetObject (builder state) castToLayout "thumbnails.layout"
  widgetModifyBg layout StateNormal black -- TODO: really should do this earlier
  containerForeach layout (containerRemove layout)
  numPages <- pageAdjustment state `get` adjustmentUpper
  let width = newWidth `div` columns
      height = heightFromWidth width
      numRows = round numPages `div` columns + 1
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

mainWindows state =
  mapM (builderGetObject (builder state) castToWindow) ["presenterWindow", "audienceWindow"]

-- TODO: remove
heightFromWidth w = w * 3 `div` 4
columns = 4

------------------------
-- User Event handling
------------------------

-- TODO: left and right mouse click for forward and backward
handleKey :: State -> [Modifier] -> String -> IO Bool
handleKey state [Graphics.UI.Gtk.Control] "q" = mainQuit >> return True
handleKey state _ "h" = builderGetObject (builder state) castToAboutDialog "aboutDialog" >>= widgetShowAll >> return True
handleKey state _ "question" = builderGetObject (builder state) castToAboutDialog "aboutDialog" >>= widgetShowAll >> return True
handleKey state [] key | key `elem` ["left", "up", "page_up", "backspace"] =
  gotoPageAndUnpauseTimer state (+(negate 1)) >> return True
handleKey state [] key | key `elem` ["right", "down", "page_down", "right", "space", "return"] =
  gotoPageAndUnpauseTimer state (+1) >> return True
handleKey state [] "home" =
  pageAdjustment state `set` [adjustmentValue := 0] >> return True
handleKey state [] "end" =
  pageAdjustment state `get` adjustmentUpper >>= \p -> pageAdjustment state `set` [adjustmentValue := p] >> return True
handleKey state [] "tab" =
  builderGetObject (builder state) castToNotebook "presenter.notebook" >>= (`set` [notebookCurrentPage :~ (`mod` 2) . (+ 1)]) >> return True
handleKey state [Graphics.UI.Gtk.Control] "r" = readIORef (documentURL state) >>= openDoc state >> return True
handleKey state [] "p" = modifyTimerState state togglePauseTimer >> return True
handleKey state [Control] "p" = modifyTimerState state toggleStopTimer >> return True
handleKey state [] "pause" = modifyTimerState state togglePauseTimer >> return True
handleKey state [] "bracketleft" = builderGetObject (builder state) castToPaned "preview.paned" >>= (`set` [panedPosition :~ max 0 . (+(negate 20))]) >> return True
handleKey state [] "bracketright" = builderGetObject (builder state) castToPaned "preview.paned" >>= (`set` [panedPosition :~ (+20)]) >> return True
handleKey state [Shift] "braceleft" = builderGetObject (builder state) castToPaned "preview.paned" >>= (`set` [panedPosition :~ max 0 . (+(negate 1))]) >> return True
handleKey state [Shift] "braceright" = builderGetObject (builder state) castToPaned "preview.paned" >>= (`set` [panedPosition :~ (+1)]) >> return True
handleKey state [] "equal" = panedStops state >> return True
handleKey state [] "b" = modifyVideoMute state (\b -> return $ if b == MuteBlack then MuteOff else MuteBlack) >> return True
handleKey state [] "w" = modifyVideoMute state (\b -> return $ if b == MuteWhite then MuteOff else MuteWhite) >> return True
handleKey state [] "escape" = do f <- readIORef (fullscreen state); when f (toggleFullScreen state); return True
handleKey state [] "f" = toggleFullScreen state >> return True
handleKey state [] "f11" = toggleFullScreen state >> return True
handleKey state [Alt] "return" = toggleFullScreen state >> return True
handleKey state [Control] "l" = toggleFullScreen state >> return True
handleKey state [] "t" = do t <- readIORef (duration state)
                            modifyTimerState state (const $ return $ Paused t)
                            return True
handleKey state [Graphics.UI.Gtk.Control] "t" = timerDialog state >> return True
handleKey state [Graphics.UI.Gtk.Control] "g" = gotoSlideDialog state >> return True
handleKey state [Graphics.UI.Gtk.Control] "o" = openFileDialog state >> return True
handleKey state mods name = (putStrLn $ "KeyEvent:"++show mods ++ name) >> return False

gotoPageAndUnpauseTimer state f =
  pageAdjustment state `set` [adjustmentValue :~ f] >> modifyTimerState state unpauseTimer

toggleFullScreen state = do
  f <- readIORef (fullscreen state)
  mapM_ (if f then windowUnfullscreen else windowFullscreen) =<< mainWindows state
  -- TODO: mixed fullscreen?
  writeIORef (fullscreen state) $ not f

-- full left, fit right, 1/3, centered, 2/3, fit left, full right
panedStops state = do
  paned <- builderGetObject (builder state) castToPaned "preview.paned"
  maxPos <- liftM fromIntegral $ paned `get` panedMaxPosition
  oldPos <- liftM fromIntegral $ paned `get` panedPosition
  (_, panedHeight) <- widgetGetSize paned
  p <- liftM round $ pageAdjustment state `get` adjustmentValue
  pLast <- liftM round $ pageAdjustment state `get` adjustmentUpper
  document <- readIORef (document state)
  stops <- case document of
    Nothing -> return []
    Just document -> do
      (pageWidth1, pageHeight1) <- pageGetSize =<< documentGetPage document (p - 1)
      (pageWidth2, pageHeight2) <- pageGetSize =<< documentGetPage document (p `min` pLast - 1)
      return [round (fromIntegral panedHeight * pageWidth1 / pageHeight1),
              maxPos - round (fromIntegral panedHeight * pageWidth2 / pageHeight2)]
  let newPos = case sort $ filter (> oldPos) $ stops ++ [maxPos `div` 3, maxPos `div` 2, maxPos * 2 `div` 3, maxPos] of
              [] -> 0
              (x:_) -> x
  paned `set` [panedPosition := newPos]

timerDialog state = do
  dialog <- builderGetObject (builder state) castToDialog "timerDialog"
  [remainingTime, totalTime, warningTime] <-
    mapM (builderGetObject (builder state) castToEntry)
    ["remainingTimeEntry", "totalTimeEntry", "warningTimeEntry"]
  [stopped, paused, running] <-
    mapM (builderGetObject (builder state) castToRadioButton)
    ["timerStatusStoppedRadio", "timerStatusPausedRadio", "timerStatusRunningRadio"]

  putStrLn "timerDialog"
  duration' <- readIORef (duration state)
  warning' <- readIORef (warning state)
  oldTimer' <- readIORef (timer state)

  Paused timeRemaining <- pauseTimer =<< readIORef (timer state) -- Get the amount of time remaining, doesn't actually pause anything
  remainingTime `set` [entryText := formatTime timeRemaining]
  totalTime `set` [entryText := formatTime duration']
  warningTime `set` [entryText := formatTime warning']
  (case oldTimer' of Stopped _ -> stopped; Paused _ -> paused; Counting _ -> running) `set` [toggleButtonActive := True]
  widgetGrabFocus remainingTime
  loopDialog dialog $ do
    [timer'', duration'', warning''] <- mapM (`get` entryText) [remainingTime, totalTime, warningTime]
    case (parseTime timer'', parseTime duration'', parseTime warning'') of
      (Nothing, _, _) -> errorDialog ("Error parsing remaining time: "++timer'') >> return False
      (_, Nothing, _) -> errorDialog ("Error parsing total time: "++duration'') >> return False
      (_, _, Nothing) -> errorDialog ("Error parsing warning time: "++warning'') >> return False
      (Just remain, Just dur, Just warn) -> do
        writeIORef (duration state) dur
        writeIORef (warning state) warn
        [s,p,r] <- mapM (`get` toggleButtonActive) [stopped, paused, running]
        when s $ modifyTimerState state (const $ return $ Stopped remain)
        when p $ modifyTimerState state (const $ return $ Paused remain)
        when r $ modifyTimerState state (const $ return $ Paused remain) >> modifyTimerState state startTimer
        return True
  widgetHide dialog

gotoSlideDialog state = do
  dialog <- builderGetObject (builder state) castToDialog "pageDialog"
  entry <- builderGetObject (builder state) castToSpinButton "pageDialogSpinButton"
  adjustment <- builderGetObject (builder state) castToAdjustment "pageDialogAdjustment"
  label <- builderGetObject (builder state) castToLabel "pageDialogLabel"
  pageNum <- pageAdjustment state `get` adjustmentValue
  pageMax <- pageAdjustment state `get` adjustmentUpper
  adjustment `set` [adjustmentValue := pageNum, adjustmentUpper := pageMax]
  label `set` [labelText := "Go to slide (1-" ++ show (round pageMax :: Integer) ++ "):"]
  widgetShowAll dialog
  widgetGrabFocus entry
  r <- dialogRun dialog
  value <- entry `get` spinButtonValue
  when (r == ResponseOk) $ pageAdjustment state `set` [adjustmentValue := value]
  putStrLn (show r)
  widgetHide dialog

openFileDialog state = do
  dialog <- fileChooserDialogNew Nothing Nothing FileChooserActionOpen [(stockOpen, ResponseOk), (stockCancel, ResponseCancel)]
  fileChooserSetURI dialog =<< readIORef (documentURL state)
  loopDialog dialog (openDoc state . head =<< fileChooserGetURIs dialog)
  widgetDestroy dialog
  return True

loopDialog d m = do
  r <- dialogRun d
  when (r == ResponseOk) $ do
    m' <- m
    unless m' $ loopDialog d m

------------------------
-- Views
------------------------

makeView :: State -> (Int -> Int) -> (DrawWindow -> GC -> DrawingArea -> EventM EExpose ()) -> IO DrawingArea
makeView state offset postDraw = do
  area <- drawingAreaNew
  widgetModifyBg area StateNormal black
  area `on` exposeEvent $ tryEvent $ drawView state offset postDraw area
  area `on` sizeAllocate $ \(Graphics.UI.Gtk.Rectangle _ _ width height) ->
    liftIO $ modifyIORef (views state) (Map.insert (castToWidget area) (width, height))
  area `on` unrealize $
    liftIO $ modifyIORef (views state) (Map.delete (castToWidget area))
  return area

drawView state offset postDraw area = do
  n <- liftIO $ liftM round $ pageAdjustment state `get` adjustmentUpper
  p <- liftIO $ liftM (offset . round) $ pageAdjustment state `get` adjustmentValue
  when (p <= n && p >= 1) $ do -- Check ensures that preview is not rendered when on last slide
    drawWindow <- eventWindow
    (w, h) <- liftIO $ widgetGetSize area
    cache' <- liftIO $ readIORef (pages state)
    gc <- liftIO $ gcNew drawWindow
    case Map.lookup (p, w, h) cache' of
      Nothing -> do
        pc <- liftIO $ widgetGetPangoContext area
        doc <- liftIO $ readIORef (document state)
        layout <- liftIO $ layoutText pc $
          case doc of
            Nothing -> "Loading file..." -- TODO: filename
            Just _  -> "Rendering slide "++show p++" of "++show n++"..."
        liftIO $ layoutSetAttributes layout [AttrForeground 0 (negate 1) white,
                                             AttrWeight 0 (negate 1) WeightBold,
                                             AttrSize 0 (negate 1) 24]
        liftIO $ drawLayout drawWindow gc 0 0 {-x y-} layout
        return ()
      Just (pixels, width', height', stride) -> do
        (width, height) <- liftIO $ drawableGetSize drawWindow
        liftIO $ BSU.unsafeUseAsCString (BS.concat (BSL.toChunks (decompressWith (defaultDecompressParams { decompressBufferSize = height' * stride }) pixels))) (\pixelPtr ->
          withImageSurfaceForData (castPtr pixelPtr) FormatRGB24 width' height' stride (\surface ->
            renderWithDrawable drawWindow $ do
              translate (fromIntegral $ (width - width') `div` 2) (fromIntegral $ (height - height') `div` 2)
              setSourceSurface surface 0 0
              paint))
        return ()
    postDraw drawWindow gc area

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
-- State modification and rendering functions
------------------------

startTimer (Counting microseconds) = return (Counting microseconds)
startTimer (Paused microseconds) = liftM (Counting . (microseconds+)) getMicroseconds
startTimer (Stopped microseconds) = liftM (Counting . (microseconds+)) getMicroseconds

pauseTimer (Counting microseconds) = liftM (Paused . (microseconds-)) getMicroseconds
pauseTimer (Paused microseconds) = return (Paused microseconds)
pauseTimer (Stopped microseconds) = return (Paused microseconds)

stopTimer (Counting microseconds) = liftM (Stopped . (microseconds-)) getMicroseconds
stopTimer (Paused microseconds) = return (Stopped microseconds)
stopTimer (Stopped microseconds) = return (Stopped microseconds)

unpauseTimer (Counting microseconds) = return (Counting microseconds)
unpauseTimer (Paused microseconds) = liftM (Counting . (microseconds+)) getMicroseconds
unpauseTimer (Stopped microseconds) = return (Stopped microseconds)

togglePauseTimer t@(Paused _) = startTimer t
togglePauseTimer t = pauseTimer t

toggleStopTimer t@(Stopped _) = startTimer t
toggleStopTimer t = stopTimer t

getMicroseconds :: IO Integer
getMicroseconds = do
  GTimeVal { gTimeValSec = sec, gTimeValUSec = usec } <- gGetCurrentTime
  return (fromIntegral sec * 1000 * 1000 + fromIntegral usec)

displayTime state (Paused microseconds) = displayTime' state microseconds
displayTime state (Stopped microseconds) = displayTime' state microseconds
displayTime state (Counting microseconds) = getMicroseconds >>= displayTime' state . (microseconds-)
displayTime' :: State -> Integer -> IO ()
displayTime' state microseconds = do
  warning' <- readIORef (warning state)
  let (fg_color, bg_color) | microseconds < 0 = (white, warningColor)
                           | microseconds < warning' = (warningColor, black)
                           | otherwise = (white, black)
  -- Set the hidden label so we maintain a minimum size
  do label2 <- builderGetObject (builder state) castToLabel "timeLabel2"
     set label2 [labelText := formatTime microseconds,
                 labelAttributes := [AttrSize 0 (negate 1) 60, --(fromIntegral minHeight),
                                     AttrForeground 0 (negate 1) fg_color,
                                     AttrBackground 0 (negate 1) bg_color]]
-- TODO: Why is AttrSize a different font than what the GUI builder makes?

  -- Initially set the size based on height
  label <- builderGetObject (builder state) castToLabel "timeLabel"
  (w, h) <- widgetGetSize label
  set label [labelText := formatTime microseconds,
             labelAttributes := [AttrAbsSize 0 (negate 1) (fromIntegral h),
                                 AttrForeground 0 (negate 1) fg_color,
                                 AttrBackground 0 (negate 1) bg_color]]

  -- Then rescale the text to ensure it is not too wide
  (_, PangoRectangle _ _ iw _) <- layoutGetExtents =<< labelGetLayout label
  let h' = h `min` (w * h `div` round iw)
  set label [labelText := formatTime microseconds,
             labelAttributes := [AttrAbsSize 0 (negate 1) (fromIntegral h'),
                                 AttrForeground 0 (negate 1) fg_color,
                                 AttrBackground 0 (negate 1) bg_color]]

overtimeColor = Color 0x8888 0x3333 0xffff {-purple-}
warningColor = Color 0xffff 0x0000 0x0000 {-red-}
white = Color 0xffff 0xffff 0xffff
black = Color 0x0000 0x0000 0x0000

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
    Stopped _ -> [labelText := "\x25A0",
                 labelAttributes := [AttrSize 0 (negate 1) 30, AttrForeground 0 (negate 1) white]]
    Counting _ -> [labelText := "\x25B6",
                   labelAttributes := [AttrSize 0 (negate 1) 30, AttrForeground 0 (negate 1) black]])
  writeIORef (timer state) timerState

------------------------
-- Time formatting
------------------------

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


formatTime :: Integer -> String
formatTime microseconds =
  (if microseconds < 0 then "-" else "") ++
  show hours ++ ":" ++ pad minutes ++ ":" ++ pad seconds {-++ "." ++ show tenths-} where
    ((((hours, minutes), seconds), _tenths), _) = id // 60 // 60 // 10 // (100*1000) $ abs microseconds
    (//) f base val = (f q, r) where (q, r) = val `quotRem` base
    pad x = case show x of
      [c]     -> ['0', c]
      ['-',c] -> ['-', '0', c]
      cs       -> cs

------------------------
-- Render thread
------------------------

-- We render in a timeout to keep the GUI responsive.  We move to a
-- slower timeout when everything is already rendered to avoid hogging
-- the CPU.  We avoid using a separate thread because that would
-- require using postGUISync which causes delays that slow down the
-- rendering.
renderThreadSoon state = timeoutAdd (renderThread' state >> return False) 1{-milliseconds-} >> return ()
renderThreadNotSoon state = timeoutAdd (renderThread' state >> return False) 100{-milliseconds-} >> return ()
renderThread' state = do
  doc <- readIORef (document state)
  case doc of
    Nothing -> renderThreadNotSoon state
    Just doc -> do
      numPages <- liftM round $ pageAdjustment state `get` adjustmentUpper
      currPage <- liftM round $ pageAdjustment state `get` adjustmentValue
      views <- liftM (nub . Map.elems) $ readIORef (views state) -- Sizes of the views
      cache <- liftM (Map.filterWithKey (\(_, w, h) _ -> (w, h) `elem` views)) $ readIORef (pages state)
      case [(page,w,h) |
            page <- sortBy (comparing (\i -> max (i - currPage) (2 * (currPage - i)))) [1..numPages],
            (w,h) <- sortBy (flip compare) $ views,
            (page,w,h) `Map.notMember` cache] of
        [] -> renderThreadNotSoon state
        (pageNum, width, height) : _ -> do -- Render a page
          page <- documentGetPage doc (pageNum-1)
          (docWidth, docHeight) <- pageGetSize page
          let scaleFactor = min (fromIntegral width  / docWidth)
                                (fromIntegral height / docHeight)
          -- NOTE: we use 24 bits instead of 32 to cut down on size and time
          putStrLn (show (pageNum, width, height))
          (pixels, w, h, stride) <- withImageSurface FormatRGB24 (round $ scaleFactor * docWidth) (round $ scaleFactor * docHeight) (\surface -> do
            renderWith surface $ do
              scale scaleFactor scaleFactor
              setSourceRGB 1.0 1.0 1.0 >> paint -- draw page background
              pageRender page -- draw page
            return (,,,) `ap` imageSurfaceGetData surface
                         `ap` imageSurfaceGetWidth surface
                         `ap` imageSurfaceGetHeight surface
                         `ap` imageSurfaceGetStride surface)
          -- TODO: filter out unused sizes
          c <- readIORef (compression state)
          let pixels' = compressWith (defaultCompressParams { compressLevel = compressionLevel c }) (BSL.fromChunks [pixels])
          BSL.length pixels' `seq` return () -- avoid memory leak due to not being strict enough
          writeIORef (pages state) (Map.insert (pageNum,width,height) (pixels', w, h, stride) cache)
          mainWindows state >>= mapM_ widgetQueueDraw
          renderThreadSoon state

errorDialog msg = do
  d <- messageDialogNew Nothing [DialogModal, DialogDestroyWithParent] MessageError ButtonsClose msg
  dialogRun d
  widgetDestroy d

openDoc state uri = do
  doc <- catchGError (documentNewFromFile uri Nothing)
           (\x -> errorDialog ("Error opening file: " ++ show x {- TODO: ++ "\n\nURI:" ++ uri-}) >> return Nothing)
  case doc of
    Nothing -> return False
    Just doc -> do
      title <- doc `get` documentTitle
      windowListToplevels >>= mapM_ (flip windowSetTitle ((if null title then takeFileName uri else title) ++ " - PDF Presenter"))
      numPages <- documentGetNPages doc
      pageAdjustment state `set` [adjustmentLower := 1, adjustmentUpper := fromIntegral numPages, adjustmentValue :~ (+0)]
      writeIORef (pages state) Map.empty
      writeIORef (documentURL state) uri
      writeIORef (document state) (Just doc)
      return True
