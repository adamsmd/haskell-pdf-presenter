module Main where

--import Control.Applicative
--import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust)
import Data.IORef
import Data.List
import Data.Ord
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Types
--import Graphics.Rendering.Pango
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC (gcNew)
import Graphics.UI.Gtk.Poppler.Document
import Graphics.UI.Gtk.Poppler.Page
import Graphics.UI.Gtk.Poppler.Annot
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


-- L (last)
-- Tab (overview)
-- Time tracking mode
-- Any key gets out of mute mode?
-- Store shortcuts in F11 keys (or alpha keys? shifted?)
-- escape, Alt+F4, q
-- help, h, ?
-- scroll up/down (zoom presenter(?): duplicate, timer, next slide, minor overview, major overview)

-- initial page (for startup)
-- Ctrl-o (open)
-- start counter with "back"
-- pause timer
-- elapsed vs remaining time vs time of day
-- disable screen saver
-- disable screen dimming
-- hyperlinks
-- test if works on MS Windows
-- zoom?
-- drawing? (w/ colors, undo, clear, cursor shape, on fresh slide)
-- overview? (TAB?)
-- "Yes" button
-- follow link (ALT-left, ALT-right)
-- overview in the presenter view:
--   - possibly along with current
--   - could have cool zoom out effect
--   - partial zoom out
-- load multiple PDF files?
-- goto slide by number (entry box?, entry box as slide number display? label changes to entry box when clicked)
-- windowed (non-fullscreen)?
-- switch to demo
-- vim or arrow key navigation on overview screen
-- hilight cursor? big cursor? cursor shape?
-- mouse wheel? select slide? select color? zoom?
-- incremental rendering on overview slide (priorizie one's under cursor or ones near current slide)
-- mouse cursor on audience vs presenter screen (click on audience, means next, on presenter means select)
-- set boarder of time on even seconds
-- send-to-back or minimize button

-- does alt-tab do both presenter and audience? (if one window is foregound, then both should be?)
-- what would ctrl-tab mean? switch to other pdf? switch mouse to presenter vs audience? swap presenter and audience windows?

-- adjust ending time
-- adjust durration
-- end time = start time + durration (dynamically update?)

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
 { endTime :: GTimeVal -- 'Maybe' for "dont" display or haven't started counting yet? Or as a seperate flag?
 , blanking :: Blanking
 , documentURL :: String
 -- split ratio
 -- switch screens
}

data ThreadingInfo = ThreadingInfo
 { views :: IORef (Map.Map Widget (Int{-width-}, Int{-height-}))
 , document :: IORef (Maybe Document)
 , pages :: IORef (Map.Map (Int{-width-}, Int{-height-}) (Map.Map Int{-page number-} Pixmap{-cached page-}))
 }

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
    startTime <- gGetCurrentTime
    newIORef (State { blanking = BlankNone, endTime = startTime, documentURL = "file://"++file })
  page <- adjustmentNew 0 0 0 1 10 1
  threadingInfo <- do
    viewers <- newIORef (Map.empty :: Map.Map Widget (Int{-width-}, Int{-height-}))
    doc <- newIORef Nothing
    cache <- newIORef (Map.fromList [] :: Map.Map (Int, Int) (Map.Map Int Pixmap))
    return $ ThreadingInfo { views = viewers, document = doc, pages = cache }

  Just display <- displayGetDefault
  nscreens <- displayGetNScreens display
  putStrLn $ "NScreens:"++show nscreens
  Just screen <- screenGetDefault
  nmonitors <- screenGetNMonitors screen
  putStrLn $ "NMonitors:"++show nmonitors
  --Rectangle x y width height <- screenGetMonitorGeometry screen 1

  top <- notebookNew
  presenter <- hPanedNew

  -------------------
  -- Audience window
  -------------------
  do window <- makeWindow page presenter top threadingInfo state
     view <- makeView threadingInfo page id
     window `containerAdd` view

  -------------------
  -- Presenter window
  -------------------
  do window <- makeWindow page presenter top threadingInfo state
     windowBox <- vBoxNew False 0
     window `containerAdd` windowBox
     set top [notebookShowBorder := False, notebookShowTabs := False]
     boxPackStart windowBox top PackGrow 0

     -- Views
-- NOTE: for some odd reason, this crashes if we add the scrolled second
     do do thumbnails <- newIORef []
       
           scrolled <- scrolledWindowNew Nothing Nothing
           --windowBox `containerAdd` scrolled
           layout <- layoutNew Nothing Nothing
           notebookInsertPage top scrolled "3" 1
           scrolled `containerAdd` layout
   
           let recomputeViews = do
                 thumbnails' <- readIORef thumbnails
                 mapM (containerRemove layout :: DrawingArea -> IO ()) thumbnails'
           
                 (totalWidth, _) <- widgetGetSize window --scrolled
                 numPages <- get page adjustmentUpper
                 let width = totalWidth `div` 4
                     height = width * 3 `div` 4
                     numRows = round numPages `div` 4 + 1
                 putStrLn $ "RECOMPUTING" ++ show (numPages, totalWidth, width, height, numRows)
                 layoutSetSize layout totalWidth (height*numRows)
                 newThumbnails <- sequence [ do
                   view <- makeView threadingInfo page (const (4*row+col+1))
                   widgetSetSizeRequest view width height
                   layoutPut layout view (col*width) (row*height)
                   --putStrLn $ "X"++ show (row,col,width,height,row*height,col*width)
                   widgetShowAll view
                   return view
                   | row <- [0..numRows-1], col <- [0..3]]
                 () <- atomicWriteIORef thumbnails newThumbnails
                 return ()
   
           page `onAdjChanged` recomputeViews
           --page `onValueChanged` changeHilight
           window `on` configureEvent $ liftIO recomputeViews >> return False
           --onZoom $ recocmputeViews
           page `onValueChanged` (containerRemove windowBox presenter >> boxPackStart windowBox scrolled PackGrow 0)

        do --presenter <- vBoxNew False 0
           --boxPackStart windowBox presenter PackGrow 0
           notebookAppendPage top presenter "1"
           view1 <- makeView threadingInfo page id
           view2 <- makeView threadingInfo page (+1)
           panedPack1 presenter view1 True True
           panedPack2 presenter view2 True True

        return ()

     -- Meta-data
     do metaBox <- hBoxNew False 0
        boxPackEnd windowBox metaBox PackNatural 0
   
        time <- labelNew Nothing
        set time [labelAttributes := [AttrSize 0 (negate 1) 40]]
        timeoutAdd (liftM endTime (readIORef state) >>= displayTime time >> return True) 100
        boxPackStart metaBox time PackRepel 0

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
        --page `on` value
        page `onValueChanged` (get page adjustmentValue >>= entrySetText slideNum . show . round)
        slideNum `onEditableChanged` (slideNum `set` [entryText :~ filter (isDigit)])
{-
        slideNum `onInsertText` \s p -> if False && all isDigit s
                                        then editableInsertText slideNum s p >> return (length s + p)
                                        else return p
-}
        slideNum `onEntryActivate` (slideNum `get` entryText >>= \text -> page `set` [adjustmentValue := read text])


{-
        slideNum <- spinButtonNew page 1 0
        slideNum `set` [entryHasFrame := False]
        pc <- widgetGetPangoContext slideNum
        fd <- contextGetFontDescription pc
        fontDescriptionSetSize fd 40
        widgetModifyFont slideNum (Just fd)
        widgetModifyBase slideNum StateNormal (Color 0 0 0)
        entrySetAlignment slideNum 1
        --entrySetMaxLength slideNum 5 -- TODO: make it dynamic based on slide count
        --entrySetWidthChars
        spinButtonSetNumeric slideNum True
        --contextSetFontDescription pc fd
        --entrySetWidthChars
        widgetModifyBg slideNum StateNormal (Color (64*256) (64*256) (64*256))
        boxPackStart metaBox slideNum PackNatural 0
-}

        slide <- labelNew Nothing
        set slide [labelAttributes := [AttrSize 0 (negate 1) 40]]
        let update = do p <- liftM round $ get page adjustmentValue
                        n <- liftM round $ get page adjustmentUpper
                        set page [adjustmentValue := fromIntegral (max 1 (min p n))]
                        set slide [labelText := "/" ++ show n]
          in do --page `onValueChanged` update
                page `onAdjChanged` update
        boxPackStart metaBox slide PackNatural 0

  tops <- windowListToplevels
  putStrLn $ show (length tops)

  -- Show windows, fork the rendering thread, and start main loop
  windowListToplevels >>= mapM_ widgetShowAll
  liftIO $ forkIO $ renderThread page presenter threadingInfo state
  mainGUI

makeWindow adjustment paned top threadingInfo state = do
  window <- windowNew
  windowFullscreen window
  widgetModifyBg window StateNormal (Color 0 0 0)
  window `onDestroy` mainQuit
  adjustment `onValueChanged` widgetQueueDraw window
  adjustment `onAdjChanged` widgetQueueDraw window -- ensures page counter is updated
  window `on` keyPressEvent $ do
     mods <- eventModifier
     name <- eventKeyName
     liftIO $ handleKey mods name
  return window where
    -- TODO: left and right mouse click for forward and backward
    handleResponce dialog ResponseOk = do
      (uri : _) <- fileChooserGetURIs dialog
      () <- atomicModifyIORef state (\x -> (x { documentURL = uri }, ()))
      putStrLn "NEWFILE"
      () <- atomicWriteIORef (document threadingInfo) Nothing
      --() <- atomicWriteIORef (pages threadingInfo) Map.empty
      widgetDestroy dialog
    handleResponce dialog _ = widgetDestroy dialog
    handleKey :: [Modifier] -> String -> IO Bool
    handleKey [Graphics.UI.Gtk.Control] "q" = mainQuit >> return True
    handleKey [] key | key `elem` ["Left", "Up", "Page_Up", "BackSpace"] =
      adjustment `set` [adjustmentValue :~ (+(negate 1))] >> return True
    handleKey [] key | key `elem` ["Right", "Down", "Page_Down", "Right", "space", "Return"] =
      adjustment `set` [adjustmentValue :~ (+1)] >> return True
    handleKey [] "Home" = adjustment `set` [adjustmentValue := 0] >> return True
    handleKey [] "End" = adjustment `get` adjustmentUpper >>= \p -> adjustment `set` [adjustmentValue := p] >> return True
    handleKey [] "Tab" = set top [notebookCurrentPage :~ (`mod` 2) . (+ 1)] >> return True
    handleKey [] "r" = atomicWriteIORef (document threadingInfo) Nothing >>= \() -> return True
    handleKey [] "bracketleft" = paned `set` [panedPosition :~ max 0 . (+(negate 20))] >> return True
    handleKey [] "bracketright" = paned `set` [panedPosition :~ (+20)] >> return True
    handleKey [Shift] "braceleft" = paned `set` [panedPosition :~ max 0 . (+(negate 1))] >> return True
    handleKey [Shift] "braceright" = paned `set` [panedPosition :~ (+1)] >> return True
    handleKey [Graphics.UI.Gtk.Control] "o" = do
        dialog <- fileChooserDialogNew Nothing Nothing FileChooserActionOpen [(stockOpen, ResponseOk), (stockCancel, ResponseCancel)]
        dialog `on` response $ handleResponce dialog
        widgetShowAll dialog
--fileChooserAddFilter :: FileChooserClass self => self -> FileFilter -> IO ()
        return True
    handleKey [] "x" = do doc <- readIORef (document threadingInfo)
                          case doc of
                            Nothing -> return True
                            Just doc -> panedStops paned doc >> return True
    handleKey mods name = (putStrLn $ "KeyEvent:"++show mods ++ name) >> return False

panedStops paned document = do
  maxPos <- liftM fromIntegral $ paned `get` panedMaxPosition
  (_, panedHeight) <- widgetGetSize paned
  page <- documentGetPage document 0
  (pageWidth, pageHeight) <- pageGetSize page
  let newPos = (fromIntegral panedHeight) * pageWidth / pageHeight
  paned `set` [panedPosition := maxPos - round newPos]

makeView :: ThreadingInfo -> Adjustment -> (Int -> Int) -> IO DrawingArea
makeView threadingInfo page offset = do
  area <- drawingAreaNew
  widgetModifyBg area StateNormal (Color 0 0 0)
  area `on` exposeEvent $ tryEvent $ do
    n <- liftIO (liftM round $ get page adjustmentUpper)
    p <- liftIO (liftM (offset . round) $ get page adjustmentValue)
    when (p <= n && p >= 1) $ do
      drawWindow <- eventWindow
      size <- liftIO $ widgetGetSize area
      cache' <- liftIO $ readIORef (pages threadingInfo)
      gc <- liftIO $ gcNew drawWindow
      case Map.lookup p $ Map.findWithDefault Map.empty size cache' of
        Nothing -> do
          pc <- liftIO $ widgetGetPangoContext area
          doc <- liftIO $ readIORef (document threadingInfo)
          layout <- liftIO $ layoutText pc $
            case doc of
              Nothing -> "Loading file..." -- TODO: filename
              Just _  -> "Loading slide "++show p++" of "++show n++"..."
          liftIO $ layoutSetAttributes layout [AttrForeground 0 (negate 1) (Color 65535 65535 65535),
                                               AttrWeight 0 (negate 1) WeightBold,
                                               AttrSize 0 (negate 1) 24]
          liftIO $ drawLayout drawWindow gc 0 0 {-x y-} layout
          return ()
        Just pixmap -> do
          (width, height) <- liftIO $ drawableGetSize drawWindow
          (width', height') <- liftIO $ drawableGetSize pixmap
          liftIO $ drawDrawable drawWindow gc pixmap 0 0 ((width - width') `div` 2) ((height - height') `div` 2) width height
  area `on` configureEvent $ tryEvent $ do
    (width, height) <- eventSize
    () <- liftIO $ atomicModifyIORef (views threadingInfo) (\x -> (Map.insert (castToWidget area) (width, height) x, ()))
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
renderThread adjustment paned threadingInfo state = sequence_ $ repeat loop where
  loop = do
    threadDelay 1000 -- Keeps the GUI responsive
    state' <- readIORef state
    oldDoc <- readIORef (document threadingInfo)

    -- Note that we do exactly one postGUISync in each branch.  This may improve GUI responsiveness
    (doc, currPage, numPages) <- case oldDoc of
      Just doc -> do (currPages, numPages) <- postGUISync $ do
                       n <- get adjustment adjustmentUpper
                       c <- get adjustment adjustmentValue
                       return (round c, round n)
                     return (doc, currPages, numPages)
      Nothing -> do doc <- catchGError (documentNewFromFile (documentURL state') Nothing)
                             (\x -> putStrLn ("---"++(show x)) >> return Nothing)
                    case doc of
                      Nothing -> error "Exit"
                      Just doc -> do
                        title <- get doc documentTitle
                        numPages <- documentGetNPages doc
                        currPage <- postGUISync $ do
                          -- TODO: URL basename if title is empty
                          windowListToplevels >>= mapM (flip windowSetTitle (title ++ " - PDF Presenter"))
                          set adjustment [adjustmentLower := 1, adjustmentUpper := fromIntegral numPages, adjustmentValue :~ (+0)]
                          liftM round $ get adjustment adjustmentValue
                        () <- atomicWriteIORef (pages threadingInfo) Map.empty
                        () <- atomicWriteIORef (document threadingInfo) (Just doc)
                        return (doc, currPage, numPages)

    --postGUISync $ panedStops paned doc
    --x <- widgetGetSize t
    --putStrLn $ "FOO:" ++ show x

    -- Sizes of the views
    views <- readIORef (views threadingInfo)
    cache <- liftM (Map.filterWithKey (\k a -> k `elem` Map.elems views)) $ readIORef (pages threadingInfo)
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
          () <- atomicModifyIORef (pages threadingInfo) (\x -> (Map.unionWith Map.union (Map.singleton (width,height) (Map.singleton pageNum pixmap)) x, ()))
          windowListToplevels >>= mapM_ widgetQueueDraw


--windowPresent :: WindowClass self => self -> IO ()
--windowSetScreen
--windowScreen :: WindowClass self => Attr self Screen
  --size <- drawableGetSize (castToDrawable window)
  --Just screen <- screenGetDefault
  --Rectangle _ _ width height <- screenGetMonitorGeometry screen 0
--widgetSetSizeRequest area (truncate width) (truncate height)
--windowSetPosition window WinPosCenter
