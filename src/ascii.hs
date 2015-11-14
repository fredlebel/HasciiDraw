{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

--import System.IO
--import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad
import Data.Word
import Data.Maybe
import Data.IORef
--import System.Locale.SetLocale
import UI.NCurses
import UI.NCurses.Panel
import Control.Lens

colorBlack          = Color 0
colorBrightRed      = Color 1
colorBrightGreen    = Color 2
colorBrightYellow   = Color 3
colorBrightBlue     = Color 4
colorBrightPurple   = Color 5
colorBrightCyan     = Color 6
colorWhite          = Color 7
colorDarkGray       = Color 8
colorDarkRed        = Color 9
colorDarkGreen      = Color 10
colorDarkYellow     = Color 11
colorDarkBlue       = Color 12
colorDarkPurple     = Color 13
colorDarkCyan       = Color 14
colorBrightGray     = Color 15

codepage437 :: [Char]
codepage437 = [
        '\x0000',
        '\x263A', -- Smiley
        '\x263B', -- Dark smiley
        '\x2665', -- Suit Heart
        '\x2666', -- Suit Diamond
        '\x2663', -- Suit Clubs
        '\x2660', -- Suit Spade
        '\x2022', -- Dot
        '\x25D8', -- Inverted Dot
        '\x25CB', -- Circle
        '\x25D9', -- Inverted Circle
        '\x2642', -- Male
        '\x2640', -- Female
        '\x266A', -- One Note
        '\x266B', -- Two Notes
        '\x263C', -- Sun

        '\x25BA', -- Right Wedge
        '\x25C4', -- Left Wedge
        '\x2195', -- Up/Down Arrows
        '\x203c', -- Two exclamation points
        '\x00B6', -- Pilcrow
        '\x00A7', -- Section sig
        '\x25AC', -- Black Rectangle
        '\x21A8', -- Up/Down Bar Arrows
        '\x2191', -- Up Arrow
        '\x2193', -- Down Arrow
        '\x2192', -- Right Arrow
        '\x2190', -- Left Arrow
        '\x221F', -- Right Angle
        '\x2194', -- Left/Right Arrows
        '\x25B2', -- Up Wedge
        '\x25BC' -- Down Wedge
    ] ++
    " !\"#$%&'()*+,-./" ++
    "0123456789:;<=>?" ++
    "@ABCDEFGHIJKLMNO" ++
    "PQRSTUVWXYZ[\\]^_" ++
    "`abcdefghijklmno" ++
    "pqrstuvwxyz{|}~" ++
    [
        '\x2302', -- House

        -- Accents
        '\x00C7',
        '\x00FC',
        '\x00E9',
        '\x00E2',
        '\x00E4',
        '\x00E0',
        '\x00E5',
        '\x00E7',
        '\x00EA',
        '\x00EB',
        '\x00E8',
        '\x00EF',
        '\x00EE',
        '\x00EC',
        '\x00C4',
        '\x00C5',

        '\x00C9',
        '\x00E6',
        '\x00C6',
        '\x00F4',
        '\x00F6',
        '\x00F2',
        '\x00FB',
        '\x00F9',
        '\x00FF',
        '\x00D6',
        '\x00DC',
        '\x00A2', -- Cent
        '\x00A3', -- Pound
        '\x00A5', -- Yen
        '\x20A7', -- Peseta
        '\x0192', -- F with hook

        '\x00E1', -- 
        '\x00ED', -- 
        '\x00F3', -- 
        '\x00F4', -- 
        '\x00F1', -- 
        '\x00D1', -- 
        '\x00AA', -- 
        '\x00BA', -- 
        '\x00BF', -- Inverted ?
        '\x2310', -- Negation left
        '\x00AC', -- Negation right
        '\x00BD', -- 1/2
        '\x00BC', -- 1/4
        '\x00A1', -- Inverted !
        '\x00AB', -- Shift left
        '\x00BB', -- Shift right

        '\x2591', -- Block light shade
        '\x2592', -- Block medium shade
        '\x2593', -- Block dark shade
        '\x2502', -- Box drawing element
        '\x2524', -- Box drawing element
        '\x2561', -- Box drawing element
        '\x2562', -- Box drawing element
        '\x2556', -- Box drawing element
        '\x2555', -- Box drawing element
        '\x2563', -- Box drawing element
        '\x2551', -- Box drawing element
        '\x2557', -- Box drawing element
        '\x255D', -- Box drawing element
        '\x255C', -- Box drawing element
        '\x255B', -- Box drawing element
        '\x2510', -- Box drawing element

        '\x2514', -- Box drawing element
        '\x2534', -- Box drawing element
        '\x252C', -- Box drawing element
        '\x251C', -- Box drawing element
        '\x2500', -- Box drawing element
        '\x253C', -- Box drawing element
        '\x255E', -- Box drawing element
        '\x255F', -- Box drawing element
        '\x255A', -- Box drawing element
        '\x2554', -- Box drawing element
        '\x2569', -- Box drawing element
        '\x2566', -- Box drawing element
        '\x2560', -- Box drawing element
        '\x2550', -- Box drawing element
        '\x256C', -- Box drawing element
        '\x2567', -- Box drawing element

        '\x2568', -- Box drawing element
        '\x2564', -- Box drawing element
        '\x2565', -- Box drawing element
        '\x2559', -- Box drawing element
        '\x2558', -- Box drawing element
        '\x2552', -- Box drawing element
        '\x2553', -- Box drawing element
        '\x256B', -- Box drawing element
        '\x256A', -- Box drawing element
        '\x2518', -- Box drawing element
        '\x250C', -- Box drawing element
        '\x2588', -- Black box
        '\x2584', -- Black bottom box
        '\x258C', -- Black left box
        '\x2590', -- Black right box
        '\x2580', -- Black top box

        -- Greek letters
        '\x03B1',
        '\x00DF',
        '\x0393',
        '\x03C0',
        '\x03A3',
        '\x03C3',
        '\x00B5',
        '\x03C4',
        '\x03A6',
        '\x0398',
        '\x03A9',
        '\x03B4',
        '\x221E', -- Infinity
        '\x03C6',
        '\x03B5',
        '\x2229', -- Intersection

        '\x2261', -- 
        '\x00B1', -- 
        '\x2265', -- 
        '\x2264', -- 
        '\x2320', -- 
        '\x2321', -- 
        '\x00F7', -- 
        '\x2248', -- 
        '\x00B0', -- 
        '\x2219', -- 
        '\x00B7', -- 
        '\x221A', -- 
        '\x207F', -- 
        '\x00B2', -- 
        '\x25A0', -- 
        '\x00A0' -- 

    ]

byteToChar :: Word8 -> Char
byteToChar b = codepage437 !! (fromIntegral b)

initDefaultColors :: Curses [(Integer, Integer, Integer)]
initDefaultColors = do
    oldColors <- forM [0..15] $ queryColor . Color
    defineColor (Color  0)    0    0    0
    defineColor (Color  1) 1000    0    0
    defineColor (Color  2)    0 1000    0
    defineColor (Color  3) 1000 1000    0
    defineColor (Color  4)    0    0 1000
    defineColor (Color  5) 1000    0 1000
    defineColor (Color  6)    0 1000 1000
    defineColor (Color  7) 1000 1000 1000
    defineColor (Color  8)  500  500  500
    defineColor (Color  9)  500    0    0
    defineColor (Color 10)    0  500    0
    defineColor (Color 11)  500  500    0
    defineColor (Color 12)    0    0  500
    defineColor (Color 13)  500    0  500
    defineColor (Color 14)    0  500  500
    defineColor (Color 15)  750  750  750
    forM_ [1..255] $ \n -> do
        let ns = fromIntegral n
        newColorID (Color (ns `mod` 16)) (Color (ns `div` 16)) n
    return oldColors

restoreColors colors = do
    zipWithM_ fn colors [0..]
    where
        fn (r,g,b) n = defineColor (Color n) r g b

findColorID :: Color -> Color -> Curses ColorID
findColorID fg bg = fromMaybe (ColorID 0) <$> foldM fn Nothing [0..255]
    where
        fn Nothing n = do
            pair <- queryColorID (ColorID n)
            return $ if pair == (fg, bg) then Just (ColorID n) else Nothing
        fn match _ = return match

updatePanel :: Panel -> Update a -> Curses a
updatePanel p fn = getPanelWindow p >>= flip updateWindow fn

mkGlyphPicker :: Curses Panel
mkGlyphPicker = do
    win <- newWindow 18 18 1 1
    updateWindow win $ do
        clear
        drawBox Nothing Nothing
        forM_ [0..255] $ \n -> do
            moveCursor (n `div` 16 + 1) (n `mod` 16 + 1)
            drawGlyph (Glyph (codepage437 !! fromIntegral n) [])
        moveCursor 8 8
    newPanel win

mkColorPicker :: Curses Panel
mkColorPicker = do
    win <- newWindow 18 18 1 1
    updateWindow win $ do
        clear
        drawBox Nothing Nothing
        forM_ [0..255] $ \n -> do
            moveCursor (n `div` 16 + 1) (n `mod` 16 + 1)
            drawGlyph (Glyph 'M' [AttributeColor . ColorID . fromIntegral $ n])
        moveCursor 8 8
    newPanel win

mkCursorPanel = do
    curWin <- newWindow 1 1 12 40
    updateWindow curWin $ do
        moveCursor 0 0
    curPan <- newPanel curWin
    return curPan

data AsciiDrawState = ADS
    { _colorPanel :: Panel
    , _glyphPanel :: Panel
    , _imagePanel :: Panel
    , _editorPanel :: Panel
    , _visualPanel :: Panel
    , _cursorPanel :: Panel
    , _currentColor :: ColorID
    , _currentChar :: Char
    , _colorNormal :: ColorID
    , _colorInsert :: ColorID
    , _colorVisual :: ColorID
    , _yankWin :: Window
    , _undoBuffer :: [Window]
    }

$(makeLenses ''AsciiDrawState)

newtype Box = Box (Integer, Integer, Integer, Integer)

getWindowBox :: Bool -> Window -> Curses Box
getWindowBox hasBorder win = do
    (h, w) <- updateWindow win windowSize
    if hasBorder
        then return $ Box (h-2, w-2, 1, 1)
        else return $ Box (h, w, 0, 0)

inBox :: Box -> (Integer, Integer) -> Bool
inBox (Box (h, w, y, x)) (y', x') =
    y' >= y && y' < (h+y) &&
    x' >= x && x' < (w+x)

data Direction = DirUp | DirDown | DirLeft | DirRight

shiftDirection :: Direction -> (Integer, Integer) -> (Integer, Integer)
shiftDirection DirUp    (y, x) = (y-1, x)
shiftDirection DirDown  (y, x) = (y+1, x)
shiftDirection DirLeft  (y, x) = (y, x-1)
shiftDirection DirRight (y, x) = (y, x+1)

safeMoveCursor :: Direction -> Bool -> Panel -> Curses ()
safeMoveCursor dir hasBorder p = do
    win <- getPanelWindow p
    box <- getWindowBox hasBorder win
    newPos <- (shiftDirection dir) <$> updateWindow win cursorPosition
    when (inBox box newPos) $
        updateWindow win (uncurry moveCursor newPos)

moveCursorPanel :: Panel -> Panel -> Curses ()
moveCursorPanel curPan hostPan = do
    hostWin <- getPanelWindow hostPan
    (_, _, y, x) <- getWindowRect hostWin
    (cy, cx) <- updateWindow hostWin cursorPosition
    movePanel curPan (cy + y) (cx + x)
    updatePanel curPan $ overwriteFrom hostWin

updateCurrentChar c stateRef = do
    liftIO $ modifyIORef stateRef (set currentChar c)

yankAction :: (Integer, Integer, Integer, Integer) -> IORef AsciiDrawState -> Curses ()
yankAction (h, w, y, x) stateRef = do
    st <- liftIO $ readIORef stateRef
    imageWin <- getPanelWindow (_imagePanel st)
    updateWindow (_yankWin st) $ do
        resizeWindow h w
        moveWindow y x
        overwriteFrom imageWin
    return ()

pasteAction :: (Integer, Integer) -> IORef AsciiDrawState -> Curses ()
pasteAction (y, x) stateRef = do
    st <- liftIO $ readIORef stateRef
    (yh, yw, _, _) <- getWindowRect (_yankWin st)
    updatePanel (_imagePanel st) $ do
        copyFrom (_yankWin st) 0 0 y x (min (y + yh - 1) 23) (min (x + yw - 1) 79) False
    return ()

pushUndo stateRef= do
    st <- liftIO $ readIORef stateRef
    undoWin <- newWindow 24 80 0 0
    imageWin <- getPanelWindow (_imagePanel st)
    updateWindow undoWin $ overwriteFrom imageWin
    liftIO $ modifyIORef stateRef (over undoBuffer (undoWin:))

popUndo stateRef= do
    st <- liftIO $ readIORef stateRef
    doPop (_undoBuffer st)
    where
        doPop [] = return ()
        doPop (win:wins) = do
            st <- liftIO $ readIORef stateRef
            updatePanel (_imagePanel st) $ overwriteFrom win
            liftIO $ modifyIORef stateRef (set undoBuffer wins)
            closeWindow win

insertMode :: IORef AsciiDrawState -> Curses ()
insertMode stateRef = do
    st <- liftIO $ readIORef stateRef
    editorWin <- getPanelWindow $ _editorPanel st
    updateWindow editorWin $ do
       clear
       setColor (_colorInsert st)
       moveCursor 0 0
       drawString "INSERT"

    loop

    where
        loop = do
            st <- liftIO $ readIORef stateRef
            w <- defaultWindow
            moveCursorPanel (_cursorPanel st) (_imagePanel st)
            updatePanel (_editorPanel st) $ setTouched True
            refreshPanels
            render
            ev <- getEvent w Nothing
            imageWin <- getPanelWindow $ _imagePanel st
            case ev of
                Nothing -> loop
                Just ev' -> case ev' of
                    EventCharacter '\x001B' -> do -- ESC or ALT
                        evProbe <- getEvent w (Just 0)
                        case evProbe of
                            Nothing -> do
                                normalMode stateRef
                            Just _ -> loop
                    EventCharacter c -> do
                        updateWindow imageWin $ do
                            drawGlyph $ Glyph c [AttributeColor (_currentColor st)]
                        updateCurrentChar c stateRef
                        loop
                    EventSpecialKey KeyLeftArrow -> do
                        safeMoveCursor DirLeft False (_imagePanel st)
                        loop
                    EventSpecialKey KeyRightArrow -> do
                        safeMoveCursor DirRight False (_imagePanel st)
                        loop
                    EventSpecialKey KeyUpArrow -> do
                        safeMoveCursor DirUp False (_imagePanel st)
                        loop
                    EventSpecialKey KeyDownArrow -> do
                        safeMoveCursor DirDown False (_imagePanel st)
                        loop
                    EventSpecialKey (KeyFunction 1) -> do
                        colorPrompt stateRef
                        loop
                    EventSpecialKey (KeyFunction 2) -> do
                        glyphPrompt stateRef
                        loop
                    _ -> loop

normalMode :: IORef AsciiDrawState -> Curses ()
normalMode stateRef = do
    st <- liftIO $ readIORef stateRef
    editorWin <- getPanelWindow $ _editorPanel st
    updateWindow editorWin $ do
       clear
       setColor (_colorNormal st)
       moveCursor 0 0
       drawString "NORMAL"

    loop

    where
        loop = do
            st <- liftIO $ readIORef stateRef
            w <- defaultWindow
            moveCursorPanel (_cursorPanel st) (_imagePanel st)
            updatePanel (_editorPanel st) $ setTouched True
            refreshPanels
            render
            --imageWin <- getPanelWindow $ _imagePanel st
            ev <- getEvent w Nothing
            case ev of
                Nothing -> loop
                Just ev' -> case ev' of
                    EventCharacter 'q' -> return ()
                    EventCharacter 'i' -> pushUndo stateRef >> insertMode stateRef
                    EventCharacter 'v' -> visualMode stateRef
                    EventCharacter 'u' -> popUndo stateRef >> loop
                    EventCharacter 'p' -> do
                        pushUndo stateRef
                        curPos <- getPanelWindow (_imagePanel st) >>= flip updateWindow cursorPosition
                        pasteAction curPos stateRef
                        loop
                    EventSpecialKey KeyLeftArrow -> do
                        safeMoveCursor DirLeft False (_imagePanel st)
                        loop
                    EventSpecialKey KeyRightArrow -> do
                        safeMoveCursor DirRight False (_imagePanel st)
                        loop
                    EventSpecialKey KeyUpArrow -> do
                        safeMoveCursor DirUp False (_imagePanel st)
                        loop
                    EventSpecialKey KeyDownArrow -> do
                        safeMoveCursor DirDown False (_imagePanel st)
                        loop
                    EventSpecialKey (KeyFunction 1) -> do
                        colorPrompt stateRef
                        loop
                    EventSpecialKey (KeyFunction 2) -> do
                        glyphPrompt stateRef
                        loop
                    _ -> loop

visualMode :: IORef AsciiDrawState -> Curses ()
visualMode stateRef = do
    enter
    loop

    where
        enter = do
            st <- liftIO $ readIORef stateRef
            showPanel $ _visualPanel st
            (cy, cx) <- updatePanel (_imagePanel st) cursorPosition
            modVisualPanel (\_ -> (1, 1, cy, cx))
            setCursorMode CursorInvisible

        leave = do
            st <- liftIO $ readIORef stateRef
            hidePanel $ _visualPanel st
            setCursorMode CursorVisible

        modVisualPanel fn = do
            st <- liftIO $ readIORef stateRef
            let vp = _visualPanel st
            vw <- getPanelWindow vp
            (h, w, y, x) <- fn <$> getWindowRect vw
            newVisualWindow <- newWindow h w y x
            replacePanelWindow vp newVisualWindow
            closeWindow vw

        drawStatusBar = do
            st <- liftIO $ readIORef stateRef
            editorWin <- getPanelWindow $ _editorPanel st
            updateWindow editorWin $ do
               clear
               setColor (_colorVisual st)
               moveCursor 0 0
               drawString "VISUAL"

        updateVisualSelection update = do
            st <- liftIO $ readIORef stateRef
            let vp = _visualPanel st
            vw <- getPanelWindow vp
            (h, w, y, x) <- getWindowRect vw
            tmpWin <- newWindow h w y x
            imageWin <- getPanelWindow (_imagePanel st)
            updateWindow tmpWin $ overwriteFrom imageWin
            updateWindow tmpWin update
            updateWindow imageWin $ overwriteFrom tmpWin
            closeWindow tmpWin

        drawSelection = do
            st <- liftIO $ readIORef stateRef
            let vp = _visualPanel st
            vw <- getPanelWindow vp
            imageWin <- getPanelWindow (_imagePanel st)
            updateWindow vw $ do
                setBackground $ Glyph ' ' [AttributeReverse]
                clear
                overlayFrom imageWin

        loop = do
            drawStatusBar
            drawSelection
            st <- liftIO $ readIORef stateRef
            win <- defaultWindow
            updatePanel (_editorPanel st) $ setTouched True
            refreshPanels
            render
            ev <- getEvent win Nothing
            case ev of
                Nothing -> loop
                Just ev' -> case ev' of
                    EventCharacter '\x001B' -> do -- ESC or ALT
                        evProbe <- getEvent win (Just 0)
                        case evProbe of
                            Nothing -> leave >> normalMode stateRef
                            Just _ -> loop
                    EventCharacter 'i' -> leave >> pushUndo stateRef >> insertMode stateRef
                    EventSpecialKey KeyLeftArrow -> do
                        modVisualPanel $ \(h, w, y, x) -> (h, w-1, y, x)
                        loop
                    EventSpecialKey KeyRightArrow -> do
                        modVisualPanel $ \(h, w, y, x) -> (h, w+1, y, x)
                        loop
                    EventSpecialKey KeyUpArrow -> do
                        modVisualPanel $ \(h, w, y, x) -> (h-1, w, y, x)
                        loop
                    EventSpecialKey KeyDownArrow -> do
                        modVisualPanel $ \(h, w, y, x) -> (h+1, w, y, x)
                        loop
                    EventSpecialKey (KeyFunction 1) -> do
                        colorPrompt stateRef
                        loop
                    EventSpecialKey (KeyFunction 2) -> do
                        glyphPrompt stateRef
                        loop
                    EventCharacter 'b' -> do
                        pushUndo stateRef
                        updateVisualSelection $ drawBox Nothing Nothing
                        loop
                    EventCharacter 'B' -> do
                        evProbe <- getEvent win Nothing
                        case evProbe of
                            Nothing -> loop
                            Just (EventCharacter ch) -> do
                                pushUndo stateRef
                                updateVisualSelection $ do
                                    let glyph = Just $ Glyph ch [AttributeColor (_currentColor st)]
                                    drawBorder glyph glyph glyph glyph glyph glyph glyph glyph
                                loop
                            _ -> loop
                    EventCharacter 'd' -> do
                        pushUndo stateRef
                        updateVisualSelection $ clear
                        loop
                    EventCharacter 'p' -> do
                        pushUndo stateRef
                        updateVisualSelection $ do
                            setBackground $ Glyph (_currentChar st) [AttributeColor (_currentColor st)]
                            clear
                        loop
                    EventCharacter 'f' -> do
                        evProbe <- getEvent win Nothing
                        case evProbe of
                            Nothing -> loop
                            Just (EventCharacter ch) -> do
                                pushUndo stateRef
                                updateVisualSelection $ do
                                    setBackground $ Glyph ch [AttributeColor (_currentColor st)]
                                    clear
                                loop
                            _ -> loop
                    EventCharacter 'y' -> do
                        rect <- getPanelWindow (_visualPanel st) >>= getWindowRect
                        yankAction rect stateRef
                        loop
                    EventCharacter 'u' -> popUndo stateRef >> loop
                    _ -> loop

prompt :: ((Integer, Integer) -> Curses ()) -> (AsciiDrawState -> Panel) -> IORef AsciiDrawState -> Curses ()
prompt fn panel stateRef = do
    st <- liftIO $ readIORef stateRef
    oldCursorMode <- setCursorMode CursorVisible
    showPanel $ panel st
    raisePanel $ _cursorPanel st
    loop
    hidePanel $ panel st
    setCursorMode oldCursorMode
    return ()
    where
        loop = do
            st <- liftIO $ readIORef stateRef
            w <- defaultWindow
            moveCursorPanel (_cursorPanel st) (panel st)
            refreshPanels
            render
            panelWin <- getPanelWindow $ panel st
            ev <- getEvent w Nothing
            case ev of
                Nothing -> loop
                Just ev' -> case ev' of
                    EventCharacter 'q' -> do
                        return ()
                    --EventSpecialKey KeyEnter -> do
                    EventCharacter '\n' -> do
                        updateWindow panelWin cursorPosition >>= fn
                    EventSpecialKey KeyLeftArrow -> do
                        safeMoveCursor DirLeft True (panel st)
                        loop
                    EventSpecialKey KeyRightArrow -> do
                        safeMoveCursor DirRight True (panel st)
                        loop
                    EventSpecialKey KeyUpArrow -> do
                        safeMoveCursor DirUp True (panel st)
                        loop
                    EventSpecialKey KeyDownArrow -> do
                        safeMoveCursor DirDown True (panel st)
                        loop
                    _ -> loop

colorPrompt stateRef = prompt selectCol _colorPanel stateRef
    where
        selectCol (y, x) = do
            let color = ColorID . fromIntegral $ (y-1) * 16 + (x-1)
            liftIO $ modifyIORef stateRef (set currentColor color)

glyphPrompt stateRef = prompt selectChar _glyphPanel stateRef
    where
        selectChar :: (Integer, Integer) -> Curses ()
        selectChar (y, x) = do
            let ch = codepage437 !! (fromIntegral ((y-1) * 16 + (x-1)))
            liftIO $ modifyIORef stateRef (set currentChar ch)

runAsciiDraw :: Curses ()
runAsciiDraw = do
    setEcho False
    resizeTerminal 24 80
    oldColors <- initDefaultColors

    st <- ADS
        <$> mkColorPicker
        <*> mkGlyphPicker
        <*> (newWindow 24 80 0 0 >>= newPanel) -- Image
        <*> (newWindow 1 80 23 0 >>= newPanel) -- Editor status bar
        <*> (newWindow 1 80 23 0 >>= newPanel) -- Visual
        <*> mkCursorPanel -- Cursor
        <*> findColorID colorBrightRed colorBlack
        <*> pure 'X'
        <*> findColorID colorDarkGreen colorBrightYellow  -- Normal
        <*> findColorID colorBrightBlue colorBrightCyan -- Insert
        <*> findColorID colorBlack colorBrightRed -- Visual
        <*> newWindow 24 80 0 0 -- Yank window
        <*> pure []

    stRef <- liftIO $ newIORef st

    raisePanel $ _imagePanel st
    raisePanel $ _editorPanel st
    raisePanel $ _cursorPanel st
    hidePanel $ _colorPanel st
    hidePanel $ _glyphPanel st

    normalMode stRef
    restoreColors oldColors

main :: IO ()
main = do
    --setLocale LC_ALL (Just "en-US.UTF-8")
    runCurses runAsciiDraw


waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop


