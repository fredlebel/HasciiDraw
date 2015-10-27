{-# LANGUAGE RankNTypes #-}

--import System.IO
--import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad
import Data.Word
import Data.IORef
--import System.Locale.SetLocale
import UI.NCurses
import UI.NCurses.Panel
--import Control.Lens

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

mkGlyphPicker :: Curses Panel
mkGlyphPicker = do
    win <- newWindow 18 18 1 1
    updateWindowNoRefresh win $ do
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
    updateWindowNoRefresh win $ do
        clear
        drawBox Nothing Nothing
        forM_ [0..255] $ \n -> do
            moveCursor (n `div` 16 + 1) (n `mod` 16 + 1)
            drawGlyph (Glyph 'M' [AttributeColor . ColorID . fromIntegral $ n])
        moveCursor 8 8
    newPanel win

mkCursorPanel = do
    curWin <- newWindow 1 1 12 40
    updateWindowNoRefresh curWin $ do
        moveCursor 0 0
    curPan <- newPanel curWin
    return curPan

data AsciiDrawState = ADS
    { _colorWin :: Panel
    , _glyphWin :: Panel
    , _imageWin :: Panel
    , _editorWin :: Panel
    , _cursorPanel :: Panel
    , _currentColor :: ColorID
    , _currentChar :: Char
    , _colorNormal :: ColorID
    , _colorInsert :: ColorID
    }

newtype Box = Box (Integer, Integer, Integer, Integer)

getWindowBox :: Bool -> Window -> Curses Box
getWindowBox hasBorder win = do
    (h, w) <- updateWindowNoRefresh win windowSize
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
    newPos <- (shiftDirection dir) <$> updateWindowNoRefresh win cursorPosition
    when (inBox box newPos) $
        updateWindowNoRefresh win (uncurry moveCursor newPos)

moveCursorPanel :: Panel -> Panel -> Curses ()
moveCursorPanel curPan hostPan = do
    hostWin <- getPanelWindow hostPan
    (_, _, y, x) <- getWindowRect hostWin
    (cy, cx) <- updateWindowNoRefresh hostWin cursorPosition
    movePanel curPan (cy + y) (cx + x)

updateCurrentChar c stateRef = do
    st <- liftIO $ readIORef stateRef
    liftIO $ writeIORef stateRef (st {_currentChar = c})

insertMode :: IORef AsciiDrawState -> Curses ()
insertMode stateRef = do
    st <- liftIO $ readIORef stateRef
    editorWin <- getPanelWindow $ _editorWin st
    updateWindowNoRefresh editorWin $ do
       clear
       setColor (_colorInsert st)
       moveCursor 0 0
       drawString "INSERT"

    normalLoop

    where
        normalLoop = do
            st <- liftIO $ readIORef stateRef
            w <- defaultWindow
            moveCursorPanel (_cursorPanel st) (_imageWin st)
            refreshPanels
            render
            ev <- getEvent w Nothing
            imageWin <- getPanelWindow $ _imageWin st
            case ev of
                Nothing -> normalLoop
                Just ev' -> case ev' of
                    EventCharacter '\x001B' -> do -- ESC or ALT
                        evProbe <- getEvent w (Just 0)
                        case evProbe of
                            Nothing -> normalMode stateRef
                            Just _ -> normalLoop
                    EventCharacter c -> do
                        updateWindowNoRefresh imageWin $ do
                            drawGlyph $ Glyph c [AttributeColor (_currentColor st)]
                        updateCurrentChar c stateRef
                        normalLoop
                    EventSpecialKey KeyLeftArrow -> do
                        safeMoveCursor DirLeft False (_imageWin st)
                        normalLoop
                    EventSpecialKey KeyRightArrow -> do
                        safeMoveCursor DirRight False (_imageWin st)
                        normalLoop
                    EventSpecialKey KeyUpArrow -> do
                        safeMoveCursor DirUp False (_imageWin st)
                        normalLoop
                    EventSpecialKey KeyDownArrow -> do
                        safeMoveCursor DirDown False (_imageWin st)
                        normalLoop
                    EventSpecialKey (KeyFunction 1) -> do
                        colorPrompt stateRef
                        normalLoop
                    _ -> normalLoop

normalMode :: IORef AsciiDrawState -> Curses ()
normalMode stateRef = do
    st <- liftIO $ readIORef stateRef
    editorWin <- getPanelWindow $ _editorWin st
    updateWindowNoRefresh editorWin $ do
       clear
       setColor (_colorNormal st)
       moveCursor 0 0
       drawString "NORMAL"

    normalLoop

    where
        normalLoop = do
            st <- liftIO $ readIORef stateRef
            w <- defaultWindow
            moveCursorPanel (_cursorPanel st) (_imageWin st)
            refreshPanels
            render
            ev <- getEvent w Nothing
            case ev of
                Nothing -> normalLoop
                Just ev' -> case ev' of
                    EventCharacter 'q' -> return ()
                    EventCharacter 'i' -> insertMode stateRef
                    EventSpecialKey KeyLeftArrow -> do
                        safeMoveCursor DirLeft False (_imageWin st)
                        normalLoop
                    EventSpecialKey KeyRightArrow -> do
                        safeMoveCursor DirRight False (_imageWin st)
                        normalLoop
                    EventSpecialKey KeyUpArrow -> do
                        safeMoveCursor DirUp False (_imageWin st)
                        normalLoop
                    EventSpecialKey KeyDownArrow -> do
                        safeMoveCursor DirDown False (_imageWin st)
                        normalLoop
                    _ -> normalLoop

colorPrompt :: IORef AsciiDrawState -> Curses ()
colorPrompt stateRef = do
    st <- liftIO $ readIORef stateRef
    showPanel $ _colorWin st
    raisePanel $ _cursorPanel st
    loop
    hidePanel $ _colorWin st
    where
        loop = do
            st <- liftIO $ readIORef stateRef
            w <- defaultWindow
            moveCursorPanel (_cursorPanel st) (_colorWin st)
            refreshPanels
            render
            ev <- getEvent w Nothing
            case ev of
                Nothing -> loop
                Just ev' -> case ev' of
                    EventCharacter 'q' -> do
                        return ()
                    --EventSpecialKey KeyEnter -> do
                    EventCharacter '\n' -> do
                        colorWin <- getPanelWindow $ _colorWin st
                        (y, x) <- updateWindowNoRefresh colorWin cursorPosition
                        let color = ColorID . fromIntegral $ (y-1) * 16 + (x-1)
                        liftIO $ writeIORef stateRef (st {_currentColor = color})
                    EventSpecialKey KeyLeftArrow -> do
                        safeMoveCursor DirLeft True (_colorWin st)
                        loop
                    EventSpecialKey KeyRightArrow -> do
                        safeMoveCursor DirRight True (_colorWin st)
                        loop
                    EventSpecialKey KeyUpArrow -> do
                        safeMoveCursor DirUp True (_colorWin st)
                        loop
                    EventSpecialKey KeyDownArrow -> do
                        safeMoveCursor DirDown True (_colorWin st)
                        loop
                    _ -> loop

runAsciiDraw :: Curses ()
runAsciiDraw = do
    setEcho False
    resizeTerminal 24 80
    oldColors <- initDefaultColors

    st <- ADS
        <$> mkColorPicker
        <*> mkGlyphPicker
        <*> (newWindow 24 80 0 0 >>= newPanel)
        <*> (newWindow 1 80 23 0 >>= newPanel)
        <*> mkCursorPanel
        <*> newColorID colorBrightRed colorBlack 1
        <*> pure 'X'
        <*> newColorID colorDarkGreen colorBlack 2
        <*> newColorID colorBrightBlue colorBlack 3

    stRef <- liftIO $ newIORef st

    raisePanel $ _imageWin st
    raisePanel $ _editorWin st
    raisePanel $ _cursorPanel st
    hidePanel $ _colorWin st
    hidePanel $ _glyphWin st

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


