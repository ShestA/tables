{-# LANGUAGE RecordWildCards #-}
module DemoIO
    ( run
    ) where

import System.Random
import System.IO
import Data.Tuple.Select
import Data.Tuple.Update
import Data.List
--import Data.Vector hiding (zip, map)
--import Data.Array

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment

--------------
-- Data types.
--------------

-- Пункт
data CheckerPoint = CheckerPoint
    {
        stcoord     :: (Float, Float),  -- Координата начала пункта в окне
        dircoord    :: Bool,             -- Направление пункта - вверх или вниз
        curNumber   :: Int              -- Текущее количество шашек в пункте
    }

takeXCoordsCheckerPoint :: CheckerPoint -> Float
takeXCoordsCheckerPoint chPoint = fst (stcoord chPoint)

takeYCoordsCheckerPoint :: CheckerPoint -> Float
takeYCoordsCheckerPoint chPoint = snd (stcoord chPoint)

takeDirectionCheckerPoint :: CheckerPoint -> Bool
takeDirectionCheckerPoint chPoint = dircoord chPoint

takeNumberCheckerPoint :: CheckerPoint -> Int
takeNumberCheckerPoint chPoint = curNumber chPoint

getaPictureChekerPoint :: Float -> Float -> CheckerPoint -> Picture
getaPictureChekerPoint getdscale getdGAPointsLen pb = if getDirection then Polygon [(getdscale * (xpbcoord - 60), getdscale * ypbcoord), (getdscale * (xpbcoord + 60), getdscale * ypbcoord), (getdscale * xpbcoord, getdscale * (ypbcoord - getdGAPointsLen)), (getdscale * (xpbcoord - 60), getdscale * ypbcoord)] else Polygon [(getdscale * (xpbcoord - 60), getdscale * ypbcoord), (getdscale * (xpbcoord + 60), getdscale * ypbcoord), (getdscale * xpbcoord, getdscale * (ypbcoord + getdGAPointsLen)), (getdscale * (xpbcoord - 60), getdscale * ypbcoord)] where
    xpbcoord = takeXCoordsCheckerPoint pb
    ypbcoord = takeYCoordsCheckerPoint pb
    getDirection = takeDirectionCheckerPoint pb

-- Шашка
data Checker = Checker
    {
        ingame   :: Bool,   -- Шашка на доске или на баре?
        curpoint :: Int,    -- Текущий номер пункта (Нумерация внутренняя, не игровая)
        pospoint :: Int     -- Позиция в пункте
    }
 
takeInGameInfoChecker :: Checker -> Bool
takeInGameInfoChecker ch = ingame ch

takeCurPointChecker :: Checker -> Int
takeCurPointChecker ch = curpoint ch

takePosPointChecker :: Checker -> Int
takePosPointChecker ch = pospoint ch
    
-- Игрок    
data PlayerInfo = PlayerInfo
    {
        checkerColor    :: Color,       -- Цвет шашек
        checkers        :: [(Checker, Float, Float, Bool)]    -- Шашки игрока
    }
    
takePlayerColor :: PlayerInfo -> Color
takePlayerColor pl = checkerColor pl

takeCheckersPlayer :: PlayerInfo -> [(Checker, Float, Float, Bool)]
takeCheckersPlayer pl = checkers pl
    
-- Игровая доска
data GameBoard = GameBoard
    {
        coordBoard          :: (Float, Float, Float, Float),   -- Координаты области доски
        redPointsBoard      :: [CheckerPoint],                 -- Пункты на доске
        whitePointsBoard    :: [CheckerPoint], 
        pointsLength        :: Float                           -- Длина пунктов на доске
    }
  
-- Состояния параметров приложения
data AppDataState = AppDataState
    {
        drawScale :: Float,         -- Масштаб отрисовки изображения
        playerOne :: PlayerInfo,    -- Данные первого игрока
        playerTwo :: PlayerInfo,    -- Данные второго игрока
        appBoard  :: GameBoard      -- Данные игровой доски
    }
{- 
data ColorConfig = ColorConfig
  { color1 :: Color -- Color for first number.
  , color2 :: Color -- Color for second number.
  }



-- General application state.
data AppState = AppState
  { colors    :: ColorConfig -- Colors config.
  , randomGen :: StdGen      -- Random number generator.
  , number    :: Int         -- Current number.
  , angler    :: Float
  }
-}
-------------
-- Constants.
-------------

baseStatePlayerOne :: PlayerInfo
baseStatePlayerOne = PlayerInfo (makeColorI 215 215 215 255)[(Checker True 1 1, 0, 0, True), (Checker True 1 2, 0, 0, True),
                                                            (Checker True 12 1, 0, 0, True), (Checker True 12 2, 0, 0, True),
                                                            (Checker True 12 3, 0, 0, True), (Checker True 12 4, 0, 0, True),
                                                            (Checker True 12 5, 0, 0, True), (Checker True 17 1, 0, 0, True),
                                                            (Checker True 17 2, 0, 0, True), (Checker True 17 3, 0, 0, True),
                                                            (Checker True 19 1, 0, 0, True), (Checker True 19 2, 0, 0, True),
                                                            (Checker True 19 3, 0, 0, True), (Checker True 19 4, 0, 0, True),
                                                            (Checker True 19 5, 0, 0, True)]

baseStatePlayerTwo :: PlayerInfo
baseStatePlayerTwo = PlayerInfo (makeColorI 22 53 55 255) [(Checker True 6 1, 0, 0, True),   (Checker True 6 2, 0, 0, True),
                                                            (Checker True 6 3, 0, 0, True),  (Checker True 6 4, 0, 0, True),
                                                            (Checker True 6 5, 0, 0, True),  (Checker True 8 1, 0, 0, True),
                                                            (Checker True 8 2, 0, 0, True),  (Checker True 8 3, 0, 0, True),
                                                            (Checker True 13 1, 0, 0, True), (Checker True 13 2, 0, 0, True),
                                                            (Checker True 13 3, 0, 0, True), (Checker True 13 4, 0, 0, True),
                                                            (Checker True 13 5, 0, 0, True), (Checker True 24 1, 0, 0, True),
                                                            (Checker True 24 2, 0, 0, True)]

baseStateGameBoard :: GameBoard
baseStateGameBoard = GameBoard (-900.0, 500.0, 900.0, -500.0) [
                                                            CheckerPoint (840.0, 500.0)     True 2,
                                                            CheckerPoint (600.0, 500.0)     True 0,
                                                            CheckerPoint (360.0, 500.0)     True 0,
                                                            CheckerPoint (-240.0, 500.0)    True 0,
                                                            CheckerPoint (-480.0, 500.0)    True 0,
                                                            CheckerPoint (-720.0, 500.0)    True 0,
                                                            CheckerPoint (-840.0, -500.0)   False 5,
                                                            CheckerPoint (-600.0, -500.0)   False 0,
                                                            CheckerPoint (-360.0, -500.0)   False 3,
                                                            CheckerPoint (240.0, -500.0)    False 5,
                                                            CheckerPoint (480.0, -500.0)    False 5,
                                                            CheckerPoint (720.0, -500.0)    False 0] [
                                                            CheckerPoint (720.0, 500.0)     True 0,
                                                            CheckerPoint (480.0, 500.0)     True 0,
                                                            CheckerPoint (240.0, 500.0)     True 5,
                                                            CheckerPoint (-360.0, 500.0)    True 3,
                                                            CheckerPoint (-600.0, 500.0)    True 0,
                                                            CheckerPoint (-840.0, 500.0)    True 5,
                                                            CheckerPoint (-720.0, -500.0)   False 0,
                                                            CheckerPoint (-480.0, -500.0)   False 0,
                                                            CheckerPoint (-240.0, -500.0)   False 0,
                                                            CheckerPoint (360.0, -500.0)    False 0,
                                                            CheckerPoint (600.0, -500.0)    False 0,
                                                            CheckerPoint (840.0, -500.0)    False 2] 400 

radiusChecker :: Float
radiusChecker = 80

-- Path to config file.
configPath :: FilePath
configPath = "config.txt"

-- Random numbers range.
range :: (Int, Int)
range = (-10, 10)

-- Game display mode.
display :: Display
display = FullScreen

-- Background color.
bgColor :: Color
bgColor = makeColorI 118 60 40 255

-- Simulation steps per second.
fps :: Int
fps = 60

-- Text shift on screen.
textShift :: Float
textShift = 250

------------------
-- Pure functions.
------------------
{-
-- Parse config from string.
-- Config format: 2 lines, one color per line.
parseConfig :: String -> Maybe ColorConfig
parseConfig str = case map findColor (lines str) of
  [Just c1, Just c2] -> Just (ColorConfig c1 c2)
  _ -> Nothing
  where
    findColor :: String -> Maybe Color
    findColor s = lookup s colorMap
    colorMap = zip names colors
    colors = [red, green, blue, black]
    names  = ["red", "green", "blue", "black"]
-}

isOnChecker :: Float -> Float -> Float -> (Checker, Float, Float, Bool) -> (Checker, Float, Float, Bool)
isOnChecker xMouse yMouse iocScale (chk, x, y, onPos) = (chk, x, y, newonPos) where
    tmpX        = x + (iocScale * 960)
    tmpY        = y + (iocScale * 540)
    tmpXMouse   = (iocScale * (xMouse + 960))
    tmpYMouse   = (iocScale * (yMouse + 540))
    hitBox      = (iocScale * radiusChecker)
    newonPos    = if ((abs(tmpX - tmpXMouse) < hitBox) && (abs(tmpY - tmpYMouse) < hitBox)) 
        then 
            False
        else
            True
            
isOnMoveChecker :: Float -> Float -> Float -> (Checker, Float, Float, Bool) -> (Checker, Float, Float, Bool)
isOnMoveChecker xMouse yMouse iocScale (chk, x, y, onPos) = (chk, newx, newy, onPos) where
    (newx, newy) = if (not onPos)
        then 
            (xMouse, yMouse)
        else
            (x, y)

getChkPic :: [CheckerPoint] -> [CheckerPoint] -> Float -> Float -> Color -> (Checker, Float, Float, Bool) -> Picture
getChkPic _ _ _ fScale fColorChk (_, xChck, yChck, _) = pic 
    where 
        pic = Color fColorChk (Translate (xChck) (yChck) (ThickCircle (fScale * 10) (fScale * 80)) )

transChkPic -> [CheckerPoint] -> [CheckerPoint] -> Float -> Float -> (Checker, Float, Float, Bool) -> (Checker, Float, Float, Bool)
transChkPic fRedPointsBoard fWhitePointsBoard fPointLen fScale (fCheckerCur, xChck, yChck, onPos) -> (fCheckerCur, newxChck, newyChck, onPos)
    where
        fCursorPoint = curpoint fCheckerCur
        fRealCur = if even fCursorPoint then
            (div fCursorPoint 2) - 1
        else
            (div fCursorPoint 2) + 12
        fPointsBoard = fWhitePointsBoard ++ fRedPointsBoard
        fCheckerPoint = fPointsBoard !! fRealCur
        newxChck = if onPos then fScale * (takeXCoordsCheckerPoint fCheckerPoint) else xChck
        newyChck = if onPos 
        then (fScale * ((takeYCoordsCheckerPoint fCheckerPoint) - if (takeDirectionCheckerPoint fCheckerPoint)
            then
                ((fPointLen / (fromIntegral(takeNumberCheckerPoint fCheckerPoint))) * fromIntegral (takePosPointChecker fCheckerCur))
            else 
                (-(fPointLen / (fromIntegral (takeNumberCheckerPoint fCheckerPoint))) * fromIntegral(takePosPointChecker fCheckerCur)))) 
        else yChck

-- Отрисовка игрового поля и шашек игроков
drawGameApp :: AppDataState -> Picture
drawGameApp (AppDataState dscale pOne pTwo (GameBoard (xo, yo, xt, yt) dGARedPointsBoard dGAWhitePointsBoard dGAPointsLen)) = Pictures [allScreen]
    where
        picRedTriangles = map func dGARedPointsBoard
            where func = getaPictureChekerPoint dscale dGAPointsLen
        picRTriangles = map func picRedTriangles
            where func = Color (makeColorI 135 67 8 255)
        picWhiteTriangles = map func dGAWhitePointsBoard
            where func = getaPictureChekerPoint dscale dGAPointsLen
        picWTriangles = map func picWhiteTriangles
            where func = Color (makeColorI 249 214 184 255)
        picTriangles = Pictures[Pictures picWTriangles, Pictures picRTriangles]
        picboard = Color (makeColorI 91 58 41 255) (Polygon [(dscale * xo, dscale * yo),
                                                            (dscale * xo, dscale * yt),
                                                            (dscale * xt, dscale * yt),
                                                            (dscale * xt, dscale * yo),
                                                            (dscale * xo,dscale * yo)])
        picBar = Color (makeColorI 118 60 40 255) (Polygon [(dscale * (-100), dscale * yo),
                                                            (dscale * 100, dscale * yo),
                                                            (dscale * 100, dscale * yt),
                                                            (dscale * (-100), dscale * yt),
                                                            (dscale * (-100), dscale * yo)])
                                                            
        picChkPlOne = map func (takeCheckersPlayer pOne)
            where
                func = getChkPic dGARedPointsBoard dGAWhitePointsBoard dGAPointsLen dscale (takePlayerColor pOne)
        picChkPlTwo = map func (takeCheckersPlayer pTwo)
            where
                func = getChkPic dGARedPointsBoard dGAWhitePointsBoard dGAPointsLen dscale (takePlayerColor pTwo)
        allScreen = Pictures [picboard, picBar, picTriangles, Pictures picChkPlOne , Pictures picChkPlTwo]

-- Обработчик событий
handleGameEvent :: Event -> AppDataState -> AppDataState
handleGameEvent (EventMotion (xMouse, yMouse)) (AppDataState drawScale pOne pTwo hGEGBoard) = state where
    hGEChcksOne = map func (takeCheckersPlayer pOne)
        where
            func = isOnMoveChecker xMouse yMouse drawScale
    hGEpOne = PlayerInfo (takePlayerColor pOne) hGEChcksOne
    hGEChcksTwo = map func (takeCheckersPlayer pTwo)
        where
            func = isOnMoveChecker xMouse yMouse drawScale
    hGEpTwo = PlayerInfo (takePlayerColor pTwo) hGEChcksTwo
    state = AppDataState drawScale hGEpOne hGEpTwo hGEGBoard
handleGameEvent (EventKey (MouseButton LeftButton) Down _ (xMouse, yMouse)) (AppDataState drawScale pOne pTwo hGEGBoard) = state where
    hGEChcksOne = map func (takeCheckersPlayer pOne)
        where
            func = isOnChecker xMouse yMouse drawScale
    hGEpOne = PlayerInfo (takePlayerColor pOne) hGEChcksOne
    hGEChcksTwo = map func (takeCheckersPlayer pTwo)
        where
            func = isOnChecker xMouse yMouse drawScale
    hGEpTwo = PlayerInfo (takePlayerColor pTwo) hGEChcksTwo
    state = AppDataState drawScale hGEpOne hGEpTwo hGEGBoard
handleGameEvent _ state = state

-- Обработчик кадра
updateGameApp :: Float -> AppDataState -> AppDataState
updateGameApp _ x = x
{-
-- Draw a picture: two numbers of different colors defined in config.
drawApp :: AppState -> Picture
drawApp (AppState ColorConfig{..} _ n a) = Pictures [pic3, pic2]
  where
    txt = Text (show n)
    pic1 = Translate (-textShift) 0 $ Color color1 txt
    pic2 = Translate textShift 0 $ Color color2 txt
    pic3 = Rotate a pic1

-- Handle events.
handleEvent :: Event -> AppState -> AppState
-- Generate new random number when Space is pressed.
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) AppState{..} =
  -- Get new random number and generator.
  let (newn, newGen) = randomR range randomGen
  -- Update BOTH number AND generator.
  in AppState colors newGen newn angler
-- Ignore all other events.
handleEvent _ state = state

-- Simulation step (updates nothing).
updateApp :: Float -> AppState -> AppState
updateApp _ AppState{..} = 
    let newangler = angler + 1
    in AppState colors randomGen number newangler
-}
------------------------------
-- Main function for this app.
------------------------------

-- Run game. This is the ONLY unpure function.
run :: IO ()
run = do
    ds <- getScreenSize                   -- Получение размеров экрана в формате IO(Int, Int)
    let tmp = fst ds                      -- Взятие ширины экрана
    let ds = fromIntegral(tmp) / 1920.0   -- Расчет масштабного коэффициента
    print ds                              -- Вывод масштабного коэффициента на экран
    let initGameState = AppDataState ds baseStatePlayerOne baseStatePlayerTwo baseStateGameBoard
    play display bgColor fps initGameState drawGameApp handleGameEvent updateGameApp
{-
  -- Load config file contents (unpure action).
  str <- readFile configPath
  -- Try to parse config.
  case parseConfig str of
    Nothing -> putStrLn "Parse error"
    Just cfg -> do
      -- Get new random number generator (unpure action).
      gen <- getStdGen
      let initState = AppState cfg gen 0 0
      ds <- getScreenSize                   -- Получение размеров экрана в формате IO(Int, Int)
      let tmp = fst ds                      -- Взятие ширины экрана
      let ds = fromIntegral(tmp) / 1920.0   -- Расчет масштабного коэффициента
      print ds                              -- Вывод масштабного коэффициента на экран
      let initGameState = AppDataState ds (PlayerInfo (makeColorI 215 215 215 255)((Checker True 1 6), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1) )) (PlayerInfo (makeColorI 22 53 55 255)((Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1) )) (GameBoard (-900.0, 500.0, 900.0, -500.0) (CheckerPoint (840.0, 500.0) True, CheckerPoint (720.0, 500.0) True, CheckerPoint (600.0, 500.0) True, CheckerPoint (480.0, 500.0) True, CheckerPoint (360.0, 500.0) True, CheckerPoint (240.0, 500.0) True, CheckerPoint (-240.0, 500.0) True, CheckerPoint (-360.0, 500.0) True, CheckerPoint (-480.0, 500.0) True, CheckerPoint (-600.0, 500.0) True, CheckerPoint (-720.0, 500.0) True, CheckerPoint (-840.0, 500.0) True, CheckerPoint (-840.0, -500.0) False,CheckerPoint (-720.0, -500.0) False,CheckerPoint (-600.0, -500.0) False,CheckerPoint (-480.0, -500.0) False,CheckerPoint (-360.0, -500.0) False,CheckerPoint (-240.0, -500.0) False,CheckerPoint (240.0, -500.0) False,CheckerPoint (360.0, -500.0) False,CheckerPoint (480.0, -500.0) False, CheckerPoint (600.0, -500.0) False, CheckerPoint (720.0, -500.0) False, CheckerPoint (840.0, -500.0) False) 400)
      -- Run application.
      play display bgColor fps initGameState drawGameApp handleGameEvent updateGameApp
      -}