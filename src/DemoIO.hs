{-# LANGUAGE RecordWildCards #-}
module DemoIO
    ( run
    ) where

import System.Random
import System.IO
import Data.Tuple.Select
import Data.Tuple.Update
import Data.List
import Data.Vector hiding (zip, map)
--import Data.Array

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment

--------------
-- Data types.
--------------

-- Пункт
data CheckerPoint = CheckerPoint
    {
        stcoord :: (Float, Float),  -- Координата начала пункта в окне
        dircoord :: Bool            -- Направление пункта - вверх или вниз
    }

takeCurrentCheckerPoint :: CheckerPoint -> CheckerPoint
takeCurrentCheckerPoint x = x

takeXCoordsCheckerPoint :: CheckerPoint -> Float
takeXCoordsCheckerPoint chPoint = fst (stcoord chPoint)

takeYCoordsCheckerPoint :: CheckerPoint -> Float
takeYCoordsCheckerPoint chPoint = snd (stcoord chPoint)

takeDirectionCheckerPoint :: CheckerPoint -> Bool
takeDirectionCheckerPoint chPoint = dircoord chPoint

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
        checkerColor :: Color,  -- Цвет шашек
        checkers :: [Checker]   -- Шашки игрока
    }
    
takePlayerColor :: PlayerInfo -> Color
takePlayerColor pl = checkerColor pl

--takeCheckPlayer :: PlayerInfo -> Int -> Checker
--takeCheckPlayer pl n = if n == 1 then sel1 (checkers pl) else sel2 (checkers pl)
    
-- Игровая доска
data GameBoard = GameBoard
    {
        coordBoard   :: (Float, Float, Float, Float),   -- Координаты области доски
        pointsBoard  :: [CheckerPoint],                 -- Пункты на доске
        pointsLength :: Float                           -- Длина пунктов на доске
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
baseStatePlayerOne = PlayerInfo (makeColorI 215 215 215 255)((Checker True 1 6), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1) )

baseStatePlayerTwo :: PlayerInfo
baseStatePlayerTwo = PlayerInfo (makeColorI 22 53 55 255)((Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1), (Checker True 1 1) )

baseStateGameBoard :: GameBoard
baseStateGameBoard = GameBoard (-900.0, 500.0, 900.0, -500.0) (CheckerPoint (840.0, 500.0) True, CheckerPoint (720.0, 500.0) True, CheckerPoint (600.0, 500.0) True, CheckerPoint (480.0, 500.0) True, CheckerPoint (360.0, 500.0) True, CheckerPoint (240.0, 500.0) True, CheckerPoint (-240.0, 500.0) True, CheckerPoint (-360.0, 500.0) True, CheckerPoint (-480.0, 500.0) True, CheckerPoint (-600.0, 500.0) True, CheckerPoint (-720.0, 500.0) True, CheckerPoint (-840.0, 500.0) True, CheckerPoint (-840.0, -500.0) False,CheckerPoint (-720.0, -500.0) False,CheckerPoint (-600.0, -500.0) False,CheckerPoint (-480.0, -500.0) False,CheckerPoint (-360.0, -500.0) False,CheckerPoint (-240.0, -500.0) False,CheckerPoint (240.0, -500.0) False,CheckerPoint (360.0, -500.0) False,CheckerPoint (480.0, -500.0) False, CheckerPoint (600.0, -500.0) False, CheckerPoint (720.0, -500.0) False, CheckerPoint (840.0, -500.0) False) 400 

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
-- Отрисовка игрового поля и шашек игроков
drawGameApp :: AppDataState -> Picture
drawGameApp (AppDataState dscale pone ptwo (GameBoard (xo, yo, xt, yt) dGApointsBoard dGAPointsLen)) = Pictures[allScreen]
    where
        --(pb1, pb2, pb3, pb4, pb5, pb6, pb7, pb8, pb9, pb10, pb11, pb12, pb13, pb14, pb15, pb16, pb17, pb18, pb19, pb20, pb21, pb22, pb23, pb24) = dGApointsBoard
        
        
        picTriangles = map func dGApointsBoard
            where func = getaPictureChekerPoint dscale dGAPointsLen 
        
        --pictriangle1  = Color (makeColorI 135 67 8 255)     (getaPictureChekerPoint pb1 dscale dGAPointsLen)
        --pictriangle2  = Color (makeColorI 249 214 184 255)  (getaPictureChekerPoint pb2 dscale dGAPointsLen)
        --pictriangle3  = Color (makeColorI 135 67 8 255)     (getaPictureChekerPoint pb3 dscale dGAPointsLen)
        --pictriangle4  = Color (makeColorI 249 214 184 255)  (getaPictureChekerPoint pb4 dscale dGAPointsLen)
        --pictriangle5  = Color (makeColorI 135 67 8 255)     (getaPictureChekerPoint pb5 dscale dGAPointsLen)
        --pictriangle6  = Color (makeColorI 249 214 184 255)  (getaPictureChekerPoint pb6 dscale dGAPointsLen)
        --pictriangle7  = Color (makeColorI 135 67 8 255)     (getaPictureChekerPoint pb7 dscale dGAPointsLen)
        --pictriangle8  = Color (makeColorI 249 214 184 255)  (getaPictureChekerPoint pb8 dscale dGAPointsLen)
        --pictriangle9  = Color (makeColorI 135 67 8 255)     (getaPictureChekerPoint pb9 dscale dGAPointsLen)
        --pictriangle10 = Color (makeColorI 249 214 184 255)  (getaPictureChekerPoint pb10 dscale dGAPointsLen)
        --pictriangle11 = Color (makeColorI 135 67 8 255)     (getaPictureChekerPoint pb11 dscale dGAPointsLen)
        --pictriangle12 = Color (makeColorI 249 214 184 255)  (getaPictureChekerPoint pb12 dscale dGAPointsLen)
        --pictriangle13 = Color (makeColorI 135 67 8 255)     (getaPictureChekerPoint pb13 dscale dGAPointsLen)
        --pictriangle14 = Color (makeColorI 249 214 184 255)  (getaPictureChekerPoint pb14 dscale dGAPointsLen)
        --pictriangle15 = Color (makeColorI 135 67 8 255)     (getaPictureChekerPoint pb15 dscale dGAPointsLen)
        --pictriangle16 = Color (makeColorI 249 214 184 255)  (getaPictureChekerPoint pb16 dscale dGAPointsLen)
        --pictriangle17 = Color (makeColorI 135 67 8 255)     (getaPictureChekerPoint pb17 dscale dGAPointsLen)
        --pictriangle18 = Color (makeColorI 249 214 184 255)  (getaPictureChekerPoint pb18 dscale dGAPointsLen)
        --pictriangle19 = Color (makeColorI 135 67 8 255)     (getaPictureChekerPoint pb19 dscale dGAPointsLen)
        --pictriangle20 = Color (makeColorI 249 214 184 255)  (getaPictureChekerPoint pb20 dscale dGAPointsLen)
        --pictriangle21 = Color (makeColorI 135 67 8 255)     (getaPictureChekerPoint pb21 dscale dGAPointsLen)
        --pictriangle22 = Color (makeColorI 249 214 184 255)  (getaPictureChekerPoint pb22 dscale dGAPointsLen)
        --pictriangle23 = Color (makeColorI 135 67 8 255)     (getaPictureChekerPoint pb23 dscale dGAPointsLen)
        --pictriangle24 = Color (makeColorI 249 214 184 255)  (getaPictureChekerPoint pb24 dscale dGAPointsLen)
        
        picboard = Color (makeColorI 91 58 41 255) (Polygon [(dscale * xo, dscale * yo), (dscale * xo, dscale * yt), (dscale * xt, dscale * yt), (dscale * xt, dscale * yo), (dscale * xo,dscale * yo)])
        
        picBar = Color (makeColorI 118 60 40 255) (Polygon [(dscale * (-100), dscale * yo), (dscale * 100, dscale * yo), (dscale * 100, dscale * yt), (dscale * (-100), dscale * yt), (dscale * (-100), dscale * yo)])
        
        picCheckPO1 = if takeCurPointChecker (takeCheckPlayer pone 1) == 1 then Translate (dscale * takeXCoordsCheckerPoint pb1) (dscale * (takeYCoordsCheckerPoint pb1 - (50 * fromIntegral (takePosPointChecker (takeCheckPlayer pone 1))))) (ThickCircle 10 80) else Translate (dscale * takeXCoordsCheckerPoint pb2) (dscale * takeYCoordsCheckerPoint pb2) (ThickCircle 40 40)
        
        allScreen = Pictures [picboard, picBar,  pictriangle1, pictriangle2, pictriangle3, pictriangle4, pictriangle5, pictriangle6, pictriangle7, pictriangle8, pictriangle9, pictriangle10, pictriangle11, pictriangle12, pictriangle13, pictriangle14, pictriangle15, pictriangle16, pictriangle17, pictriangle18, pictriangle19, pictriangle20, pictriangle21, pictriangle22, pictriangle23, pictriangle24, picCheckPO1]

-- Обработчик событий
handleGameEvent :: Event -> AppDataState -> AppDataState
handleGameEvent _ state = state -- Ничего не делаем

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