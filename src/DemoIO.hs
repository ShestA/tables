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
        checkers        :: [Checker]    -- Шашки игрока
    }
    
takePlayerColor :: PlayerInfo -> Color
takePlayerColor pl = checkerColor pl

takeCheckersPlayer :: PlayerInfo -> [Checker]
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
baseStatePlayerOne = PlayerInfo (makeColorI 215 215 215 255)[(Checker True 1 1), (Checker True 1 2),
                                                            (Checker True 12 1), (Checker True 12 2),
                                                            (Checker True 12 3), (Checker True 12 4),
                                                            (Checker True 12 5), (Checker True 17 1),
                                                            (Checker True 17 2), (Checker True 17 3),
                                                            (Checker True 19 1), (Checker True 19 2),
                                                            (Checker True 19 3), (Checker True 19 4),
                                                            (Checker True 19 5)]

baseStatePlayerTwo :: PlayerInfo
baseStatePlayerTwo = PlayerInfo (makeColorI 22 53 55 255) [(Checker True 6 1),   (Checker True 6 2),
                                                            (Checker True 6 3),  (Checker True 6 4),
                                                            (Checker True 6 5),  (Checker True 8 1),
                                                            (Checker True 8 2),  (Checker True 8 3),
                                                            (Checker True 13 1), (Checker True 13 2),
                                                            (Checker True 13 3), (Checker True 13 4),
                                                            (Checker True 13 5), (Checker True 24 1),
                                                            (Checker True 24 2)]

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
getChkPic :: [CheckerPoint] -> [CheckerPoint] -> Float -> Float -> Color -> Checker -> Picture
getChkPic fRedPointsBoard fWhitePointsBoard fPointLen fScale fColorChk fCheckerCur = pic 
    where 
        fCursorPoint = curpoint fCheckerCur
        fRealCur = if even fCursorPoint then
            (div fCursorPoint 2) - 1
        else
            (div fCursorPoint 2) + 12
        fPointsBoard = fWhitePointsBoard ++ fRedPointsBoard
        fCheckerPoint = fPointsBoard !! fRealCur
        pic = Color fColorChk (Translate (fScale * (takeXCoordsCheckerPoint fCheckerPoint)) (fScale * ((takeYCoordsCheckerPoint fCheckerPoint) - if (takeDirectionCheckerPoint fCheckerPoint)
            then
                ( (fPointLen / (fromIntegral (takeNumberCheckerPoint fCheckerPoint))) * fromIntegral (takePosPointChecker fCheckerCur))
            else 
                (- (fPointLen / (fromIntegral (takeNumberCheckerPoint fCheckerPoint))) * fromIntegral (takePosPointChecker fCheckerCur)))) (ThickCircle (fScale * 10) (fScale * 80)) )
                
                --if Even fCursorPoint then
                --    fRealCur = fCursorPoint / 2 - 1
                --    fCheckerPoint = fWhitePointsBoard !! fRealCur
                --    pic = Translate (fScale * (takeXCoordsCheckerPoint fCheckerPoint)) (fScale * (takeYCoordsCheckerPoint fCheckerPoint) - if takeDirectionCheckerPoint fCheckerPoint then (fScale * fPointLen / takeNumberCheckerPoint fCheckerPoint * (takePosPointChecker fCheckerCur)) else (- fScale * fPointLen / takeNumberCheckerPoint fCheckerPoint * (takePosPointChecker fCheckerCur))) (ThickCircle fScale * 10 fScale * 80) 
                --else 
                --    fRealCur = fCursorPoint 'div' 2
                --    fCheckerPoint = fRedPointsBoard !! fRealCur
                --    pic = Translate (fScale * (takeXCoordsCheckerPoint fCheckerPoint)) (fScale * (takeYCoordsCheckerPoint fCheckerPoint) - if takeDirectionCheckerPoint fCheckerPoint then (fScale * fPointLen / takeNumberCheckerPoint fCheckerPoint * (takePosPointChecker fCheckerCur)) else (- fScale * fPointLen / takeNumberCheckerPoint fCheckerPoint * (takePosPointChecker fCheckerCur))) (ThickCircle fScale * 10 fScale * 80) 

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
            where func = getChkPic dGARedPointsBoard dGAWhitePointsBoard dGAPointsLen dscale (takePlayerColor pOne)
        picChkPlTwo = map func (takeCheckersPlayer pTwo)
            where func = getChkPic dGARedPointsBoard dGAWhitePointsBoard dGAPointsLen dscale (takePlayerColor pTwo)

        --picCheckPO1 = if takeCurPointChecker (takeCheckPlayer pone 1) == 1 then Translate (dscale * takeXCoordsCheckerPoint pb1) (dscale * (takeYCoordsCheckerPoint pb1 - (50 * fromIntegral (takePosPointChecker (takeCheckPlayer pone 1))))) (ThickCircle dscale * 10 dscale * 80) else Translate (dscale * takeXCoordsCheckerPoint pb2) (dscale * takeYCoordsCheckerPoint pb2) (ThickCircle 40 40)
        
        allScreen = Pictures [picboard, picBar, picTriangles, Pictures picChkPlOne , Pictures picChkPlTwo]

-- Обработчик событий
handleGameEvent :: Event -> AppDataState -> AppDataState
handleGameEvent (EventKey (MouseButton LeftButton) Down _ (xMouse, yMouse)) (AppDataState drawScale pOne pTwo (GameBoard (xO, yO, xT, yT) hGERedPointsBoard hGEWhitePointsBoard hGEPointsLen)) = state -- Ничего не делаем
where
    

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