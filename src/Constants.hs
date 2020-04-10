module Constants where

-- ******************************************************************
-- Подключаемые модули
import System.IO
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment

import AbstractData
import AdditionalFunction

-- ******************************************************************
-- Объявление стартовых значений

-- Режим экрана
dispMode :: Display
dispMode = FullScreen

-- Цвет заднего фона
bgColor :: Color
bgColor = makeColorI 118 60 40 255

-- Частота вызова функции updateGameApp
fps :: Int
fps = 75

-- Базовый полигон доски
baseBoardPolygon :: AuxPolygon
baseBoardPolygon = ([(-900, 500), (900, 500),
                        (900, -500), (-900, -500)], makeColorI 91 58 41 255)

-- Пробная шашки
testChecker :: AuxPolygon
testChecker = ((createCircle 10 40.0), makeColorI 255 255 255 255)

testChecker1 :: AuxPolygon
testChecker1 = ((createCircle 10 40.0), makeColorI 0 0 0 255)

-- Базовый список пунктов
baseTablesPoints :: [TablesPoint]
baseTablesPoints = [(TablesPoint 1 [(Checker PlayerTwo (translatePolygon testChecker1 (100, 100)))] ([(800, 500), (880, 500), (840,100)], makeColorI 135 67 8 255)),
                    (TablesPoint 2 [] ([(680, 500), (760, 500), (720,100)], makeColorI 249 214 184 255))]

-- Базовый игровой бар
baseBar :: TablesBar
baseBar = TablesBar [(Checker PlayerOne testChecker)] ([(-80,500), (80, 500), (80, -500), (-80, -500)], makeColorI 118 60 40 255)

-- Базовое игровое состояние
baseGameState :: ApplicationState
baseGameState = ApplicationState PlayerOne Move False

-- Игровая доска
baseGameBoard :: GameBoard
baseGameBoard = GameBoard baseBoardPolygon baseTablesPoints baseBar (Nothing, Nothing) baseGameState (0, 0)