module Constants where

-- ******************************************************************
-- Подключаемые модули
import System.IO
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment

import AbstractData

-- ******************************************************************
-- Объявление констант

-- Базовая ширина экрана
baseScreenWidth :: Float
baseScreenWidth = 1920.0

-- Базовая высота экрана
baseScreenHeight :: Float
baseScreenHeight = 1080.0

-- Режим экрана
dispMode :: Display
dispMode = FullScreen

-- Цвет заднего фона
bgColor :: Color
bgColor = makeColorI 118 60 40 255

-- Частота вызова функции updateGameApp
fps :: Int
fps = 75

-- Цвет первого игрока
pOneColor :: Color
pOneColor = makeColorI 210 177 152 255

-- Цвет второго игрока
pTwoColor :: Color
pTwoColor = makeColorI 22 53 55 255

-- Список шашек первого игрока
pOneChcr :: [DChcr]
pOneChcr = [DChcr True 1  1 50 0 False,
            DChcr True 1  2 100 0 False,
            DChcr True 12 1 0 0 False,
            DChcr True 12 2 0 0 False,
            DChcr True 12 3 0 0 False,
            DChcr True 12 4 0 0 False,
            DChcr True 12 5 0 0 False,
            DChcr True 17 1 0 0 False,
            DChcr True 17 2 0 0 False,
            DChcr True 17 3 0 0 False,
            DChcr True 19 1 0 0 False,
            DChcr True 19 2 0 0 False,
            DChcr True 19 3 0 0 False,
            DChcr True 19 4 0 0 False,
            DChcr True 19 5 0 0 False]

-- Список шашек второго игрока
pTwoChcr :: [DChcr]
pTwoChcr = [DChcr True 6  1 0 0 False,
            DChcr True 6  2 0 0 False,
            DChcr True 6  3 0 0 False,
            DChcr True 6  4 0 0 False,
            DChcr True 6  5 0 0 False,
            DChcr True 8  1 0 0 False,
            DChcr True 8  2 0 0 False,
            DChcr True 8  3 0 0 False,
            DChcr True 13 1 0 0 False,
            DChcr True 13 2 0 0 False,
            DChcr True 13 3 0 0 False,
            DChcr True 13 4 0 0 False,
            DChcr True 13 5 0 0 False,
            DChcr True 24 1 0 0 False,
            DChcr True 24 2 0 0 False]

-- Информация об игроке 1
playerOne :: DPInf
playerOne = DPInf pOneColor pOneChcr

-- Информация об игроке 2
playerTwo :: DPInf
playerTwo = DPInf pTwoColor pTwoChcr

-- Прямоугольник игровой доски
deckRect :: AuxRect
deckRect = AuxRect (-900) 500 900 (-500)

-- Цвет красных пунктов
redColorDeckPoint :: Color
redColorDeckPoint = makeColorI 135 67 8 255

-- Цвет белых пунктов
whiteColorDeckPoint :: Color
whiteColorDeckPoint = makeColorI 249 214 184 255

-- Цвет прямоугольника игровой доски
deckColor :: Color
deckColor = makeColorI 91 58 41 255

-- Список пунктов (красные)
listRedPoints :: [DChPoint]
listRedPoints = [DChPoint   (AuxCoord 840   500)        True  2 1,
                DChPoint    (AuxCoord 600   500)        True  0 3,
                DChPoint    (AuxCoord 360   500)        True  0 5,
                DChPoint    (AuxCoord (-240)  500)      True  0 7,
                DChPoint    (AuxCoord (-480)  500)      True  0 9,
                DChPoint    (AuxCoord (-720)  500)      True  0 11,
                DChPoint    (AuxCoord (-840)  (-500))   False 5 13,
                DChPoint    (AuxCoord (-600)  (-500))   False 0 15,
                DChPoint    (AuxCoord (-360) (-500))    False 3 17,
                DChPoint    (AuxCoord 240  (-500))      False 5 19,
                DChPoint    (AuxCoord 480  (-500))      False 5 21,
                DChPoint    (AuxCoord 720  (-500))      False 0 23]
                
-- Список пунктов (белые)
listWhitePoints :: [DChPoint]
listWhitePoints = [DChPoint (AuxCoord 720   500)      True  0 2,
                DChPoint    (AuxCoord 480   500)      True  0 4,
                DChPoint    (AuxCoord 240   500)      True  5 6,
                DChPoint    (AuxCoord (-360)  500)    True  3 8,
                DChPoint    (AuxCoord (-600)  500)    True  0 10,
                DChPoint    (AuxCoord (-840)  500)    True  5 12,
                DChPoint    (AuxCoord (-720)  (-500)) False 0 14,
                DChPoint    (AuxCoord (-480)  (-500)) False 0 16,
                DChPoint    (AuxCoord (-240) (-500))  False 0 18,
                DChPoint    (AuxCoord 360  (-500))    False 0 20,
                DChPoint    (AuxCoord 600  (-500))    False 0 22,
                DChPoint    (AuxCoord 840  (-500))    False 2 24]

-- Красные пункты на игровой доске
redDeckPoints :: DChPntList
redDeckPoints = DChPntList listRedPoints redColorDeckPoint

-- Белые пункты на игровой доске
whiteDeckPoints :: DChPntList
whiteDeckPoints = DChPntList listWhitePoints whiteColorDeckPoint

-- Длина пункта
pointLen :: Float
pointLen = 400

-- Ширина пункта
pointWidth :: Float
pointWidth = 50

-- Радиус шашки
radiusChecker :: Float
radiusChecker = 40

-- Игровая доска
gameBoard :: DGBoard
gameBoard = DGBoard deckRect deckColor redDeckPoints whiteDeckPoints pointLen