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
whiteChecker :: AuxPolygon
whiteChecker = ((createCircle 10 40.0), makeColorI 255 255 255 255)

blackChecker :: AuxPolygon
blackChecker = ((createCircle 10 40.0), makeColorI 0 0 0 255)

-- Базовый список пунктов
baseTablesPoints :: [TablesPoint]
baseTablesPoints = [(TablesPoint 0 [(Checker PlayerOne whiteChecker),(Checker PlayerOne whiteChecker)]  ([(760, 500), (840, 500), (800,100)], makeColorI 135 67 8 255)),
                    (TablesPoint 1 []([(640, 500), (720, 500), (680,100)], makeColorI 249 214 184 255)),
                    (TablesPoint 2 []([(520, 500), (600, 500), (560,100)], makeColorI 135 67 8 255)),
                    (TablesPoint 3 []([(400, 500), (480, 500), (440,100)], makeColorI 249 214 184 255)),
                    (TablesPoint 4 []([(280, 500), (360, 500), (320,100)], makeColorI 135 67 8 255)),
                    (TablesPoint 5 [(Checker PlayerTwo blackChecker),(Checker PlayerTwo blackChecker),(Checker PlayerTwo blackChecker),(Checker PlayerTwo blackChecker),(Checker PlayerTwo blackChecker)] ([(160, 500), (240, 500), (200,100)], makeColorI 249 214 184 255)),
                    (TablesPoint 6 []([(-160, 500), (-240, 500), (-200,100)], makeColorI 135 67 8 255)),
                    (TablesPoint 7 [(Checker PlayerTwo blackChecker),(Checker PlayerTwo blackChecker),(Checker PlayerTwo blackChecker)]  ([(-280, 500), (-360, 500), (-320,100)], makeColorI 249 214 184 255)),
                    (TablesPoint 8 []([(-400, 500), (-480, 500), (-440,100)], makeColorI 135 67 8 255)),
                    (TablesPoint 9 []([(-520, 500), (-600, 500), (-560,100)], makeColorI 249 214 184 255)),
                    (TablesPoint 10 [] ([(-640, 500), (-720, 500), (-680,100)], makeColorI 135 67 8 255)),
                    (TablesPoint 11 [(Checker PlayerOne whiteChecker),(Checker PlayerOne whiteChecker),(Checker PlayerOne whiteChecker),(Checker PlayerOne whiteChecker),(Checker PlayerOne whiteChecker)] ([(-760, 500), (-840, 500), (-800,100)], makeColorI 249 214 184 255)),
                    (TablesPoint 12 [(Checker PlayerTwo blackChecker),(Checker PlayerTwo blackChecker),(Checker PlayerTwo blackChecker),(Checker PlayerTwo blackChecker),(Checker PlayerTwo blackChecker)] ([(-760, -500), (-840, -500), (-800,-100)], makeColorI 135 67 8 255)),
                    (TablesPoint 13 []([(-640, -500), (-720, -500), (-680,-100)], makeColorI 249 214 184 255)),
                    (TablesPoint 14 []  ([(-520, -500), (-600, -500), (-560,-100)], makeColorI 135 67 8 255)),
                    (TablesPoint 15 []([(-400, -500), (-480, -500), (-440,-100)], makeColorI 249 214 184 255)),
                    (TablesPoint 16 [(Checker PlayerOne whiteChecker),(Checker PlayerOne whiteChecker),(Checker PlayerOne whiteChecker)]  ([(-280, -500), (-360, -500), (-320,-100)], makeColorI 135 67 8 255)),
                    (TablesPoint 17 []([(-160, -500), (-240, -500), (-200,-100)], makeColorI 249 214 184 255)),
                    (TablesPoint 18 [(Checker PlayerOne whiteChecker),(Checker PlayerOne whiteChecker),(Checker PlayerOne whiteChecker),(Checker PlayerOne whiteChecker),(Checker PlayerOne whiteChecker)] ([(160, -500), (240, -500), (200,-100)], makeColorI 135 67 8 255)),
                    (TablesPoint 19 []([(280, -500), (360, -500), (320,-100)], makeColorI 249 214 184 255)),
                    (TablesPoint 20 []([(400, -500), (480, -500), (440,-100)], makeColorI 135 67 8 255)),
                    (TablesPoint 21 []([(520, -500), (600, -500), (560,-100)], makeColorI 249 214 184 255)),
                    (TablesPoint 22 []([(640, -500), (720, -500), (680,-100)], makeColorI 135 67 8 255)),
                    (TablesPoint 23 [(Checker PlayerTwo blackChecker),(Checker PlayerTwo blackChecker)]([(760, -500), (840, -500), (800,-100)], makeColorI 249 214 184 255))]

-- Базовый игровой бар
baseBar :: TablesBar
baseBar = TablesBar [] ([(-80,500), (80, 500), (80, -500), (-80, -500)], makeColorI 118 60 40 255)

-- Базовое игровое состояние
baseGameState :: ApplicationState
baseGameState = ApplicationState PlayerOne Roll False