module AbstractData where

-- ******************************************************************
-- Подключаемые модули
import System.IO
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment

-- ******************************************************************
-- Объявление данных
-- ******************************************************************
-- Вспомогательные данные

-- Полигон
type AuxPolygon = ([(Float, Float)], Color)
-- ******************************************************************
-- Данные абстракции приложения

-- Сотстояния приложения
data StepState = 
                PlayerOneMove                    -- Ход игрока 1
                | PlayerTwoMove                  -- Ход игрока 2
                | PlayerOneRoll                  -- Бросок костей игрока 1
                | PlayerTwoRoll  deriving (Eq)   -- Бросок костей игрока 2
data ApplicationState = ApplicationState StepState Bool deriving (Eq)
-- Игроки
data PlayerName =
                PlayerOne
                | PlayerTwo deriving (Eq)
data Source     = 
                Bar
                | PointOnBoard deriving (Eq)
data CheckerSource = CheckerSource Source (Maybe Int) deriving (Eq)
-- Шашка
data Checker = Checker
    {
        chPlayer        :: PlayerName,      -- Игрок, которому принадлежит шашка
        chPolygon       :: AuxPolygon       -- Полигон шашки
    }deriving (Eq)
-- Пункт
data TablesPoint = TablesPoint
    {
        tpNumber            :: Int,         -- Номер пункта
        tpCheckers          :: [Checker],   -- Шашки находящиеся в пункте
        tpPolygon           :: AuxPolygon   -- Полигон пункта
    }
-- Бар
data TablesBar = TablesBar
    {
        tbCheckers  :: [Checker],   -- Шашки находящиеся на игровом баре
        tbPolygon   :: AuxPolygon   -- Полигон бара
    }
-- Игровая доска
data GameBoard = GameBoard
    {
        gbBoardPolygon  :: AuxPolygon,                      -- Полигон игровой доски
        gbPoints        :: [TablesPoint],                   -- Пункты на доске
        gbBar           :: TablesBar,                       -- Игровой бар
        gbChecker       :: Maybe (Checker, CheckerSource)   -- Перемещаемая шашка и ее источник
    }
-- Игровое положение
data ApplicationData = ApplicationData
    {
        adScale :: (Float, Float),      -- Масштаб отрисовки
        adState :: ApplicationState,    -- Состояние приложения
        adBoard :: GameBoard,           -- Игровая доска
        adDices :: (Int, Int)           -- Игровые кости
    }