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

-- Игроки
data PlayerName =
                PlayerOne
                | PlayerTwo deriving (Eq)
                
-- Сотстояния приложения
data StepState = 
                Move                    -- Ход игрока
                | Roll  deriving (Eq)   -- Бросок костей игрока 
data ApplicationState = ApplicationState
    {
        currentPlayer   :: PlayerName,
        currentAction   :: StepState,
        isFinish        :: Bool 
    }deriving (Eq)
-- Источник движущейся шашки
data Source     = 
                Bar
                | PointOnBoard deriving (Eq)
data CheckerSource = CheckerSource 
    {
        csSource :: Source, 
        csNumber :: (Maybe Int) 
    }deriving (Eq)
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
        gbChecker       :: (Maybe Checker, Maybe CheckerSource),   -- Перемещаемая шашка и ее источник
        gbState         :: ApplicationState,                -- Игровое состояние
        gbDices         :: (Int, Int)                       -- Игровые кости
    }
-- Игровое положение
data ApplicationData = ApplicationData
    {
        adScale :: (Float, Float),      -- Масштаб отрисовки
        adBoard :: GameBoard            -- Игровая доска
    }