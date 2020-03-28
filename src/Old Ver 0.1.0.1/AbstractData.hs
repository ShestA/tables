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

-- Координата
data AuxCoord = AuxCoord
    {
        acX :: Float,
        acY :: Float
    }

-- Прямоугольник
data AuxRect = AuxRect
    {
        arLeft      :: Float,
        arTop       :: Float,
        arRight     :: Float,
        arBottom    :: Float
    }
-- ******************************************************************
-- Данные абстракции приложения

-- Сотстояния приложения
data AppStates = 
                POneMove                    -- Ход игрока 1
                | PTwoMove                  -- Ход игрока 2
                | POneRoll                  -- Бросок костей игрока 1
                | PTwoRoll  deriving (Enum) -- Бросок костей игрока 2
                
-- Пункт (Data Checker-Point)
data DChPoint = DChPoint
    {
        cpCoord :: AuxCoord,    -- Координата начала пункта в окне
        cpDirec :: Bool,        -- Направление пункта - вверх (True) или вниз (False)
        cpNEl   :: Int,         -- Текущее количество шашек в пункте
        cpN     :: Int          -- Номер пункта
    }

-- Список пунктов (Data Checker-Point List)
data DChPntList = DChPntList
    {
        cplPnts     :: [DChPoint],  -- Список пунктов
        cplColor    :: Color        -- Цвет пунктов
    }

-- Игровая доска (Data Game Board)
data DGBoard = DGBoard
    {
        gbBrdRect       :: AuxRect,     -- Прямоугольник доски
        gbBrdClr        :: Color,       -- Цвет прямоугольника
        gbRdPntsBrd     :: DChPntList,  -- Красные пункты на доске
        gbWhtPntsBrd    :: DChPntList,  -- Белые пункты на доске
        gbPtsLen        :: Float        -- Длина пунктов на доске (Не используется)
    }

-- Шашка (Data Checker)
data DChcr = DChcr
    {
        chInGame    :: Bool,    -- Шашка на доске (True) или на баре (False)?
        chPnt       :: Int,     -- Текущий номер пункта (Нумерация внутренняя, не игровая)
        chPos       :: Int,     -- Позиция в пункте
        chX         :: Float,   -- Координта Х в СК окна
        chY         :: Float,   -- Координата У в СК окна
        chOnMv      :: Bool     -- Шашку двигает игрок?
    }

-- Игрок (Data Player Info)
data DPInf = DPInf
    {
        piColor :: Color,   -- Цвет шашек
        piChcrs :: [DChcr]  -- Шашки игрока
    }
    
-- Полное состояние приложения (Data Application Data State)
data DAppDtState = DAppDtState
    {
        adsXScale   :: Float,       -- Масштаб отрисовки изображения по X
        adsYScale   :: Float,       -- Масштаб отрисовки изображения по Y
        adsPOne     :: DPInf,       -- Данные первого игрока
        adsPTwo     :: DPInf,       -- Данные второго игрока
        adsBoard    :: DGBoard,     -- Данные игровой доски
        adsState    :: AppStates,   -- Состояние приложения
        adsLastUpd  :: Float,       -- Время с последнего обновления
        adsDiceOne  :: Int,         -- Первая кость
        adsDiceTwo  :: Int          -- Вторая кость
    }
