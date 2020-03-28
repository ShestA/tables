module AdditionalFunction where
-- ******************************************************************
-- Подключаемые модули
import System.IO
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment

import AbstractData
import Constants

-- ******************************************************************
-- Описание вспомогательных функций программы

-- Получение масштабного коэффициента
getScreenScale :: (Float, Float) -> (Float, Float)
getScreenScale (gsScrWidth, gsScrHeight) = (gsScaleX, gsScaleY) where
    gsScaleX =  gsScrWidth / baseScreenWidth
    gsScaleY =  gsScrHeight / baseScreenHeight
    
-- Отрисовка полигона
getPolygonPicture :: AuxPolygon -> Picture
getPolygonPicture (gppCoordinates, gppColor) = result where
    result = Color gppColor (Polygon gppCoordinates)

-- Получить список X координат
getCoordinatesX :: [(Float, Float)] -> [Float]
getCoordinatesX list = [fst x | x <- list]

-- Получить список Y координат
getCoordinatesY :: [(Float, Float)] -> [Float]
getCoordinatesY list = [snd x | x <- list]

-- Минимальный X полигона
minX :: [(Float, Float)] -> Float
minX mxCoords = result where
    result = minimum $ getCoordinatesX mxCoords

-- Максимальный X полигона
maxX :: [(Float, Float)] -> Float
maxX mxCoords = result where
    result = maximum $ getCoordinatesX mxCoords
    
-- Минимальный Y полигона
minY :: [(Float, Float)] -> Float
minY myCoords = result where
    result = minimum $ getCoordinatesY myCoords

-- Максимальный Y полигона
maxY :: [(Float, Float)] -> Float
maxY myCoords = result where
    result = maximum $ getCoordinatesY myCoords

-- Геометрический центр полигона
centerOfPolygon :: [(Float, Float)] -> (Float, Float)
centerOfPolygon copCoords = (resultX, resultY) where
    resultX = ((maxX copCoords) + (minX copCoords)) / 2
    resultY = ((maxY copCoords) + (minY copCoords)) / 2

-- Масимальная длина полигона по X
maxLengthX :: [(Float, Float)] -> Float
maxLengthX mlxCoord = (maxX mlxCoord) - (minX mlxCoord)

-- Масимальная длина полигона по Y
maxLengthY :: [(Float, Float)] -> Float
maxLengthY mlyCoord = (maxY mlyCoord) - (minY mlyCoord)

-- Координаты внутри полигона?
isInPolygon :: (Float, Float) -> AuxPolygon -> Bool
isInPolygon (iipX, iipY) (iipPolygon, _) = result where
    result =    iipX >= (minX iipPolygon) &&
                iipX <= (maxX iipPolygon) &&
                iipY >= (minY iipPolygon) &&
                iipY <= (maxY iipPolygon)

-- Перемещение координат
translateCoordinate :: (Float, Float) -> (Float, Float) -> (Float, Float)
translateCoordinate (tcDX, tcDY) (tcX, tcY) = (tcResultX, tcResultY) where
    tcResultX = tcX + tcDX
    tcResultY = tcY + tcDY

-- Перемещение полигона
translatePolygon :: AuxPolygon -> (Float, Float) -> AuxPolygon
translatePolygon (tpCoords, tpColor) (tpX, tpY) = (tpNewCoords, tpColor) where
    (tpCenterX, tpCenterY) = centerOfPolygon tpCoords
    tpDX = tpX - tpCenterX
    tpDY = tpY - tpCenterY
    tpNewCoords = map (translateCoordinate (tpDX, tpDY)) (tpCoords)

-- Отрисовка пункта с шашками
pointAndCheckersPicture :: TablesPoint -> Picture
pointAndCheckersPicture pcpPoint = result where
    pcpPointPicture = getPolygonPicture (tpPolygon pcpPoint)
    pcpCheckersPicture = map getPolygonPicture (map chPolygon (tpCheckers pcpPoint))
    result = Pictures ([pcpPointPicture] ++ pcpCheckersPicture)

-- Отрисовка пунктов с шашками
getPointsPicture :: [TablesPoint] -> Picture
getPointsPicture gppPoints = result where
    result = Pictures (map pointAndCheckersPicture gppPoints) 

-- Отрисовка бара с шашками
getBarPicture :: TablesBar -> Picture
getBarPicture gbpBar = result where
    gbpBarPicture = getPolygonPicture (tbPolygon gbpBar)
    gbpCheckersPicture = map getPolygonPicture (map chPolygon (tbCheckers gbpBar))
    result = Pictures ([gbpBarPicture] ++ gbpCheckersPicture)

-- Отрисовка полного стола
getGamePicture :: GameBoard -> Picture
getGamePicture ggpBoard = result where
    ggpBoardPicture     = getPolygonPicture (gbBoardPolygon ggpBoard)
    ggpPointsPicture    = getPointsPicture (gbPoints ggpBoard)
    ggpBarPicture       = getBarPicture (gbBar ggpBoard)
    result   =   Pictures [ggpBoardPicture, ggpPointsPicture, ggpBarPicture]

-- Продолжение следует