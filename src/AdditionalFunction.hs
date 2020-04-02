module AdditionalFunction where
-- ******************************************************************
-- Подключаемые модули
import System.IO
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment
import Data.Maybe

import AbstractData
import Constants

-- ******************************************************************
-- Описание вспомогательных функций программы

-- Нахождение значения True в Boolean списке
getTrueItrBooleanList :: [Bool] -> Int -> Int
getTrueItrBooleanList [] x = x                      -- True не найден
getTrueItrBooleanList (x:xs) itr
    | x = itr                                           -- Мы нашли True
    | otherwise = getTrueItrBooleanList xs (itr + 1)    -- Переходим к следующему элементу

-- Удалить элемент из списка по индексу
deleteListElement :: Bool -> [a] -> Int -> [a]
deleteListElement False x _ = x
deleteListElement True dleList dleItr = dleNewList where
    dleLeftPart = fst(splitAt dleItr dleList)
    dleRightPart = tail (snd(splitAt dleItr dleList))
    dleNewList = dleLeftPart ++ dleRightPart

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

-- Создание координат окружности
createCircle :: Integer -> Float -> [(Float, Float)]
createCircle ccN ccRadius = result where
    ccDeltaQuarter = fromInteger(ccN)/4
    ccDeltaQuarter' = ceiling(ccDeltaQuarter)
    ccItr = [0 .. ccDeltaQuarter']
    ccFirstQuarter = map (createFirstQuarter (pi/(2 * ccDeltaQuarter))) ccItr where
        createFirstQuarter :: Float -> Integer -> (Float, Float)
        createFirstQuarter cfqDeltaAlpha cfqItrAlpha = (cfqResultX, cfqResultY) where
            cfqResultX = (-cos (cfqDeltaAlpha * fromIntegral(cfqItrAlpha))) * ccRadius
            cfqResultY = (sin (cfqDeltaAlpha * fromIntegral(cfqItrAlpha))) * ccRadius
    ccSecondQuarter = map (createSecondQuarter (pi/(2 * ccDeltaQuarter))) ccItr where
        createSecondQuarter :: Float -> Integer -> (Float, Float)
        createSecondQuarter csqDeltaAlpha csqItrAlpha = (csqResultX, csqResultY) where
            csqResultX = (cos (csqDeltaAlpha * fromIntegral(csqItrAlpha))) * ccRadius
            csqResultY = (sin (csqDeltaAlpha * fromIntegral(csqItrAlpha))) * ccRadius
    ccThirdQuarter = map (createThirdQuarter (pi/(2 * ccDeltaQuarter))) ccItr where
        createThirdQuarter :: Float -> Integer -> (Float, Float)
        createThirdQuarter ctqDeltaAlpha ctqItrAlpha = (ctqResultX, ctqResultY) where
            ctqResultX = (cos (ctqDeltaAlpha * fromIntegral(ctqItrAlpha))) * ccRadius
            ctqResultY = (-sin (ctqDeltaAlpha * fromIntegral(ctqItrAlpha))) * ccRadius
    ccFourthQuarter = map (createFourthQuarter (pi/(2 * ccDeltaQuarter))) ccItr where
        createFourthQuarter :: Float -> Integer -> (Float, Float)
        createFourthQuarter cfqDeltaAlpha cfqItrAlpha = (cfqResultX, cfqResultY) where
            cfqResultX = (-cos (cfqDeltaAlpha * fromIntegral(cfqItrAlpha))) * ccRadius
            cfqResultY = (-sin (cfqDeltaAlpha * fromIntegral(cfqItrAlpha))) * ccRadius
    result = ccFirstQuarter ++ ccSecondQuarter ++ ccThirdQuarter ++ ccFourthQuarter

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
    ggpMoveChecker      = gbChecker ggpBoard
    ggpMoveCheckPicture = if isNothing (fst(ggpMoveChecker)) then
                            Blank
                        else
                            getPolygonPicture (chPolygon (fromJust (fst(ggpMoveChecker))))
    result   =   Pictures [ggpBoardPicture, ggpBarPicture, ggpPointsPicture, ggpMoveCheckPicture]

-- При нахождении вытащить шашку из списка
data OutPutCheckers = OutPutCheckers {opcList :: [Checker], opcElement :: Maybe Checker}
swapChecker :: (Float, Float) -> [Checker] -> OutPutCheckers
swapChecker scMouseCoord scList = scOutput where
    scFlagsList = map (isInPolygon scMouseCoord) (map chPolygon scList)
    scItr = getTrueItrBooleanList scFlagsList 0
    scNewChecker = if (scItr == length scFlagsList) then
        Nothing
    else
        Just (scList !! scItr)
    scNewList = deleteListElement (scItr < length scFlagsList) scList scItr
    scOutput = OutPutCheckers scNewList scNewChecker

-- Замена списка шашек в пункте
replaceCheckersList :: TablesPoint -> OutPutCheckers -> TablesPoint
replaceCheckersList rclPoint rclCheckersList = rclNewPoint where
    rclNewList = if (isNothing(opcElement rclCheckersList)) then
        (tpCheckers rclPoint)
    else
        (opcList rclCheckersList)
    rclNewPoint = TablesPoint (tpNumber rclPoint) rclNewList (tpPolygon rclPoint)

-- Нахождение значения Just в списке
getNewCheckerItr :: [OutPutCheckers] -> Int -> Int
getNewCheckerItr [] gniItr = gniItr     -- Не найдено
getNewCheckerItr (x:xs) gniItr
    | isJust(opcElement x) = gniItr     -- Найдено
    | otherwise = getNewCheckerItr xs (gniItr + 1)  -- Идем дальше

-- Заменить пункт в списке пунктов
changePointInList :: [TablesPoint] -> Int -> Checker -> [TablesPoint]
changePointInList cplList cplItr cplChecker = cplNewList where
    cplPoint = cplList !! cplItr
    cplNewPoint = TablesPoint (tpNumber cplPoint) ((tpCheckers cplPoint) ++ [cplChecker]) (tpPolygon cplPoint)
    cplSplitList = splitAt cplItr cplList
    cplNewList = (fst cplSplitList) ++ [cplNewPoint] ++ (tail (snd cplSplitList))

-- Примагнитить шашку к движению мышки
attachMovingChecker :: (Float, Float) -> GameBoard -> GameBoard
attachMovingChecker amcMouseCoord amcOldBoard = amcNewBoard where
    amcOldPoints = gbPoints amcOldBoard
    amcOldBar = gbBar amcOldBoard
    amcBarCheckers = tbCheckers amcOldBar
    amcInputBar = swapChecker amcMouseCoord amcBarCheckers
    amcNewBarCheckers = opcList amcInputBar
    amcInputPoints = map (swapChecker amcMouseCoord) (map tpCheckers amcOldPoints)
    amcNewPoints = zipWith replaceCheckersList amcOldPoints amcInputPoints
    amcItr = getNewCheckerItr (amcInputPoints ++ [amcInputBar]) 0
    amcNewChecker = if (amcItr < length (amcInputPoints ++ [amcInputBar])) then
        opcElement ((amcInputPoints ++ [amcInputBar]) !! amcItr)
    else
        Nothing
    amcSource = if (amcItr < length amcInputPoints) then
        Just (CheckerSource PointOnBoard (return (amcItr)))
    else
        if (amcItr == length amcInputPoints) then
            Just (CheckerSource Bar Nothing)
        else
            Nothing
    amcNewBar = TablesBar (opcList amcInputBar) (tbPolygon amcOldBar)
    amcNewBoard = GameBoard (gbBoardPolygon amcOldBoard) amcNewPoints amcNewBar (amcNewChecker, amcSource)

-- Переместить шашку
translateMovingChecker :: (Float, Float) -> GameBoard -> GameBoard
translateMovingChecker tmcCoord tmcBoard = tmcNewBoard where
    tmcMoveChecker = gbChecker tmcBoard
    tmcChecker = fst(tmcMoveChecker)
    tmcSource = snd(tmcMoveChecker)
    tmcPlayer = if (isJust(tmcChecker)) then
        chPlayer (fromJust(tmcChecker))
    else
        PlayerOne
    tmcNewChecker = if (isJust(tmcChecker)) then
        Just (Checker tmcPlayer (translatePolygon (chPolygon(fromJust(tmcChecker))) tmcCoord))
    else
        Nothing
    tmcNewBoard = GameBoard (gbBoardPolygon tmcBoard) (gbPoints tmcBoard) (gbBar tmcBoard) (tmcNewChecker, tmcSource)

-- Открепить шашку
detachMovingChecker :: (Float, Float) -> GameBoard -> GameBoard
detachMovingChecker dmcCoord dmcBoard = dmcNewBoard where
    dmcOldBar = gbBar dmcBoard
    dmcOldPoints = gbPoints dmcBoard
    dmcMoveChecker = gbChecker dmcBoard
    dmcChecker = fst(dmcMoveChecker)
    dmcSource = snd(dmcMoveChecker)
    
    dmcNewBar = if (dmcSource == Just(CheckerSource Bar Nothing)) then
        TablesBar ((tbCheckers dmcOldBar) ++ [fromJust(dmcChecker)]) (tbPolygon dmcOldBar)
    else
        dmcOldBar
    dmcNewPoints = if (isJust(dmcSource)) then
        if ((csSource (fromJust(dmcSource))) == PointOnBoard) then
            changePointInList dmcOldPoints (fromJust(csNumber (fromJust(dmcSource)))) (fromJust(dmcChecker))
        else
            dmcOldPoints
    else
        dmcOldPoints
    dmcNewBoard = GameBoard (gbBoardPolygon dmcBoard) dmcNewPoints dmcNewBar (Nothing, Nothing)

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

-- Базовый полигон доски
baseBoardPolygon :: AuxPolygon
baseBoardPolygon = ([(-900, 500), (900, 500),
                        (900, -500), (-900, -500)], makeColorI 91 58 41 255)

-- Пробная шашка
testChecker :: AuxPolygon
testChecker = ((createCircle 400 40.0), makeColorI 255 255 255 255)

-- Базовый список пунктов
baseTablesPoints :: [TablesPoint]
baseTablesPoints = [(TablesPoint 1 [(Checker PlayerOne (translatePolygon testChecker (100, 100)))] ([(800, 500), (880, 500), (840,100)], makeColorI 135 67 8 255)),
                    (TablesPoint 2 [] ([(680, 500), (760, 500), (720,100)], makeColorI 249 214 184 255))]

-- Базовый игровой бар
baseBar :: TablesBar
baseBar = TablesBar [(Checker PlayerOne testChecker)] ([(-80,500), (80, 500), (80, -500), (-80, -500)], makeColorI 118 60 40 255)

-- Игровая доска
baseGameBoard :: GameBoard
baseGameBoard = GameBoard baseBoardPolygon baseTablesPoints baseBar (Nothing, Nothing)

-- Базовое игровое состояние
baseGameState :: ApplicationState
baseGameState = ApplicationState PlayerOneRoll False