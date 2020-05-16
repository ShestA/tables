module AdditionalFunction where
-- ******************************************************************
-- Подключаемые модули
import System.IO
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment
import Data.Maybe
import Data.List
import System.Random

import AbstractData

-- ******************************************************************
-- Описание вспомогательных констант
-- Базовая ширина экрана
baseScreenWidth :: Float
baseScreenWidth = 1920.0

-- Базовая высота экрана
baseScreenHeight :: Float
baseScreenHeight = 1080.0

-- ******************************************************************
-- Описание вспомогательных функций программы

-- Нахождение значения в списке по функции
getItrFuncList :: [a] -> (a -> Bool) -> Int -> Int
getItrFuncList [] _ x = x                           -- Значение не найдено
getItrFuncList (x:xs) func itr
    | func x = itr                                  -- Найдено
    | otherwise = getItrFuncList xs func (itr + 1)  -- Переходим к следующему

-- Удалить элемент из списка по индексу
deleteListElement :: Bool -> [a] -> Int -> [a]
deleteListElement False x _ = x
deleteListElement True dleList dleItr = dleNewList where
    dleLeftPart = fst(splitAt dleItr dleList)
    dleRightPart = tail (snd(splitAt dleItr dleList))
    dleNewList = dleLeftPart ++ dleRightPart

-- Изменить элемент списка по индексу
changeListElement :: Bool -> [a] -> a -> Int -> [a]
changeListElement False x _ _ = x
changeListElement True cleList cleEl cleItr = cleNewList where
    cleLeftPart = fst(splitAt cleItr cleList)
    cleRightPart = tail (snd(splitAt cleItr cleList))
    cleNewList = cleLeftPart ++ [cleEl] ++ cleRightPart
    
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
swapChecker :: (Checker -> Bool) -> [Checker] -> OutPutCheckers
swapChecker scTest scList = scOutput where
    scFlagsList = map scTest scList
    scItr = findIndex (\x -> x) scFlagsList
    scNewChecker = if (isNothing scItr) then
        Nothing
    else
        Just (scList !! (fromJust scItr))
    scNewList = deleteListElement (isJust scItr) scList (fromJust scItr)
    scOutput = OutPutCheckers scNewList scNewChecker

-- Замена списка шашек в пункте
replaceCheckersList :: TablesPoint -> OutPutCheckers -> TablesPoint
replaceCheckersList rclPoint rclCheckersList = rclNewPoint where
    rclNewList = if (isNothing(opcElement rclCheckersList)) then
        (tpCheckers rclPoint)
    else
        (opcList rclCheckersList)
    rclNewPoint = TablesPoint (tpNumber rclPoint) rclNewList (tpPolygon rclPoint)

-- Заменить пункт в списке пунктов, добавив в него шашку
changePointInList :: [TablesPoint] -> Int -> Checker -> [TablesPoint]
changePointInList cplList cplItr cplChecker = cplNewList where
    cplPoint = cplList !! cplItr
    cplNewPoint = TablesPoint (tpNumber cplPoint) ((tpCheckers cplPoint) ++ [cplChecker]) (tpPolygon cplPoint)
    cplSplitList = splitAt cplItr cplList
    cplNewList = (fst cplSplitList) ++ [cplNewPoint] ++ (tail (snd cplSplitList))

-- Поиск шашек игрока на баре
findDropedChecker :: PlayerName -> [Checker] -> Bool
findDropedChecker fdcPlayer fdcList = result where
    fdcItr = getItrFuncList fdcList func 0 where
                func :: Checker -> Bool
                func chk = (chPlayer chk == fdcPlayer)
    result = if (fdcItr == length fdcList) then True else False

-- Пересчет координат шашек пункта
recalcCheckerPoint :: TablesPoint -> TablesPoint
recalcCheckerPoint x = xnew where
    a = tpCheckers x
    b = tpPolygon x
    d = tpNumber x
    ml = maxLengthY (fst b)
    mdy = ml / fromIntegral (length a)
    (cX, _) = centerOfPolygon (fst b)
    fl = d < 12
    h = if fl then ((maxY (fst b)) - 40) else ((minY (fst b)) + 40)
    k = zip [0..] a
    newc = if fl then map (func True mdy cX h) k else map (func False mdy cX h) k where
        func :: Bool -> Float -> Float -> Float -> (Int, Checker) -> Checker
        func True dy dx sy (n, c) = cnew where
            cp = chPolygon c
            cpnew = translatePolygon cp (dx, sy - ((fromIntegral n) * dy))
            cnew = Checker (chPlayer c) cpnew
        func False dy dx sy (n, c) = cnew where
            cp = chPolygon c
            cpnew = translatePolygon cp (dx, sy + ((fromIntegral n) * dy))
            cnew = Checker (chPlayer c) cpnew
    xnew = TablesPoint d newc b

-- Пересчет координат шашек бара
recalcCheckerBar :: TablesBar -> TablesBar
recalcCheckerBar x = xnew where
    a = tbCheckers x
    b = tbPolygon x
    c = filter (\x -> chPlayer x == PlayerOne) a
    d = filter (\x -> chPlayer x == PlayerTwo) a
    cl = length c
    dl = length d
    e = maxLengthX (fst b)
    ey = maxLengthY (fst b)
    edyc = ey / (fromIntegral cl)
    edyd = ey / (fromIntegral dl)
    f = e / 4
    (g, h) = centerOfPolygon (fst b)
    i = maxY (fst b)
    j = minY (fst b)
    k = zip [0..] c
    l = zip [0..] d
    m = map func k where
        func :: (Int, Checker) -> Checker
        func (n, o) = p where
            r = chPolygon o
            s = translatePolygon r (g - f, i - ((fromIntegral n) * edyc))
            p = Checker PlayerOne s
    t = map func l where
        func :: (Int, Checker) -> Checker
        func (n, o) = p where
            r = chPolygon o
            s = translatePolygon r (g + f, j + ((fromIntegral n) * edyd))
            p = Checker PlayerTwo s
    u = m ++ t
    xnew = TablesBar u b

-- Примагнитить шашку к движению мышки
attachMovingChecker :: (Float, Float) -> GameBoard -> GameBoard
attachMovingChecker amcMouseCoord amcOldBoard = amcNewBoard where
    amcOldPoints = gbPoints amcOldBoard
    amcOldBar = gbBar amcOldBoard
    amcBarCheckers = tbCheckers amcOldBar
    amcState = gbState amcOldBoard
    amcOldDices = (gbDices amcOldBoard)
    amcOldBoardPolygon = (gbBoardPolygon amcOldBoard)
    amcBarIsEmpty = findDropedChecker (currentPlayer amcState) amcBarCheckers
    amcNewBoard = if amcBarIsEmpty then
        GameBoard amcOldBoardPolygon amcNewPoints amcOldBar (amcNewPointChecker, amcNewPointSource) amcState amcOldDices (gbDoubles amcOldBoard)
    else
        GameBoard amcOldBoardPolygon amcOldPoints amcNewBar (amcNewBarChecker, amcNewBarSource) amcState amcOldDices (gbDoubles amcOldBoard) where
            amcInputPoints = map (swapChecker (\x -> isInPolygon amcMouseCoord (chPolygon x))) (map tpCheckers amcOldPoints)
            amcItr = getItrFuncList (amcInputPoints) func 0 where
                func :: OutPutCheckers -> Bool
                func x = isJust(opcElement x)
            amcNewPointChecker = if (amcItr < length (amcInputPoints)) then
                let amcTmpChecker = opcElement (amcInputPoints !! amcItr) in
                    if (((currentAction amcState) == Move) && ((currentPlayer amcState) == chPlayer (fromJust (amcTmpChecker)))) then
                        amcTmpChecker
                    else
                        Nothing 
            else
                Nothing
            amcNewPointSource = if isJust(amcNewPointChecker) then
                    Just (CheckerSource PointOnBoard (return (tpNumber (amcOldPoints !! (amcItr)))))
                else
                    Nothing
            amcNewPoints = if isJust(amcNewPointChecker) then
                zipWith replaceCheckersList amcOldPoints amcInputPoints
            else
                amcOldPoints
            amcInputBar = swapChecker (\x -> isInPolygon amcMouseCoord (chPolygon x)) amcBarCheckers
            amcNewBarCheckers = opcList amcInputBar
            amcNewBarChecker = let amcTmpChecker = opcElement amcInputBar in
                if isJust amcTmpChecker then
                    if (((currentAction amcState) == Move) && ((currentPlayer amcState) == chPlayer (fromJust (amcTmpChecker)))) then
                        amcTmpChecker
                    else
                        Nothing
                else
                    Nothing
            amcNewBarSource = if isJust(amcNewBarChecker) then Just (CheckerSource Bar Nothing) else Nothing
            amcNewBar = if isJust(amcNewBarChecker) then
                TablesBar amcNewBarCheckers (tbPolygon amcOldBar)
            else
                amcOldBar

-- Переместить примагниченную шашку
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
    tmcOldBoardPolygon = (gbBoardPolygon tmcBoard)
    tmcOldDices = (gbDices tmcBoard)
    tmcState = gbState tmcBoard
    tmcNewBoard = GameBoard tmcOldBoardPolygon (gbPoints tmcBoard) (gbBar tmcBoard) (tmcNewChecker, tmcSource) tmcState tmcOldDices (gbDoubles tmcBoard)

-- Открепить шашку
detachMovingChecker :: (Float, Float) -> GameBoard -> GameBoard
detachMovingChecker dmcCoord dmcBoard = dmcNewBoard where
    dmcBoardPolygon = gbBoardPolygon dmcBoard
    dmcOldBar = gbBar dmcBoard
    dmcOldBarCheckers = tbCheckers dmcOldBar
    dmcOldBarPolygon = tbPolygon dmcOldBar
    dmcOldPoints = gbPoints dmcBoard
    dmcMoveChecker = gbChecker dmcBoard
    dmcChecker = fst(dmcMoveChecker)
    dmcSource = snd(dmcMoveChecker)
    dmcOldDices = gbDices dmcBoard
    dmcOldState = gbState dmcBoard
    dmcPlayer = currentPlayer dmcOldState
    dmcNewBoard = if (isJust dmcChecker) then
        GameBoard dmcBoardPolygon dmcNewPoints dmcNewBar (Nothing, Nothing) dmcNewState dmcNewDices newDoubles
    else
        dmcBoard where
            dmcOffsets = [fst dmcOldDices, snd dmcOldDices, fst dmcOldDices + snd dmcOldDices]
            dmcStart = if (csSource (fromJust(dmcSource)) == Bar) then 
                if (dmcPlayer == PlayerOne) then
                    -1
                else
                    24
            else
                fromJust (csNumber (fromJust(dmcSource)))
            dmcAllTargets' = if (dmcPlayer == PlayerOne) then 
                map (\x -> dmcStart + x) dmcOffsets
            else
                map (\x -> dmcStart - x) dmcOffsets
            dmcAllTargets = filter (\x -> (x < 24) && (x > -1)) dmcAllTargets'
            dmcFitTargets = func dmcPlayer dmcOldPoints dmcAllTargets where
                func :: PlayerName -> [TablesPoint] -> [Int] -> [Int]
                func name pts targets = result where
                    lst = map (\x -> pts !! x) targets
                    result = concat (map func1 lst) where
                        func1 :: TablesPoint -> [Int]
                        func1 pnt = itr where
                            ckrlst = tpCheckers pnt
                            enemylist = filter (\x -> (chPlayer x) /= name) ckrlst
                            itr = if (length enemylist > 1) then [] else [tpNumber pnt]
            dmcPlayerTarget = getItrFuncList dmcOldPoints func 0 where
                func :: TablesPoint -> Bool
                func pts = isInPolygon dmcCoord (tpPolygon pts)
            dmcSearchFlag = elem dmcPlayerTarget dmcFitTargets
            dmcNewBar'' = if dmcSearchFlag then
                dmcOldBar
            else
                if (csSource (fromJust(dmcSource)) == Bar) then
                    TablesBar (dmcOldBarCheckers ++ [fromJust(dmcChecker)]) dmcOldBarPolygon
                else
                    dmcOldBar
            dmcNewPoints'' = if dmcSearchFlag then
                changePointInList dmcOldPoints dmcPlayerTarget (fromJust(dmcChecker))
            else
                if (csSource (fromJust(dmcSource)) == PointOnBoard) then
                    changePointInList dmcOldPoints (fromJust(csNumber (fromJust(dmcSource)))) (fromJust(dmcChecker))
                else
                    dmcOldPoints
            dmcEnemyBeate = if dmcSearchFlag then
                Just $ swapChecker (\x -> chPlayer x /= dmcPlayer) (tpCheckers (dmcNewPoints'' !! dmcPlayerTarget))
            else
                Nothing
            dmcInitList = map (swapChecker (\x -> False)) (map tpCheckers dmcNewPoints'')
            dmcReadyList = changeListElement dmcSearchFlag dmcInitList (fromJust dmcEnemyBeate) dmcPlayerTarget
            dmcNewPoints' = if dmcSearchFlag then 
                if isJust (opcElement (fromJust dmcEnemyBeate)) then
                    zipWith replaceCheckersList dmcNewPoints'' dmcReadyList
                else
                    dmcNewPoints''
            else
                dmcNewPoints''
            dmcNewPoints = map recalcCheckerPoint dmcNewPoints'
            dmcNewBar' = if dmcSearchFlag then
                if isJust (opcElement (fromJust dmcEnemyBeate)) then
                    TablesBar (dmcOldBarCheckers ++ [fromJust (opcElement (fromJust dmcEnemyBeate))]) dmcOldBarPolygon
                else
                    dmcNewBar''
            else
                dmcNewBar''
            dmcNewBar = recalcCheckerBar dmcNewBar'
            dmcMoveInd = if dmcSearchFlag then
                findIndex (\x -> x == dmcPlayerTarget) dmcAllTargets'
            else
                Nothing
            dmcNewOffsets' = changeListElement (isJust dmcMoveInd) dmcOffsets 0 (fromJust dmcMoveInd)
            dmcNewOffsets = if (dmcNewOffsets' !! 2 == 0) then [0,0] else [dmcNewOffsets' !! 0, dmcNewOffsets' !! 1]
            fl1 = if (fst (gbDoubles dmcBoard)) && ((dmcNewOffsets !! 0) == 0) then
                True
            else
                False
            fl2 = if (snd (gbDoubles dmcBoard)) && ((dmcNewOffsets !! 1) == 0) then
                True
            else
                False
            newDoubles = if fl1 then
                if fl2 then
                    (False, False)
                else
                    (False, snd (gbDoubles dmcBoard))
            else
                if fl2 then
                    (fst (gbDoubles dmcBoard), False)
                else
                    (fst (gbDoubles dmcBoard), snd (gbDoubles dmcBoard))
            dmcNewDices = (if fl1 then dmcOffsets !! 0 else dmcNewOffsets !! 0, if fl2 then dmcOffsets !! 1 else dmcNewOffsets !! 1)
            --dmcNewState = if ((dmcNewDices == (0,0)) || ((length dmcFitTargets) == 0)) then 
            dmcNewState = if (dmcNewDices /= (0,0)) && canYouDo dmcBoard then
                dmcOldState
            else
                ApplicationState (currentPlayer dmcOldState) Move True
                --if dmcPlayer == PlayerOne then
                --    ApplicationState PlayerTwo Roll False
                --else
                --    ApplicationState PlayerOne Roll False
            --else
            --    dmcOldState

-- Бросаем кубики
rollDices :: ApplicationData -> ApplicationData
rollDices y = ynew where 
    x = adBoard y
    a = gbBoardPolygon x
    b = gbPoints x
    c = gbBar x
    d = gbChecker x
    e = gbState x
    f = currentPlayer e
    ff = currentAction e
    dd = gbDices x
    isRoll = ff == Roll
    ynew = if isRoll then
        ApplicationData (adScale y) (snd j) xnew
    else 
        y where
        g = ApplicationState f Move False
        h = adGen y
        i = randomR (1,6) h 
        j = randomR (1,6) (snd i)
        xnew = GameBoard a b c d g ((fst i), (fst j)) (if fst i == fst j then (True,True) else (False,False))

canDeleteChecker :: GameBoard -> Bool
canDeleteChecker board = result where
    tdcOldPoints = gbPoints board
    tdcOldBar = gbBar board
    tdcOldState = gbState board
    tdcPlayer = currentPlayer tdcOldState
    result' = theEnd tdcOldPoints tdcOldBar tdcPlayer where
        theEnd :: [TablesPoint] -> TablesBar -> PlayerName -> Bool
        theEnd tePoints teBar teName = teResult where
            teFilteredPoints = if teName == PlayerOne then
                filter (\x -> (tpNumber x) < 18) foo
            else
                filter (\x -> (tpNumber x) > 5) foo where
                    foo = filter (\x -> length (filter (\y -> (chPlayer y) == teName) (tpCheckers x)) > 0) tePoints
            teFilteredBar = filter (\x -> (chPlayer x) == teName) (tbCheckers teBar)
            teResult = ((length teFilteredPoints) == 0) && ((length teFilteredBar) == 0)
    resultl = if fst (gbDices board) == 0 then
        if (snd (gbDices board)) == 0 then
            False
        else
            length ys > 0
    else
        if (snd (gbDices board)) == 0 then
            length xs > 0
        else
            (length xs > 0) || (length ys > 0) where
                xsxxx = if tdcPlayer == PlayerOne then (24 - (fst (gbDices board))) else (fst (gbDices board)) - 1
                ysyyy = if tdcPlayer == PlayerOne then (24 - (snd (gbDices board))) else (snd (gbDices board)) - 1
                xsxx = tdcOldPoints !! xsxxx
                ysyy = tdcOldPoints !! ysyyy
                xsx = tpCheckers xsxx
                ysy = tpCheckers ysyy
                xs = filter (\x-> chPlayer x == tdcPlayer) xsx
                ys = filter (\x-> chPlayer x == tdcPlayer) ysy
    resultc = if isJust (snd (gbChecker board)) then
        if (csSource (fromJust (snd (gbChecker board)))) == PointOnBoard then
            if tdcPlayer == PlayerOne then
                (fromJust (csNumber (fromJust (snd (gbChecker board)))) == (24 - (fst (gbDices board)))) || (fromJust (csNumber (fromJust (snd (gbChecker board)))) == (24 - (snd (gbDices board))))
            else
                (fromJust (csNumber (fromJust (snd (gbChecker board)))) == ((fst (gbDices board)) - 1)) || (fromJust (csNumber (fromJust (snd (gbChecker board)))) == ((snd (gbDices board)) - 1))
        else
            False
    else
        False
    result = result' && (resultl || resultc)
    
    

tryDeleteChecker :: GameBoard -> GameBoard
tryDeleteChecker tdcBoard = tdcNewBoard where
    tdcBoardPolygon = gbBoardPolygon tdcBoard
    tdcOldBar = gbBar tdcBoard
    tdcOldBarCheckers = tbCheckers tdcOldBar
    tdcOldBarPolygon = tbPolygon tdcOldBar
    tdcOldPoints = gbPoints tdcBoard
    tdcMoveChecker = gbChecker tdcBoard
    tdcChecker = fst(tdcMoveChecker)
    tdcSource = snd(tdcMoveChecker)
    tdcOldDices = gbDices tdcBoard
    tdcOldState = gbState tdcBoard
    tdcPlayer = currentPlayer tdcOldState
    tdcEndingFlag = canDeleteChecker tdcBoard
    tdcNewBoard = if tdcEndingFlag then
        if (isJust tdcChecker) then 
            if (csSource (fromJust(tdcSource)) == PointOnBoard) then
                tdNewBoard'
            else 
                tdcBoard
        else
            tdcBoard
    else
       tdcBoard where
            -- Можно заканчивать игру...проверяем кубики
            tdcTargets = if tdcPlayer == PlayerOne then
                (24 - fst tdcOldDices, 24 - snd tdcOldDices)
            else
                (fst tdcOldDices - 1, snd tdcOldDices - 1)
            fl1 = (fst tdcTargets) == fromJust (csNumber (fromJust(tdcSource)))
            fl2 = (snd tdcTargets) == fromJust (csNumber (fromJust(tdcSource)))
            fl3 = fl1 || fl2
            -- Если условия соблюлись, двигающаяся шашка пропадёт
            -- Пересчитываем оставшиеся кубики и меняем состояние, если надо
            dfl1 = if (fst (gbDoubles tdcBoard)) && fl1 then
                True
            else
                False
            dfl2 = if (snd (gbDoubles tdcBoard)) && fl2 then
                True
            else
                False
            newDoubles = if dfl1 then
                if dfl2 then
                    (False, False)
                else
                    (False, snd (gbDoubles tdcBoard))
            else
                if dfl2 then
                    (fst (gbDoubles tdcBoard), False)
                else
                    (fst (gbDoubles tdcBoard), snd (gbDoubles tdcBoard))
            tdcNewDices = if fl3 then
                if fl1 then
                    if dfl1 then
                        (fst tdcOldDices, snd tdcOldDices)
                    else
                        (0, snd tdcOldDices)
                else
                    if dfl2 then
                        (fst tdcOldDices, snd tdcOldDices)
                    else
                        (fst tdcOldDices, 0)
            else
                tdcOldDices
            tdcNewMoveChecker = if fl3 then
                (Nothing, Nothing)
            else
                tdcMoveChecker
            tdcNewTargets = if tdcPlayer == PlayerOne then
                (24 - fst tdcNewDices, 24 - snd tdcNewDices)
            else
                (fst tdcNewDices - 1, snd tdcNewDices - 1)
            ffl1 = if (fst tdcNewDices > 0) then
                (length (filter (\x -> chPlayer x == tdcPlayer) (tpCheckers (tdcOldPoints !! (fst tdcNewTargets))))) == 0
            else
                True
            ffl2 = if (snd tdcNewDices > 0) then
                (length (filter (\x -> chPlayer x == tdcPlayer) (tpCheckers (tdcOldPoints !! (snd tdcNewTargets))))) == 0
            else
                True
            tdcNewState = if (tdcNewDices == (0,0)) || ((not (canYouDo tdcBoard)) && ((length (tpCheckers (tdcOldPoints !! (fst tdcTargets)))) == 0) && ((length (tpCheckers (tdcOldPoints !! (snd tdcTargets)))) == 0)) then
                if tdcPlayer == PlayerOne then
                    ApplicationState PlayerTwo Roll False
                else
                    ApplicationState PlayerOne Roll False
            else
                tdcOldState
            -- Пересоздаем доску
            tdNewBoard' = GameBoard tdcBoardPolygon tdcOldPoints tdcOldBar tdcNewMoveChecker tdcNewState tdcNewDices newDoubles
canYouDo :: GameBoard -> Bool
canYouDo board = result where
    state = gbState board
    dices = gbDices board
    bar = gbBar board
    points = gbPoints board
    player = currentPlayer state
    -- Определим номера пунктов принадлежащих игроку [Int]
    playerPoints = map tpNumber (filter (\x -> length (filter (\y -> (chPlayer y) == player) (tpCheckers x)) > 0) points)
    -- Определим номера пунктов доступных игроку(шашек противника <2)
    possiblePoints = map tpNumber (filter (\x -> length (filter (\y -> (chPlayer y) /= player) (tpCheckers x)) < 2) points)
    -- Определим наличие шашек игрока на баре
    playerBar = length $ filter (\x -> (chPlayer x) == player) (tbCheckers bar)
    -- Если шашки есть, проверим, может ли игрок походить ими
    -- вхождение кубиков в список доступные пункты
    resultBar = if playerBar /= 0 then
        length ( intersect possiblePoints [if player == PlayerOne then fst dices - 1 else 24 - fst dices, if player == PlayerOne then snd dices - 1 else 24 - snd dices]) > 0
    else
        True -- Если шашек нет, то надо проверить пункты
    -- Теперь надо определить доступные ходы шашками на доске
    -- Для этого прибавим(вычтем) к номерам пунктов принадлежащих игроку кубики
    -- И найдем вхождение получившегося списка со списком номеров доступных пунктов
    pts = if fst dices /=  0 then
        map (if player == PlayerOne then (+(fst dices)) else (subtract (fst dices))) playerPoints
    else
        []
    ptss = if snd dices /= 0 then
        map (if player == PlayerOne then (+(snd dices)) else (subtract (snd dices))) playerPoints
    else
        []
    ptsss = if (fst dices /= 0) && (snd dices /= 0) then
        map (if player == PlayerOne then (+ (fst dices + snd dices)) else (subtract (fst dices + snd dices))) playerPoints
    else 
        []
    ptssss = pts ++ ptss ++ ptsss
    resultPoints = ((length (intersect possiblePoints ptssss)) > 0) || playerBar /= 0
    result = resultPoints && resultBar
tryStepped :: GameBoard -> GameBoard
tryStepped board = newboard where
    polygon = gbBoardPolygon board
    points = gbPoints board
    bar = gbBar board
    checker = gbChecker board
    state = gbState board
    dices = gbDices board
    newstate = if (isFinish state) || ((not (canYouDo board)) && (not (canDeleteChecker board)) && currentAction state == Move) then 
        if currentAction state == Roll then
            ApplicationState (currentPlayer state) Move False
        else
            if currentPlayer state == PlayerOne then
                ApplicationState PlayerTwo Roll False
            else
                ApplicationState PlayerOne Roll False
    else
        state
    newdoubles = if (isFinish state) || ((not (canYouDo board)) && (not (canDeleteChecker board)) && currentAction state == Move) then 
        (False, False)
    else
        gbDoubles board
    newboard = GameBoard polygon points bar checker newstate dices newdoubles
canYouFinish :: GameBoard -> Bool
canYouFinish board = result where
    result = isFinish (gbState board)

win :: GameBoard -> PlayerName -> Bool
win board name = result where
    points = gbPoints board
    bar = gbBar board
    playerPoints = map tpNumber (filter (\x -> length (filter (\y -> (chPlayer y) == name) (tpCheckers x)) > 0) points)
    playerBar = length $ filter (\x -> (chPlayer x) == name) (tbCheckers bar)
    result = (length playerPoints == 0) && (playerBar == 0)