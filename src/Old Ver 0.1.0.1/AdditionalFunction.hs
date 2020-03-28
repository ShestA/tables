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
getScrScale :: (Float, Float) -> (Float, Float)
getScrScale (gsScrW, gsScrH) = (gsScaleX, gsScaleY) where
    gsScaleX =  gsScrW / baseScreenWidth
    gsScaleY =  gsScrH / baseScreenHeight

 -- Проверка нажатия кнопки на шашке
onCheckerClick :: DPInf -> (Float, Float) -> Bool
onCheckerClick occPlayer (occX, occY) = occFlag where
    occCheckers = piChcrs occPlayer 
    occFlag = any (==True) (map func occCheckers) where
            func = occProbe occX occY where
                occProbe :: Float -> Float -> DChcr -> Bool
                occProbe tstX tstY (DChcr _ _ _ prbX prbY _) =  tstFl where
                    tstFl = if ( (abs(tstX - prbX) < radiusChecker) && (abs(tstY - prbY) < radiusChecker) ) -- РАДИУС НЕ МАСШТАБИРУЕМ!!!!
                    then
                        True
                     else
                        False

-- Создание рисунка пункта доски без цвета
createPointPic :: DChPoint ->Picture
createPointPic cppPnt = cppPicPoint where
    cppPntCoord = cpCoord cppPnt                    -- AuxCoord Координаты начала пункта
    cppPntXCrd = acX cppPntCoord                    -- Float    Координата Х начала пункта
    cppPntYCrd = acY cppPntCoord                    -- Float    Координата У начала пункта
    cppPntDir = cpDirec cppPnt                      -- Bool     Направление пункта
    cppPicPoint = if cppPntDir then                 -- Picture  Создание изображения пункта
        Polygon [((cppPntXCrd - pointWidth), cppPntYCrd),
                ((cppPntXCrd),((cppPntYCrd - pointLen))),
                ((cppPntXCrd + pointWidth), cppPntYCrd)]
    else
        Polygon [((cppPntXCrd - pointWidth), cppPntYCrd),
                ((cppPntXCrd),((cppPntYCrd + pointLen))),
                ((cppPntXCrd + pointWidth), cppPntYCrd)]

-- Создание полного рисунка доски
createBoardPic :: DGBoard -> Picture
createBoardPic cbBoard = cbPicBrd where
    cbBrdRect = gbBrdRect cbBoard       --AuxRect       Прямоугольник поля
    cbBrdTop = arTop cbBrdRect          --Float         Верхняя грань поля
    cbBrdLeft = arLeft cbBrdRect        --Float         Левая грань поля
    cbBrdRight = arRight cbBrdRect      --Float         Правая грань поля
    cbBrdBottom = arBottom cbBrdRect    --Float         Нижняя грань поля
    cbRdPtsBrd = gbRdPntsBrd cbBoard    --DChPntList    Структура красных пунктов
    cbRdColor = cplColor cbRdPtsBrd     --Color         Цвет красных пунктов
    cbWhtPtsBrd = gbWhtPntsBrd cbBoard  --DChPntList    Структура белых пунктов
    cbWhtColor = cplColor cbWhtPtsBrd   --Color         Цвет белых пунктов
    cbRdPtsLst = cplPnts cbRdPtsBrd     --[DChPoint]    Список красных пунктов
    cbWhtPtsLst = cplPnts cbWhtPtsBrd   --[DChPoint]    Список красных пунктов
    cbColor = gbBrdClr cbBoard          --Color         Цвет поля
    cbRdPics = map func cbRdPtsLst where                    -- Создание списка рисунков красных пунктов
        func = createPointPic
    cbWhtPics = map func cbWhtPtsLst where                  -- Создание списка рисунков белых пунктов
        func = createPointPic
    cbClRdPics = Color cbRdColor (Pictures cbRdPics)        -- Цветное изображение красных пунктов
    cbClWhtPics = Color cbWhtColor (Pictures cbWhtPics)     -- Цветное изображение белых пунктов
    cbRectPic = Color cbColor (Polygon [(cbBrdLeft, cbBrdTop),
        (cbBrdRight, cbBrdTop),
        (cbBrdRight, cbBrdBottom),
        (cbBrdLeft, cbBrdBottom)])    -- Изображение поля
    cbPicBrd = Pictures[cbRectPic, cbClRdPics, cbClWhtPics] -- Итоговое изображение доски

-- Создание рисунка шашки без цвета
createOneCheckerPic :: DChcr -> Picture
createOneCheckerPic cocChecker = cocPic where
    cocX = chX cocChecker
    cocY = chY cocChecker
    cocChckPic = ThickCircle (radiusChecker) (30)
    cocPic = Translate (cocX) (cocY) cocChckPic
    
-- Создание русунка шашек одного цвета
createCheckersPic :: Color -> [DChcr] -> Picture
createCheckersPic ccpColor ccpChckrs = ccpPic where
    ccpPicLst = map func ccpChckrs where
        func = createOneCheckerPic
    ccpPic = Color ccpColor (Pictures ccpPicLst)

-- Поиск индекса шашки по координатам
lookupChecker :: (Float, Float) -> [DChcr] -> Int -> Int
lookupChecker _ [ ] lcLen = lcLen  -- Нужной шашки нет
lookupChecker (lcX, lcY) ( (DChcr _ _ _ lcCX lcCY _) : lcOthers ) lcItr 
    | ((abs(lcX - lcCX) < radiusChecker) && (abs(lcY - lcCY) < radiusChecker)) = lcItr -- Шашка найдена
    | otherwise = lookupChecker (lcX, lcY) lcOthers (lcItr + 1) -- Переходим к следующей шашке
    
-- Поиск шашки в движении
lookupMoveChecker :: [DChcr] -> Int -> Int
lookupMoveChecker [] lmcLen = lmcLen -- Нужной шашки нет
lookupMoveChecker ( (DChcr _ _ _ _ _ lmcFlag) : lmcOthers ) lmcItr
    | lmcFlag = lmcItr -- Шашка найдена
    | otherwise = lookupMoveChecker lmcOthers (lmcItr + 1) -- Переходим к следующей шашке

-- Поиск пункта для шашки по координате шашки
lookupCheckerPoint :: (Float, Float) -> [DChPoint] -> Int -> Int
lookupCheckerPoint _ [] lcpLen = lcpLen -- Нужного пункта нет
lookupCheckerPoint (lcpX, lcpY) ((DChPoint (AuxCoord lcpPX lcpPY) lcpPDir _ _ ): lcpOthers) lcpItr
    | ((abs(lcpY - lcpPY) < pointLen)&&(abs(lcpX - lcpPX) < pointWidth)) = lcpItr -- Пункт найден
    | otherwise = lookupCheckerPoint (lcpX, lcpY) lcpOthers (lcpItr + 1) -- Переходим к следующему пункту
    -- | ((lcpY >= if lcpPDir then (lcpPY - pointLen) else lcpPY) && (lcpY <= if lcpPDir then (lcpPY) else (lcpPY + pointLen)) && ()&&())

-- Поиск всех шашек находящихся в пункте и пересчет их номеров


-- Заменить шашку в листе
swapChecker :: Bool -> [DChcr] -> Int -> DChcr -> [DChcr]
swapChecker False x _ _ = x
swapChecker True scOldList scCur scNewEl = scNewList where
    scFstPart= fst (splitAt scCur scOldList)
    scSndPart= tail (snd (splitAt scCur scOldList))
    scNewList = scFstPart ++ [scNewEl] ++ scSndPart

-- Заменить пункт в листе
swapPoint :: Bool -> [DChPoint] -> Int -> DChPoint -> [DChPoint]
swapPoint False x _ _ = x
swapPoint True swpOldList swpCur swpNewEl = swpNewList where
    swpFstPart = fst(splitAt swpCur swpOldList)
    swpSndPart = tail(snd (splitAt swpCur swpOldList))
    swpNewList = swpFstPart ++ [swpNewEl] ++ swpSndPart

-- Пересчет координаты шашки
searchPoint :: DChcr -> DChPoint -> DChcr
searchPoint spChcr spPoint = spnewChcr where
    spPntN = cpN spPoint
    spChN = chPnt spChcr
    spPntCoord = cpCoord spPoint
    spPntCoordX = acX spPntCoord
    spPntCoordY = acY spPntCoord
    spPntDir = cpDirec spPoint
    spPntNEl = cpNEl spPoint
    spPos = chPos spChcr
    spShift = (pointLen / fromIntegral(spPntNEl))
    spCoordX = spPntCoordX
    spCoordY = spPntCoordY   + if spPntDir then
        - ((spShift * fromIntegral(spPos - 1)) + (radiusChecker))
    else
        ((spShift * fromIntegral(spPos - 1)) + (radiusChecker))
    spnewChcr = DChcr True spChN spPos spCoordX spCoordY False

-- Поиск индекса пункта по номеру
lookupPoint :: Int -> [DChPoint] -> Int -> Int
lookupPoint _ [ ] lpLen = lpLen  -- Нужного пункта нет
lookupPoint lpPnt ( (DChPoint _ _ _ lpNPnt) : lpOthers ) lpItr 
    | lpPnt == lpNPnt = lpItr -- Пункт найден
    | otherwise = lookupPoint lpPnt lpOthers (lpItr + 1) -- Переходим к следующему пункту

-- Запуск пересчета координаты шашки
recalcChecker :: [DChPoint] -> DChcr -> DChcr
recalcChecker rccPts rccChcr = rccnewChcr where
    rccFlMv = chOnMv rccChcr
    rccFlInGame = chInGame rccChcr
    rccnewChcr = if rccFlMv then
        rccChcr -- Игрок двигает шашку
    else
        if rccFlInGame then
            searchPoint rccChcr (rccPts !! (lookupPoint (chPnt rccChcr) rccPts 0))
        else
            rccChcr -- Шашка на баре

-- Расчет координат шашек игрока 
calcCheckers :: [DChcr] -> DGBoard -> [DChcr]
calcCheckers ccChcks ccBoard = ccNewChcks where
    ccPntListRd = gbRdPntsBrd ccBoard
    ccPntListWht = gbWhtPntsBrd ccBoard
    ccPntList = (cplPnts ccPntListRd) ++ (cplPnts ccPntListWht)
    ccNewChcks = map func ccChcks where
        func = recalcChecker ccPntList