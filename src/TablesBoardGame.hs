{-# LANGUAGE RecordWildCards #-}
module TablesBoardGame( run
    ) where
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
        adsLastUpd  :: Float        -- Время с последнего обновления
    }

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
pOneColor = makeColorI 215 215 215 255

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
pointWidth = 80

-- Радиус шашки
radiusChecker :: Float
radiusChecker = 40

-- Игровая доска
gameBoard :: DGBoard
gameBoard = DGBoard deckRect deckColor redDeckPoints whiteDeckPoints pointLen

-- ******************************************************************
-- Описание вспомогательных функций программы

-- Получение масштабного коэффициента
getScrScale :: (Float, Float) -> (Float, Float)
getScrScale (gsScrW, gsScrH) = (gsScaleX, gsScaleY) where
    gsScaleX =  gsScrW / baseScreenWidth
    gsScaleY =  gsScrH / baseScreenHeight

-- Создание рисунка пункта доски без цвета
createPointPic :: Float -> Float -> DChPoint ->Picture
createPointPic cppScaleX cppScaleY cppPnt = cppPicPoint where
    cppPntCoord = cpCoord cppPnt                    -- AuxCoord Координаты начала пункта
    cppPntXCrd = acX cppPntCoord                    -- Float    Координата Х начала пункта
    cppPntYCrd = acY cppPntCoord                    -- Float    Координата У начала пункта
    cppPntDir = cpDirec cppPnt                      -- Bool     Направление пункта
    cppPicPoint = if cppPntDir then                 -- Picture  Создание изображения пункта
        Polygon [(cppScaleX * (cppPntXCrd - pointWidth), cppScaleY * cppPntYCrd),
                ((cppScaleX * cppPntXCrd),(cppScaleY * (cppPntYCrd - pointLen))),
                (cppScaleX * (cppPntXCrd + pointWidth), cppScaleY * cppPntYCrd)]
    else
        Polygon [(cppScaleX * (cppPntXCrd - pointWidth), cppScaleY * cppPntYCrd),
                ((cppScaleX * cppPntXCrd),(cppScaleY * (cppPntYCrd + pointLen))),
                (cppScaleX * (cppPntXCrd + pointWidth), cppScaleY * cppPntYCrd)]

-- Создание полного рисунка доски
createBoardPic :: Float -> Float -> DGBoard -> Picture
createBoardPic cbScaleX cbScaleY cbBoard = cbPicBrd where
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
        func = createPointPic cbScaleX cbScaleY
    cbWhtPics = map func cbWhtPtsLst where                  -- Создание списка рисунков белых пунктов
        func = createPointPic cbScaleX cbScaleY
    cbClRdPics = Color cbRdColor (Pictures cbRdPics)        -- Цветное изображение красных пунктов
    cbClWhtPics = Color cbWhtColor (Pictures cbWhtPics)     -- Цветное изображение белых пунктов
    cbRectPic = Color cbColor (Polygon [(cbScaleX * cbBrdLeft, cbScaleY * cbBrdTop),
        (cbScaleX * cbBrdRight, cbScaleY * cbBrdTop),
        (cbScaleX * cbBrdRight, cbScaleY * cbBrdBottom),
        (cbScaleX * cbBrdLeft, cbScaleY * cbBrdBottom)])    -- Изображение поля
    cbPicBrd = Pictures[cbRectPic, cbClRdPics, cbClWhtPics] -- Итоговое изображение доски

-- Создание рисунка шашки без цвета
createOneCheckerPic :: Float -> Float -> DChcr -> Picture
createOneCheckerPic cocScaleX cocScaleY cocChecker = cocPic where
    cocX = chX cocChecker
    cocY = chY cocChecker
    cocRadScale = cocScaleX / cocScaleY
    cocChckPic = ThickCircle (cocRadScale * radiusChecker) (cocRadScale * radiusChecker)
    cocPic = Translate (cocScaleX * cocX) (cocScaleY * cocY) cocChckPic
    
-- Создание русунка шашек одного цвета
createCheckersPic :: Float -> Float -> Color -> [DChcr] -> Picture
createCheckersPic ccpScaleX ccpScaleY ccpColor ccpChckrs = ccpPic where
    ccpPicLst = map func ccpChckrs where
        func = createOneCheckerPic ccpScaleX ccpScaleY
    ccpPic = Color ccpColor (Pictures ccpPicLst)

-- Пересчет координаты шашки
searchPoint :: Float -> Float -> DChcr -> DChPoint -> DChcr
searchPoint spScaleX spScaleY spChcr spPoint = spnewChcr where
    spPntN = cpN spPoint
    spChN = chPnt spChcr
    spnewChcr = DChcr True spPntN spPos spCoordX spCoordY False where
            spPntCoord = cpCoord spPoint
            spPntCoordX = acX spPntCoord
            spPntCoordY = acY spPntCoord
            spPntDir = cpDirec spPoint
            spPntNEl = cpNEl spPoint
            spPos = chPos spChcr
            spShift = spScaleY * (pointLen / fromIntegral(spPntNEl))
            spCoordX = spPntCoordX
            spCoordY = spPntCoordY + if spPntDir then
                - ((spShift * fromIntegral(spPos - 1)) + (spScaleY * radiusChecker))
            else
                ((spShift * fromIntegral(spPos - 1)) + (spScaleY * radiusChecker))

-- Запуск пересчета координаты шашки
recalcChecker :: Float -> Float -> [DChPoint] -> DChcr -> DChcr
recalcChecker rccScaleX rccScaleY rccPts rccChcr = rccnewChcr where
    rccFlMv = chOnMv rccChcr
    rccFlInGame = chInGame rccChcr
    rccnewChcr = if rccFlMv then
        rccChcr -- Игрок двигает шашку
    else
        if rccFlInGame then
            searchPoint rccScaleX rccScaleY rccChcr (rccPts !! (if even (chPnt rccChcr) then
                (div (chPnt rccChcr) 2) - 1
            else
                (div (chPnt rccChcr) 2) + 12)) -- Шашка в пункте
        else
            rccChcr -- Шашка на баре

-- Расчет координат шашек игрока 
calcCheckers :: Float -> Float -> [DChcr] -> DGBoard -> [DChcr]
calcCheckers ccScaleX ccScaleY ccChcks ccBoard = newccChcks where
    ccPntListRd = gbRdPntsBrd ccBoard
    ccPntListWht = gbWhtPntsBrd ccBoard
    ccPntList = (cplPnts ccPntListRd) ++ (cplPnts ccPntListWht)
    newccChcks = map func ccChcks where
        func = recalcChecker ccScaleX ccScaleY ccPntList

-- ******************************************************************
-- Описание основных функций программы

-- Функция ТОЛЬКО отрисовки всех изображений
drawGameApp :: DAppDtState -> Picture
drawGameApp dgaAppDS = Pictures[dgaAllPics] where
    dgaXScale = adsXScale dgaAppDS -- Float
    dgaYScale = adsYScale dgaAppDS --Float
    dgaPOne = adsPOne dgaAppDS --DPInf
    dgaPTwo = adsPTwo dgaAppDS --DPInf
    dgaBoard = adsBoard dgaAppDS --DGBoard
    dgaAppState = adsState dgaAppDS --AppStates
    dgaPOneColor = piColor dgaPOne --Color
    dgaPTwoColor = piColor dgaPTwo --Color
    dgaChcrsPOne = piChcrs dgaPOne --[DChcr]
    dgaChcrsPTwo = piChcrs dgaPTwo --[DChcr]
    
    dgaPOnePic = createCheckersPic dgaXScale dgaYScale dgaPOneColor dgaChcrsPOne
    dgaPTwoPic = createCheckersPic dgaXScale dgaYScale dgaPTwoColor dgaChcrsPTwo
    dgaBoardPic = createBoardPic dgaXScale dgaYScale dgaBoard
    
    dgaAllPics = Pictures [dgaBoardPic, dgaPOnePic, dgaPTwoPic]

-- Обработчик событий
handleGameEvent :: Event -> DAppDtState -> DAppDtState
handleGameEvent _ state = state
    
-- Обработчик кадра
updateGameApp :: Float -> DAppDtState -> DAppDtState
updateGameApp _ ugaAppDS = ugaAppDS
{-
updateGameApp ugaTime ugaAppDS = newUGAppDS where
    ugaXScale = adsXScale ugaAppDS -- Float
    ugaYScale = adsYScale ugaAppDS --Float
    ugaPOne = adsPOne ugaAppDS --DPInf
    ugaPTwo = adsPTwo ugaAppDS --DPInf
    ugaBoard = adsBoard ugaAppDS --DGBoard
    ugaAppState = adsState ugaAppDS --AppStates
    ugaPOneColor = piColor ugaPOne --Color
    ugaPTwoColor = piColor ugaPTwo --Color
    ugaChcrsPOne = piChcrs ugaPOne --[DChcr]
    ugaChcrsPTwo = piChcrs ugaPTwo --[DChcr]
    ugaBrdRect = gbBrdRect ugaBoard --AuxRect
    ugaRdPtsBrd = gbRdPntsBrd ugaBoard --DChPntList
    ugaWhtPtsBrd = gbWhtPntsBrd ugaBoard --DChPntList
    ugaLastUpd = adsLastUpd ugaAppDS --Float
    
    ugaNewPOne = DPInf ugaPOneColor (calcCheckers ugaXScale ugaYScale ugaChcrsPOne ugaBoard)
    ugaNewPTwo = DPInf ugaPTwoColor (calcCheckers ugaXScale ugaYScale ugaChcrsPTwo ugaBoard)
    newUGAppDS = DAppDtState ugaXScale ugaYScale ugaNewPOne ugaNewPTwo ugaBoard ugaAppState (ugaLastUpd + ugaTime)
-}
-- ******************************************************************
-- Описание основной функции программы
run :: IO ()
run = do
    runScrSz <- getScreenSize
    let runScrW = fromIntegral (fst runScrSz)
    let runScrH = fromIntegral (snd runScrSz)
    let (runWScale, runHScale) = getScrScale (runScrW, runScrH)
    let setPlayerOne = DPInf (piColor playerOne) (calcCheckers 1 1 (piChcrs playerOne) gameBoard)
    let setPlayerTwo = DPInf (piColor playerTwo) (calcCheckers 1 1 (piChcrs playerTwo) gameBoard)
    let initGameState = DAppDtState runWScale runHScale setPlayerOne setPlayerTwo gameBoard POneMove 0
    play dispMode bgColor fps initGameState drawGameApp handleGameEvent updateGameApp