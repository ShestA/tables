{-# LANGUAGE RecordWildCards #-}
module TablesBoardGame( run
    ) where
    
-- Подключаемые модули
import System.IO
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment
import AbstractData
import Constants
import AdditionalFunction

-- ******************************************************************
-- Описание основных функций программы

-- Функция ТОЛЬКО отрисовки всех изображений
drawGameApp :: DAppDtState -> Picture
drawGameApp dgaAppDS = scale dgaXScale dgaYScale dgaAllPics where
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
    
    dgaPOnePic = createCheckersPic dgaPOneColor dgaChcrsPOne
    dgaPTwoPic = createCheckersPic dgaPTwoColor dgaChcrsPTwo
    dgaBoardPic = createBoardPic dgaBoard
    
    dgaAllPics = Pictures [dgaBoardPic, dgaPOnePic, dgaPTwoPic]
    
-- Обработчик событий
handleGameEvent :: Event -> DAppDtState -> DAppDtState
handleGameEvent (EventKey (MouseButton LeftButton) Down _ (hgeMouseX, hgeMouseY)) dgaState = dgaNewState where
    hgeCheckersPOne = piChcrs (adsPOne dgaState)
    hgeItr = lookupChecker (hgeMouseX, hgeMouseY) hgeCheckersPOne 0
    hgeMvChk = hgeCheckersPOne !! hgeItr
    hgeNewChk = DChcr (chInGame hgeMvChk) (chPnt hgeMvChk) (chPos hgeMvChk) hgeMouseX hgeMouseY True
    hgeNewChckersPO = swapChecker ((hgeItr >= 0) && (hgeItr < length hgeCheckersPOne)) hgeCheckersPOne hgeItr hgeNewChk
    hgeNewPOne = DPInf (piColor (adsPOne dgaState)) hgeNewChckersPO
    dgaNewState = DAppDtState (adsXScale dgaState) (adsYScale dgaState) hgeNewPOne playerTwo gameBoard POneMove 0 0 0
handleGameEvent (EventMotion (hgeMouseX, hgeMouseY)) dgaState = dgaNewState where
    hgeCheckersPOne = piChcrs (adsPOne dgaState)
    hgeItr = lookupMoveChecker  hgeCheckersPOne 0
    hgeMvChk = hgeCheckersPOne !! hgeItr
    hgeMvFlChk = chOnMv hgeMvChk
    hgeNewChk = DChcr (chInGame hgeMvChk) (chPnt hgeMvChk) (chPos hgeMvChk) hgeMouseX hgeMouseY hgeMvFlChk
    hgeNewChckersPO = swapChecker ((hgeItr >= 0) && (hgeItr < length hgeCheckersPOne) && hgeMvFlChk) hgeCheckersPOne hgeItr hgeNewChk
    hgeNewPOne = DPInf (piColor (adsPOne dgaState)) hgeNewChckersPO
    dgaNewState = DAppDtState (adsXScale dgaState) (adsYScale dgaState) hgeNewPOne playerTwo gameBoard POneMove 0 0 0
handleGameEvent (EventKey (MouseButton LeftButton) Up _ (hgeMouseX, hgeMouseY)) dgaState = dgaNewState where
    hgeCheckersPOne = piChcrs (adsPOne dgaState)
    hgeItr = lookupChecker  (hgeMouseX, hgeMouseY) hgeCheckersPOne 0
    hgeMvChk = hgeCheckersPOne !! hgeItr
    hgePointListRed = (cplPnts(gbRdPntsBrd (adsBoard dgaState)))
    hgePointListWhite = (cplPnts(gbWhtPntsBrd (adsBoard dgaState)))
    hgePointList =  hgePointListRed ++ hgePointListWhite
    hgeItrPointDetected = lookupCheckerPoint (hgeMouseX, hgeMouseY) hgePointList 0
    hgeNewPoint' = hgePointList !! hgeItrPointDetected
    hgeNewPoint = DChPoint (cpCoord hgeNewPoint') (cpDirec hgeNewPoint') ((cpNEl hgeNewPoint' )+1) (cpN hgeNewPoint')
    hgeNewPointListRed = swapPoint (hgeItrPointDetected < length hgePointListRed) hgePointListRed hgeItrPointDetected hgeNewPoint
    hgeNewPointListWhite = swapPoint ((hgeItrPointDetected >= length hgePointListRed)&&(hgeItrPointDetected < length hgePointList)) hgePointListWhite (hgeItrPointDetected - length hgePointListRed) hgeNewPoint
    hgeNewPointsRed = DChPntList hgeNewPointListRed (cplColor (gbRdPntsBrd (adsBoard dgaState)))
    hgeNewPointsWhite = DChPntList hgeNewPointListWhite (cplColor (gbWhtPntsBrd (adsBoard dgaState)))
    hgeNewBoard = DGBoard deckRect deckColor hgeNewPointsRed hgeNewPointsWhite  pointLen
    -- Здесь необходимо проверить есть ли рядом пункт проверить можно ли туда поместить шашку и пересчитать координату шашки для пункта если надо
    hgeNewChk = DChcr (chInGame hgeMvChk) (cpN hgeNewPoint) (cpNEl hgeNewPoint) (chX hgeMvChk) (chY hgeMvChk) False
    hgeNewChckersPO = swapChecker ((hgeItr >= 0) && (hgeItr < length hgeCheckersPOne)) hgeCheckersPOne hgeItr hgeNewChk
    hgeNewChckersPO' = calcCheckers hgeNewChckersPO (hgeNewBoard)
    hgeNewPOne = DPInf (piColor (adsPOne dgaState)) hgeNewChckersPO'
    dgaNewState = DAppDtState (adsXScale dgaState) (adsYScale dgaState) hgeNewPOne playerTwo hgeNewBoard POneMove 0 0 0
handleGameEvent (EventKey (SpecialKey KeySpace) Down _ _) dgaState = dgaState
handleGameEvent (EventKey (SpecialKey KeyF12) Down _ _) (DAppDtState hgeWScale hgeHScale _ _ _ _ _ _ _) = dgaStartState where
    dgaStartState =  DAppDtState hgeWScale hgeHScale playerOne playerTwo gameBoard POneMove 0 0 0
handleGameEvent _ dgaState = dgaState
  
-- Обработчик кадра
updateGameApp :: Float -> DAppDtState -> DAppDtState
updateGameApp _ ugaAppDS = ugaAppDS

-- ******************************************************************
-- Описание основной функции программы
run :: IO ()
run = do
    runScrSz <- getScreenSize
    let runScrW = fromIntegral (fst runScrSz)
    let runScrH = fromIntegral (snd runScrSz)
    let (runWScale, runHScale) = getScrScale (runScrW, runScrH)
    let setPlayerOne = DPInf (piColor playerOne) (calcCheckers (piChcrs playerOne) gameBoard)
    let setPlayerTwo = DPInf (piColor playerTwo) (calcCheckers (piChcrs playerTwo) gameBoard)
    let initGameState = DAppDtState runWScale runHScale setPlayerOne setPlayerTwo gameBoard POneMove 0 0 0
    play dispMode bgColor fps initGameState drawGameApp handleGameEvent updateGameApp