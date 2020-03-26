{-# LANGUAGE RecordWildCards #-}
module TablesBoardGame( run
    ) where

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
    hgeItr = lookupChecker (hgeMouseX, hgeMouseY) hgeCheckersPOne 0
    hgeMvChk = hgeCheckersPOne !! hgeItr
    hgeMvFlChk = chOnMv hgeMvChk
    hgeNewChk = DChcr (chInGame hgeMvChk) (chPnt hgeMvChk) (chPos hgeMvChk) hgeMouseX hgeMouseY hgeMvFlChk
    hgeNewChckersPO = swapChecker ((hgeItr >= 0) && (hgeItr < length hgeCheckersPOne) && hgeMvFlChk) hgeCheckersPOne hgeItr hgeNewChk
    hgeNewPOne = DPInf (piColor (adsPOne dgaState)) hgeNewChckersPO
    dgaNewState = DAppDtState (adsXScale dgaState) (adsYScale dgaState) hgeNewPOne playerTwo gameBoard POneMove 0 0 0
handleGameEvent (EventKey (MouseButton LeftButton) Up _ (hgeMouseX, hgeMouseY)) dgaState = dgaNewState where
    hgeCheckersPOne = piChcrs (adsPOne dgaState)
    hgeItr = lookupChecker (hgeMouseX, hgeMouseY) hgeCheckersPOne 0
    hgeMvChk = hgeCheckersPOne !! hgeItr
    -- Здесь необходимо проверить есть ли рядом пункт проверить можно ли туда поместить шашку и пересчитать координату шашки для пункта если надо
    hgeNewChk = DChcr (chInGame hgeMvChk) (chPnt hgeMvChk) (chPos hgeMvChk) (chX hgeMvChk) (chY hgeMvChk) False
    hgeNewChckersPO = swapChecker ((hgeItr >= 0) && (hgeItr < length hgeCheckersPOne)) hgeCheckersPOne hgeItr hgeNewChk
    hgeNewChckersPO' = calcCheckers hgeNewChckersPO (adsBoard dgaState)
    hgeNewPOne = DPInf (piColor (adsPOne dgaState)) hgeNewChckersPO'
    dgaNewState = DAppDtState (adsXScale dgaState) (adsYScale dgaState) hgeNewPOne playerTwo gameBoard POneMove 0 0 0
handleGameEvent (EventKey (SpecialKey KeySpace) Down _ _) dgaState = dgaState
handleGameEvent _ dgaState = dgaState
  
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
    --let setPlayerOne = DPInf (piColor playerOne) (calcCheckers (piChcrs playerOne) gameBoard)
    --let setPlayerTwo = DPInf (piColor playerTwo) (calcCheckers (piChcrs playerTwo) gameBoard)
    let initGameState = DAppDtState runWScale runHScale playerOne playerTwo gameBoard POneMove 0 0 0
    play dispMode bgColor fps initGameState drawGameApp handleGameEvent updateGameApp