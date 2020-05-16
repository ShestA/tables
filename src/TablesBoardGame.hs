{-# LANGUAGE RecordWildCards #-}
module TablesBoardGame( run
    ) where
    
-- Подключаемые модули
import System.IO
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment
import Data.Maybe
import System.Random

import AbstractData
import Constants
import AdditionalFunction

-- ******************************************************************
-- Описание основных функций программы

-- Функция отрисовки всех изображений
drawGameApp :: ApplicationData -> Picture
drawGameApp dgaData = Pictures [result, fullpic, winpic] where
    (dgaScaleX,dgaScaleY) = adScale dgaData
    winpic = if win (adBoard dgaData) PlayerOne then
        Translate (-360 * dgaScaleX) (0) $ Scale (0.4 * dgaScaleX) (0.4 * dgaScaleY) (Text ("Player One WIN"))
    else
        if win (adBoard dgaData) PlayerTwo then
            Translate (-360 * dgaScaleX) (0) $ Scale (0.4 * dgaScaleX) (0.4 * dgaScaleY) (Text ("Player Two WIN"))
        else
            Blank
    fullpic = if (win (adBoard dgaData) PlayerOne) || (win (adBoard dgaData) PlayerOne) then
        Blank
    else
        Pictures [infopic, opportunitypic, pospic, deletepic] where
            infopic = if currentPlayer (gbState (adBoard dgaData)) == PlayerOne then
                if currentAction (gbState (adBoard dgaData)) == Move then
                    Translate (-600 * dgaScaleX) 0 $ Scale (0.3 * dgaScaleX) (0.3 * dgaScaleY) (Text ("Player One Move " ++ show (gbDices (adBoard dgaData))))
                else
                    if currentAction (gbState (adBoard dgaData)) == Roll then
                        Translate (-600 * dgaScaleX) 0 $ Scale (0.3 * dgaScaleX) (0.3 * dgaScaleY) (Text ("Player One Roll"))
                    else
                        Translate (-600 * dgaScaleX) 0 $ Scale (0.3 * dgaScaleX) (0.3 * dgaScaleY) (Text ("Player One Drop " ++ show (gbDices (adBoard dgaData))))
            else
                if currentAction (gbState (adBoard dgaData)) == Move then
                    Translate (300 * dgaScaleX) 0 $ Scale (0.3 * dgaScaleX) (0.3 * dgaScaleY) (Text ("Player Two Move " ++ show (gbDices (adBoard dgaData))))
                else
                    if currentAction (gbState (adBoard dgaData)) == Roll then
                        Translate (300 * dgaScaleX) 0 $ Scale (0.3 * dgaScaleX) (0.3 * dgaScaleY) (Text ("Player Two Roll"))
                    else
                        Translate (300 * dgaScaleX) 0 $ Scale (0.3 * dgaScaleX) (0.3 * dgaScaleY) (Text ("Player Two Drop " ++ show (gbDices (adBoard dgaData))))
            opportunitypic = if currentPlayer (gbState (adBoard dgaData)) == PlayerOne then
                if canYouDo (adBoard dgaData) then
                    Translate (-600 * dgaScaleX) (-50 * dgaScaleY) $ Scale (0.2 * dgaScaleX) (0.1 * dgaScaleY) (Text ("Player One have moves"))
                else
                    Translate (-600 * dgaScaleX) (-50 * dgaScaleY) $ Scale (0.2 * dgaScaleX) (0.2 * dgaScaleY) (Text ("Player One dont have moves"))
            else
                if canYouDo (adBoard dgaData) then
                    Translate (300 * dgaScaleX) (-50 * dgaScaleY) $ Scale (0.2 * dgaScaleX) (0.2 * dgaScaleY) (Text ("Player Two have moves"))
                else
                    Translate (300 * dgaScaleX) (-50 * dgaScaleY) $ Scale (0.2 * dgaScaleX) (0.2 * dgaScaleY) (Text ("Player Two don't have moves"))
            pospic = if (canYouFinish (adBoard dgaData)) then 
                Translate (-560 * dgaScaleX) (510 * dgaScaleY) $ Scale (0.2 * dgaScaleX) (0.2 * dgaScaleY) (Text ("You can finish the move"))
            else
                Translate (-560 * dgaScaleX) (510 * dgaScaleY) $ Scale (0.2 * dgaScaleX) (0.2 * dgaScaleY) (Text ("You need to make a move"))
            deletepic = if canDeleteChecker (adBoard dgaData) then
                Translate (360 * dgaScaleX) (510 * dgaScaleY) $ Scale (0.2 * dgaScaleX) (0.2 * dgaScaleY) (Text ("You can delete checker"))
            else
                Translate (360 * dgaScaleX) (510 * dgaScaleY) $ Scale (0.2 * dgaScaleX) (0.2 * dgaScaleY) (Text ("You can't delete checker"))
    result = scale (dgaScaleX) (dgaScaleY) (getGamePicture (adBoard dgaData)) 

-- Обработчик событий
handleGameEvent :: Event -> ApplicationData -> ApplicationData
handleGameEvent (EventKey (MouseButton LeftButton) Down _ (hgeMX, hgeMY)) x = xnew where
    (hgeSX, hgeSY) = adScale x
    hgeNewBoard = if (win (adBoard x) PlayerOne) || (win (adBoard x) PlayerOne) then
        adBoard x
    else
        attachMovingChecker (hgeMX / hgeSX, hgeMY / hgeSY) (adBoard x)
    xnew        = ApplicationData (adScale x) (adGen x) hgeNewBoard
handleGameEvent (EventKey (MouseButton LeftButton) Up _ (hgeMX, hgeMY)) x = xnew where
    (hgeSX, hgeSY) = adScale x
    hgeNewBoard = if (win (adBoard x) PlayerOne) || (win (adBoard x) PlayerOne) then
        adBoard x
    else
        detachMovingChecker (hgeMX / hgeSX, hgeMY / hgeSY) (adBoard x)
    xnew        = ApplicationData (adScale x) (adGen x) hgeNewBoard
handleGameEvent (EventKey (MouseButton RightButton) Down _ (hgeMX, hgeMY)) x = xnew where
    hgeNewBoard = if (win (adBoard x) PlayerOne) || (win (adBoard x) PlayerOne) then
        adBoard x
    else
        tryDeleteChecker (adBoard x)
    xnew        = ApplicationData (adScale x) (adGen x) hgeNewBoard
handleGameEvent (EventMotion (hgeMX, hgeMY)) x = xnew where
    (hgeSX, hgeSY) = adScale x
    hgeNewBoard = if (win (adBoard x) PlayerOne) || (win (adBoard x) PlayerOne) then
        adBoard x
    else
        translateMovingChecker (hgeMX / hgeSX, hgeMY / hgeSY) (adBoard x)
    xnew        = ApplicationData (adScale x) (adGen x) hgeNewBoard
handleGameEvent (EventKey (SpecialKey KeySpace) Down _ _) x = xnew where
    xnew        = if (win (adBoard x) PlayerOne) || (win (adBoard x) PlayerOne) then
        x
    else
        rollDices x
handleGameEvent (EventKey (SpecialKey KeyEnter) Down _ _) x = xnew where
    xnew        = if (win (adBoard x) PlayerOne) || (win (adBoard x) PlayerOne) then
        x
    else
        ApplicationData (adScale x) (adGen x) (tryStepped (adBoard x))
handleGameEvent (EventKey (SpecialKey KeyF12) Down _ _) x = xnew where
    xnew        = ApplicationData (adScale x) (adGen x) baseGameBoard
handleGameEvent _ x = x

-- Обработчик кадра
updateGameApp :: Float -> ApplicationData -> ApplicationData
updateGameApp _ x = x

-- Игровая доска
baseGameBoard :: GameBoard
baseGameBoard = GameBoard baseBoardPolygon (map recalcCheckerPoint baseTablesPoints) (recalcCheckerBar baseBar) (Nothing, Nothing) baseGameState (1, 1)

-- ******************************************************************
-- Описание основной функции программы
run :: IO ()
run = do
    runScreenSize <- getScreenSize
    let runScreenWidth = fromIntegral (fst runScreenSize)
    let runScreenHeight = fromIntegral (snd runScreenSize)
    let (runWidthScale, runHeightScale) = getScreenScale (runScreenWidth, runScreenHeight)
    gen <- getStdGen
    let initGameState = ApplicationData (runWidthScale, runHeightScale) gen baseGameBoard
    play dispMode bgColor fps initGameState drawGameApp handleGameEvent updateGameApp