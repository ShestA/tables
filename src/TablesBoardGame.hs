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
drawGameApp dgaData = Pictures [result,infopic] where
    infopic = if currentPlayer (gbState (adBoard dgaData)) == PlayerOne then
        if currentAction (gbState (adBoard dgaData)) == Move then
            Scale 0.3 0.3 (Text ("Player One Move" ++ show (gbDices (adBoard dgaData))))
        else
            Scale 0.3 0.3 (Text ("Player One Roll" ++ show (gbDices (adBoard dgaData))))
    else
        if currentAction (gbState (adBoard dgaData)) == Move then
            Scale 0.3 0.3 (Text ("Player Two Move" ++ show (gbDices (adBoard dgaData))))
        else
            Scale 0.3 0.3 (Text ("Player Two Roll" ++ show (gbDices (adBoard dgaData))))
    result = scale (fst(adScale dgaData)) (snd(adScale dgaData)) (getGamePicture (adBoard dgaData)) 

-- Обработчик событий
handleGameEvent :: Event -> ApplicationData -> ApplicationData
handleGameEvent (EventKey (MouseButton LeftButton) Down _ (hgeMX, hgeMY)) x = xnew where
    (hgeSX, hgeSY) = adScale x
    hgeNewBoard = attachMovingChecker (hgeMX / hgeSX, hgeMY / hgeSY) (adBoard x) -- При нестандартном разрешении координаты мыши плывут(БАГ)
    xnew        = ApplicationData (adScale x) (adGen x) hgeNewBoard
handleGameEvent (EventMotion (hgeMX, hgeMY)) x = xnew where
    (hgeSX, hgeSY) = adScale x
    hgeNewBoard = translateMovingChecker (hgeMX / hgeSX, hgeMY / hgeSY) (adBoard x)
    xnew        = ApplicationData (adScale x) (adGen x) hgeNewBoard
handleGameEvent (EventKey (MouseButton LeftButton) Up _ (hgeMX, hgeMY)) x = xnew where
    (hgeSX, hgeSY) = adScale x
    hgeNewBoard = detachMovingChecker (hgeMX / hgeSX, hgeMY / hgeSY) (adBoard x)
    xnew        = ApplicationData (adScale x) (adGen x) hgeNewBoard
handleGameEvent (EventKey (SpecialKey KeySpace) Down _ _) x = xnew where
    xnew        = rollDices x
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
-- Остатки HGE
{- Левую кнопку мыши нажали
    hgeBooleanList = map (isInPolygon hgeMouseCoord) (map chPolygon (tbCheckers (gbBar (adBoard x))))
    hgeItr = getTrueItrBooleanList hgeBooleanList 0
    hgeMoveChecker = if (hgeItr < (length hgeBooleanList)) then 
        (Just ((tbCheckers (gbBar (adBoard x))) !! hgeItr))
    else
        Nothing
    hgeNewSource = if (hgeMoveChecker == Nothing) then 
            Nothing
        else
            (Just(CheckerSource Bar Nothing))
    oldBoard = adBoard x
    oldBar = gbBar oldBoard
    oldBarCheckers = tbCheckers oldBar
    newBarCheckers = deleteListElement (hgeItr < (length hgeBooleanList)) oldBarCheckers hgeItr
    newBar = TablesBar newBarCheckers (tbPolygon oldBar)
    gameBoardNew = GameBoard (gbBoardPolygon oldBoard) (gbPoints oldBoard) newBar hgeMoveChecker hgeNewSource
    -}
{- Перемещение мыши
    oldBoard = adBoard x
    oldMoveChecker = gbChecker oldBoard
    newCheckerPolygon = if isNothing(oldMoveChecker) then
                            ([], makeColorI 255 255 255 255)
                        else
                            translatePolygon (chPolygon (fromJust(oldMoveChecker))) hgeMouseCoordinate
    
    newMoveChecker = if isNothing(oldMoveChecker) then
                        Nothing
                    else
                        Just(Checker (chPlayer (fromJust(oldMoveChecker))) newCheckerPolygon)
    gameBoardNew = GameBoard (gbBoardPolygon oldBoard) (gbPoints oldBoard) (gbBar oldBoard) newMoveChecker (gbDestination oldBoard)
    -}
{- Левую кнопку мыши отпустили
    oldBoard = adBoard x
    oldBar = gbBar oldBoard
    oldBoardPolygon = gbBoardPolygon oldBoard
    oldBoardPoints = gbPoints oldBoard
    oldCheckers = tbCheckers oldBar
    oldPolygon = tbPolygon oldBar
    oldDestination = gbDestination oldBoard
    moveChecker = gbChecker oldBoard
    newBar = if (oldDestination == (Just (CheckerSource Bar Nothing))) then 
            TablesBar (oldCheckers ++ [(fromJust(moveChecker))]) oldPolygon
        else 
            TablesBar oldCheckers oldPolygon
    (GameBoard oldBoardPolygon oldBoardPoints  newBar Nothing Nothing)
    -}