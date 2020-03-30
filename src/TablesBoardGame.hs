{-# LANGUAGE RecordWildCards #-}
module TablesBoardGame( run
    ) where
    
-- Подключаемые модули
import System.IO
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment
import Data.Maybe

import AbstractData
import Constants
import AdditionalFunction

-- ******************************************************************
-- Описание основных функций программы

-- Функция отрисовки всех изображений
drawGameApp :: ApplicationData -> Picture
drawGameApp dgaData = result where
    result = scale (fst(adScale dgaData)) (snd(adScale dgaData)) (getGamePicture (adBoard dgaData))

-- Обработчик событий
handleGameEvent :: Event -> ApplicationData -> ApplicationData
handleGameEvent (EventKey (MouseButton LeftButton) Down _ hgeMouseCoordinate) x = xnew where
    hgeBooleanList = map (isInPolygon hgeMouseCoordinate) (map chPolygon (tbCheckers (gbBar (adBoard x))))
    hgeItr = getTrueItrBooleanList hgeBooleanList 0
    hgeMoveChecker = if (hgeItr < (length hgeBooleanList)) then 
        (Just ((tbCheckers (gbBar (adBoard x))) !! hgeItr))
    else
        Nothing
    oldBoard = adBoard x
    oldBar = gbBar oldBoard
    oldBarCheckers = tbCheckers oldBar
    newBarCheckers = deleteListElement (hgeItr < (length hgeBooleanList)) oldBarCheckers hgeItr
    newBar = TablesBar newBarCheckers (tbPolygon oldBar)
    gameBoardNew = GameBoard (gbBoardPolygon oldBoard) (gbPoints oldBoard) newBar hgeMoveChecker
    xnew = ApplicationData (adScale x) (adState x) gameBoardNew (adDices x)

handleGameEvent (EventMotion hgeMouseCoordinate) x = xnew where
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
    gameBoardNew = GameBoard (gbBoardPolygon oldBoard) (gbPoints oldBoard) (gbBar oldBoard) newMoveChecker
    xnew = ApplicationData (adScale x) (adState x) gameBoardNew (adDices x)
    
handleGameEvent (EventKey (MouseButton LeftButton) Up _ (hgeMouseX, hgeMouseY)) x = x
handleGameEvent (EventKey (SpecialKey KeySpace) Down _ _) x = x
handleGameEvent (EventKey (SpecialKey KeyF12) Down _ _) x = xnew where
    xnew = ApplicationData (adScale x) PlayerOneMove baseGameBoard (0, 0)
handleGameEvent _ x = x

-- Обработчик кадра
updateGameApp :: Float -> ApplicationData -> ApplicationData
updateGameApp _ x = x

-- ******************************************************************
-- Описание основной функции программы
run :: IO ()
run = do
    runScreenSize <- getScreenSize
    let runScreenWidth = fromIntegral (fst runScreenSize)
    let runScreenHeight = fromIntegral (snd runScreenSize)
    let (runWidthScale, runHeightScale) = getScreenScale (runScreenWidth, runScreenHeight)
    let initGameState = ApplicationData (runWidthScale, runHeightScale) PlayerOneMove baseGameBoard (0, 0)
    play dispMode bgColor fps initGameState drawGameApp handleGameEvent updateGameApp