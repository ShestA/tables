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

-- Функция отрисовки всех изображений
drawGameApp :: ApplicationData -> Picture
drawGameApp dgaData = result where
    result = scale (fst(adScale dgaData)) (snd(adScale dgaData)) (getGamePicture (adBoard dgaData))

-- Обработчик событий
handleGameEvent :: Event -> ApplicationData -> ApplicationData
handleGameEvent (EventKey (MouseButton LeftButton) Down _ (hgeMouseX, hgeMouseY)) x = x
handleGameEvent (EventMotion (hgeMouseX, hgeMouseY)) x = x 
handleGameEvent (EventKey (MouseButton LeftButton) Up _ (hgeMouseX, hgeMouseY)) x = x
handleGameEvent (EventKey (SpecialKey KeySpace) Down _ _) x = x
handleGameEvent (EventKey (SpecialKey KeyF12) Down _ _) x = x
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