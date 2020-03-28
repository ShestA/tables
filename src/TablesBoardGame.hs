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
drawGameApp :: ApplicationData -> Picture
drawGameApp _ = Blank

-- Обработчик событий
handleGameEvent :: Event -> ApplicationData -> ApplicationData
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
    let initGameState = ApplicationData (runWidthScale, runHeightScale) PlayerOneMove (GAMEBOARD) (0, 0)
    play dispMode bgColor fps initGameState drawGameApp handleGameEvent updateGameApp