-- Поиск шашек игрока на баре
findDropedChecker :: PlayerName -> [Checker] -> Bool
findDropedChecker fdcPlayer fdcList = result where
    fdcItr = getItrFuncList fdcList func 0 where
                func :: Checker -> Bool
                func chk = (chPlayer chk == fdcPlayer)
    result = if (fdcItr == length fdcList) then True else False


-- Примагнитить шашку к движению мышки
attachMovingChecker :: (Float, Float) -> GameBoard -> GameBoard
attachMovingChecker amcMouseCoord amcOldBoard = amcNewBoard where
    amcOldPoints = gbPoints amcOldBoard
    amcOldBar = gbBar amcOldBoard
    amcBarCheckers = tbCheckers amcOldBar
    amcState = gbState amcOldBoard
    amcBarIsEmpty = findDropedChecker (currentPlayer amcState) amcBarCheckers
    
    -- Если бар не пустой
    amcInputBar = swapChecker amcMouseCoord amcBarCheckers
    amcNewBarCheckers = opcList amcInputBar
    amcNewChecker = opcElement amcInputBar
    amcNewSource = if isJust(amcNewChecker) then Just (CheckerSource Bar Nothing) else Nothing
    amcNewBar = if isJust(amcNewChecker) then
        TablesBar amcNewBarCheckers (tbPolygon amcOldBar)
    else
        amcOldBar
    amcOldBoardPolygon = (gbBoardPolygon amcOldBoard)
    amcOldDices = (gbDices amcOldBoard)
    amcNewBoard = GameBoard amcOldBoardPolygon amcOldPoints amcNewBar (amcNewChecker, amcNewSource) amcState amcOldDices
    
    