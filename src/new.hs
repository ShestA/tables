Выбор drop move производить при бросании кубиков. В то же время необходимо проверить доступность ходов. Если нет передать управление другому игрокуэ.
При выборе drop или move и совершении хода необходимо снова проверить доступность ходов.
Выводить на экран при старте игры сообщени:
    Пробел бросать кубик
    Энтер закончить ход
    ЛКМ перемещать шашку
    ПКМ выбросить шашку с доски
tryDeleteChecker :: GameBoard -> GameBoard
tryDeleteChecker tdcBoard = tdcNewBoard where
    tdcBoardPolygon = gbBoardPolygon tdcBoard
    tdcOldBar = gbBar tdcBoard
    tdcOldBarCheckers = tbCheckers tdcOldBar
    tdcOldBarPolygon = tbPolygon tdcOldBar
    tdcOldPoints = gbPoints tdcBoard
    tdcMoveChecker = gbChecker tdcBoard
    tdcChecker = fst(tdcMoveChecker)
    tdcSource = snd(tdcMoveChecker)
    tdcOldDices = gbDices tdcBoard
    tdcOldState = gbState tdcBoard
    tdcPlayer = currentPlayer tdcOldState
    tdcEndingFlag = theEnd tdcOldPoints tdcOldBar tdcPlayer where
        theEnd :: [TablesPoint] -> TablesBar -> PlayerName -> Bool
        theEnd tePoints teBar teName = teResult where
            teFilteredPoints = if teName == PlayerOne then
                filter (\x -> (tpNumber x) > 17) foo
            else
                filter (\x -> (tpNumber x) < 6) foo where
                    foo = filter (\x -> length (filter (\y -> (chPlayer y) /= teName) (tpCheckers x)) == 0) tePoints
                    teFilteredBar = filter (\x -> (chPlayer x) /= teName) (tbCheckers teBar)
                    teResult = ((length teFilteredPoints) == 0) && ((length teFilteredBar) == 0)
    tdcNewBoard = if tdcEndingFlag then
        if (isJust tdcChecker) then 
            if (csSource (fromJust(tdcSource)) == PointOnBoard) then
                tdNewBoard'
            else 
                tdcBoard
        else
            tdcBoard
    else
       tdcBoard where
            -- Можно заканчивать игру...проверяем кубики
            tdcTargets = if tdcPlayer == PlayerOne then
                (24 - fst tdcOldDices, 24 - snd tdcOldDices)
            else
                (fst tdcOldDices - 1, snd tdcOldDices - 1)
            fl1 = (fst tdcTargets) == fromJust (csNumber (fromJust(tdcSource)))
            fl2 = (snd tdcTargets) == fromJust (csNumber (fromJust(tdcSource)))
            fl3 = fl1 || fl2
            -- Если условия соблюлись, двигающаяся шашка пропадёт
            -- Пересчитываем оставшиеся кубики и меняем состояние, если надо
            tdcNewDices = if fl3 then
                if fl1 then 
                    (0, snd tdcOldDices)
                else
                    (fst tdcOldDices, 0)
            else
                tdcOldDices
            tdcNewMoveChecker = if fl3 then
                (Nothing, Nothing)
            else
                tdcMoveChecker
            tdcNewTargets = if tdcPlayer == PlayerOne then
                (24 - fst tdcNewDices, 24 - snd tdcNewDices)
            else
                (fst tdcNewDices - 1, snd tdcNewDices - 1)
            ffl1 = if (fst tdcNewDices > 0) then
                (length (filter (\x -> chPlayer x == tdcPlayer) (tpCheckers (tdcOldPoints !! (fst tdcNewTargets))))) == 0
            else
                True
            ffl2 = if (snd tdcNewDices > 0) then
                (length (filter (\x -> chPlayer x == tdcPlayer) (tpCheckers (tdcOldPoints !! (snd tdcNewTargets))))) == 0
            else
                True
            tdcNewState = if (tdcNewDices == (0,0)) || (ffl1 && ffl2) then
                if tdcPlayer == PlayerOne then
                    ApplicationState PlayerTwo Roll False
                else
                    ApplicationState PlayerOne Roll False
            else
                tdcOldState
            -- Пересоздаем доску
            tdNewBoard' = GameBoard tdcBoardPolygon tdcOldPoints tdcOldBar tdcNewMoveChecker tdcNewState tdcNewDices