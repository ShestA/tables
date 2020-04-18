-- Открепить шашку
detachMovingChecker :: (Float, Float) -> GameBoard -> GameBoard
detachMovingChecker dmcCoord dmcBoard = dmcNewBoard where
    dmcBoardPolygon = gbBoardPolygon dmcBoard
    dmcOldBar = gbBar dmcBoard
    dmcOldBarCheckers = tbCheckers dmcOldBar
    dmcOldBarPolygon = tbPolygon dmcOldBar
    dmcOldPoints = gbPoints dmcBoard
    dmcMoveChecker = gbChecker dmcBoard
    dmcMoveChecker = gbChecker dmcBoard
    dmcChecker = fst(dmcMoveChecker)
    dmcSource = snd(dmcMoveChecker)
    dmcOldDices = gbDices dmcBoard
    dmcOldState = gbState dmcBoard
    dmcPlayer = currentPlayer dmcOldState
    dmcNewBoard = if (isJust dmcChecker) then
        GameBoard dmcBoardPolygon dmcNewPoints dmcNewBar (Nothing, Nothing) dmcNewState dmcNewDices
    else
        dmcBoard where
            dmcOffsets = [fst dmcOldDices, snd dmcOldDices, fst dmcOldDices + snd dmcOldDices]
            dmcStart = if (csSource (fromJust(dmcSource)) == Bar) then 
                -1 
            else
                $fromJust (csNumber (fromJust(dmcSource)))
            dmcAllTargets = if (dmcPlayer == PlayerOne) then 
                map (\x -> x + dmcStart) dmcOffsets
            else
                map (\x -> x - dmcStart) dmcOffsets
            dmcFitTargets = func dmcPlayer dmcPointsList dmcAllTargets where
                func :: PlayerName -> [TablesPoint] -> [Int] -> [Int]
                func name pts targets = result where
                    lst = map (\x -> pts !! x) targets
                    result = concat (map func1 lst) where
                        func1 :: TablesPoint -> [Int]
                        func1 pnt = itr where
                            ckrlst = tpCheckers pnt
                            enemylist = filter (\x -> (chPlayer x) /= name) ckrlst
                            itr = if (length enemylist > 1) then [] else [tpNumber pnt]
            dmcPlayerTarget = getItrFuncList dmcPointsList func 0 where
                func :: TablesPoint -> Bool
                func pts = isInPolygon dmcCoord (tpPolygon pts)
            dmcSearchFlag = elem dmcPlayerTarget dmcFitTargets
            dmcNewBar' = if dmcSearchFlag then
                dmcOldBar
            else
                if (csSource (fromJust(dmcSource)) == Bar) then
                    TablesBar (dmcOldBarCheckers ++ [fromJust(dmcChecker)]) dmcOldBarPolygon
                else
                    dmcOldBar
            dmcNewPoints' = if dmcSearchFlag then
                changePointInList dmcOldPoints dmcPlayerTarget (fromJust(dmcChecker))
            else
                if (csSource (fromJust(dmcSource)) == PointsOnBoard) then
                    changePointInList dmcOldPoints (fromJust(csNumber (fromJust(dmcSource)))) (fromJust(dmcChecker))
                else
                    dmcOldPoints
            dmcEnemyBeate = if dmcSearchFlag then
                Just $ swapChecker (\x -> chPlayer x /= dmcPlayer) (tpCheckers (dmcNewPoints' !! dmcPlayerTarget))
            else
                Nothing
            dmcInitList = map (swapChecker (\x -> False)) (map tpCheckers dmcNewPoints')
            dmcReadyList = changeListElement dmcSearchFlag dmcInitList (fromJust dmcEnemyBeate) dmcPlayerTarget
            dmcNewPoints = if dmcSearchFlag then 
                if isJust (opcElement (fromJust dmcEnemyBeate)) then
                    zipWith replaceCheckersList dmcNewPoints' dmcReadyList
                else
                    dmcNewPoints'
            else
                dmcNewPoints'
            dmcNewBar = if dmcSearchFlag then
                if isJust (opcElement dmcEnemyBeate) then
                    TablesBar (dmcOldBarCheckers ++ [fromJust (opcElement dmcEnemyBeate)]) dmcOldBarPolygon
                else
                    dmcNewBar'
            else
                dmcNewBar'
            dmcMoveInd = if dmcSearchFlag then
                findIndex (\x -> x == dmcPlayerTarget) dmcAllTargets
            else
                Nothing
            dmcNewOffsets' = changeListElement (isJust dmcMoveInd) dmcOffsets 0 (fromJust dmcMoveInd)
            dmcNewOffsets = if (dmcNewOffsets' !! 2 == 0) then [0,0] else [dmcNewOffsets' !! 0, dmcNewOffsets' !! 1]
            dmcNewDices = (dmcNewOffsets !! 0, dmcNewOffsets !! 1)
            dmcNewState = if dmcNewDices == (0,0) then 
                if dmcPlayer == PlayerOne then
                    ApplicationState PlayerTwo Roll False
                else
                    ApplicationState PlayerOne Roll False
            else
                dmcOldState
            
            
            
            
            
            
            
            
            
            
            
            
            