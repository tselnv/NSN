module EE where


import Control.Monad.Reader
import qualified Data.Map.Lazy as M


import  NSNTypes



vcSN :: Reader InputData VolEE
vcSN = (fromIntegral . round) <$> do
  expCom <- expertCommission
  case expCom of
    Just val -> return val
    Nothing -> (+) <$> vcBalDpm <*> vcOther
                    
    

{- величина, установленная Экспертной Комиссией -}
expertCommission :: Reader InputData (Maybe VolEE)
expertCommission = do
   st <- asks _indtStation
   expCom <- asks _indtEC
   date <- asks _indtDateYM
   return $ M.lookup (st, date) expCom


{- Балансовый объём ДПМов меньше года -}
vcBalDpm :: Reader InputData VolEE
vcBalDpm = do
  station <- asks _indtStation
  sum <$> traverse dpmBalVol (balRowsEeOfStation station)

  where dpmBalVol :: BalRowEE -> Reader InputData VolEE
        dpmBalVol balRow = do
          newDpm <- isBalStringNewDpm balRow
          case newDpm of
            Right True  -> do
                           bal <- asks _indtBalEE
                           return $ maybe 0 id (M.lookup balRow bal)
            Right False -> return 0
            Left errMsg -> error errMsg -- TODO - change error to Either String



{- Проверка, что строка баланса отностися к ДПМ меньше года. Если в строке баланса есть ГТП разных типов, возвращает Left error  -} 
isBalStringNewDpm :: BalRowEE -> Reader InputData (Either String Bool)
isBalStringNewDpm balRow = do
  let allGtpgs = _balRowEeGtpgs balRow :: [GTPG]
  gtpgs <- filterM gtpgHasRight allGtpgs
  bools <- traverse isNewDpm gtpgs 
  return $ choise bools
                
  where
        choise :: [Bool] -> Either String Bool
        choise bools
          | all id bools  = Right True
          | all not bools = Right False
          | otherwise     = Left $ "ERROR EE.isBalStringNewDpm GTPG of two different types in one balance row: \
                                   \new DPM and not new DPM GTPGs in balance row " ++ show balRow
          



{- Проверка, что ГТП отностися к ДПМ со сроком эксплуатации меньше года -} 
isNewDpm :: GTPG -> Reader InputData Bool
isNewDpm gtpg = do
  date@(DateYM year mon) <- asks _indtDateYM
  let begin = _gtpgBeginDateYM gtpg
  let newGtp = (begin <= date && begin > (DateYM (year -1) mon))
  (&& newGtp) <$> isDpm gtpg


isDpm :: GTPG -> Reader InputData Bool
isDpm gtpg = do
  case _gtpgIsDpm gtpg of
    Nothing -> return False
    Just (beg, end) -> do
      date <- asks _indtDateYM
      return $ (date >= beg && date <= end)


{- Объём прочих - т.е. всех ГТП за исключением ДПМ меньше года-} 
vcOther :: Reader InputData VolEE
vcOther = asks _indtStation >>= \st ->
          vcBalOther >>=        \other ->
          vcFact >>=            \fact ->
          return $ chose fact other

  where chose :: (Maybe VolEE) -> VolEE -> VolEE
        chose Nothing other     = other
        chose (Just 0) other    = other
        chose (Just fact) other = min other fact



vcFact :: Reader InputData (Maybe VolEE)
vcFact = do
  station <- asks _indtStation
  checkstaionDpm2Y <- staionDpm2Y
  if checkstaionDpm2Y
    then vcFactYminus1
    else if (_stIsGES station)
           then vcFactGES
           else vcFact2PrevYears


-- year -1 {- Если в станцию входит хоть одна ГТПГ типа ДПМ  больше года, но меньше 2 лет -}
vcFactYminus1 :: Reader InputData (Maybe VolEE)
vcFactYminus1  = do
  station <- asks _indtStation
  (DateYM y m) <- asks _indtDateYM
  case M.lookup (DateYM (y - 1) m) (_stFact station) of
    Just vol -> return $ Just vol
    Nothing  -> error "vcFactYminus1: fact is Nothing" -- TODO change error to Either
    

-- 5 years {- если на начало расчетного месяца электростанция имеет тип «ГЭС» -}
vcFactGES :: Reader InputData (Maybe VolEE)
vcFactGES = vcAverageFact 5


-- year - 2 {- определения факта для всех прочих случаев -}
vcFact2PrevYears :: Reader InputData (Maybe VolEE)
vcFact2PrevYears = vcAverageFact 2


{- средний факт за последние nYears лет -}
vcAverageFact :: Int -> Reader InputData (Maybe VolEE)
vcAverageFact nYears = do
  station <- asks _indtStation
  date <- asks _indtDateYM
  let maybeFacts = map (getFact station) $ dates date :: [Maybe VolEE]
  let facts = fmap (\mf -> maybe 0 id mf) maybeFacts :: [VolEE]
  let (factSum, num) = foldl sumFact (0,0) facts :: (VolEE, Int)
  return $ Just $ choise (factSum, num)

  where factMap station = _stFact station :: Fact

        dates :: DateYM -> [DateYM]
        dates (DateYM year mon) = map (\y -> DateYM (year - y) mon) [1 .. 5]

        getFact :: Station -> DateYM -> Maybe VolEE
        getFact station date = M.lookup date (factMap station)

        sumFact :: (VolEE, Int) -> VolEE -> (VolEE, Int)
        sumFact (f, n) 0 = (f, n)
        sumFact (f, n) f' = ( f + f', n + 1)

        choise :: (VolEE, Int) -> VolEE
        choise (_, 0) = 0
        choise (factSum, num) = factSum / (fromIntegral num)
        
  

{- Проверка, что хоть одна ГТП станции относится к ДПМ больше года, но меньше 2 лет -} 
staionDpm2Y :: Reader InputData Bool
staionDpm2Y = do
  station <- asks _indtStation
  (any id) <$> (traverse gtpgDpm2Y $ _stGtpgs station)
 

{- Проверка, что ГТП относится к ДПМ больше года, но меньше 2 лет -} 
gtpgDpm2Y :: GTPG -> Reader InputData Bool
gtpgDpm2Y gtpg =  do
  date@(DateYM year mon) <- asks _indtDateYM
  let begin = _gtpgBeginDateYM gtpg
  let twoYear = (begin <= date && begin > (DateYM (year - 2) mon))
  (&& twoYear) <$> isDpm gtpg



{- ГТП имеет право на торговлю на ОРЭМ по состоянию на дату расчёта -}
gtpgHasRight :: GTPG -> Reader InputData Bool
gtpgHasRight gtpg = do
  dateYM <- asks _indtDateYM
  return $ _gtpgBeginDateYM gtpg <= dateYM



{- по состоянию на первое число расчетного месяца m года y
наступила дата получения субъектом оптового рынка права на участие в торговле электрической энергией по ГТП генерации
(и указанная дата наступила не позднее первого числа расчетного месяца m года y-1,
но позднее первого числа расчетного месяца m года y-2) -}
gtpgHasRightOnly1Year :: GTPG -> Reader InputData Bool
gtpgHasRightOnly1Year gtpg = do
  let gtpgDateYM = _gtpgBeginDateYM gtpg
  dateYM@(DateYM year mon) <- asks _indtDateYM
  return $ (gtpgDateYM <= (DateYM (year - 1) mon)) && (gtpgDateYM > (DateYM (year - 2) mon))
  


{- Балансовый объём прочих - т.е. всех ГТП за исключением ДПМ меньше года-} 
vcBalOther :: Reader InputData VolEE
vcBalOther = do
  station <- asks _indtStation
  sum <$> traverse otherBalVol (balRowsEeOfStation station)

  where otherBalVol :: BalRowEE -> Reader InputData VolEE
        otherBalVol balRow = do
          newDpm <- isBalStringNewDpm balRow
          case newDpm of
            Right False -> do
                           bal <- asks _indtBalEE
                           return $ maybe 0 id (M.lookup balRow bal)
            Right True  -> return 0
            Left errMsg -> error errMsg -- TODO - change error to Either String
  
  
