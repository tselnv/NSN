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
                    
    

{- value set by the Expert Commission-}
expertCommission :: Reader InputData (Maybe VolEE)
expertCommission = do
   st <- asks _indtStation
   expCom <- asks _indtEC
   date <- asks _indtDateYM
   return $ M.lookup (st, date) expCom


{- Balance volume of CDC less than a year-}
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



{- Checking that the balance sheet line belongs to the CDC for less than a year. If there are DPGs of different types in the balance line, it returns Left error  -}
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
          



{- Check that the DPG belongs to the CDC with a service life of less than a year -}
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


{- The volume of all others - i.e. all DPGs except for CDCs less than a year -}
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


-- year -1 {- When the station includes at least one CDC type DPG for more than a year, but less than 2 years -}
vcFactYminus1 :: Reader InputData (Maybe VolEE)
vcFactYminus1  = do
  station <- asks _indtStation
  (DateYM y m) <- asks _indtDateYM
  case M.lookup (DateYM (y - 1) m) (_stFact station) of
    Just vol -> return $ Just vol
    Nothing  -> error "vcFactYminus1: fact is Nothing" -- TODO change error to Either
    

-- 5 years {- When at the beginning of the billing month the power plant is of the type HPP -}
vcFactGES :: Reader InputData (Maybe VolEE)
vcFactGES = vcAverageFact 5


-- year - 2 {- determining the fact for all other cases -}
vcFact2PrevYears :: Reader InputData (Maybe VolEE)
vcFact2PrevYears = vcAverageFact 2


{- average fact for the last nYears years -}
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
        
  

{- Verification that at least one station DPG belongs to CDC for more than a year, but less than 2 years -}
staionDpm2Y :: Reader InputData Bool
staionDpm2Y = do
  station <- asks _indtStation
  (any id) <$> (traverse gtpgDpm2Y $ _stGtpgs station)
 

{- Check that the gtr belongs to the CDC for more than a year, but less than 2 years -}
gtpgDpm2Y :: GTPG -> Reader InputData Bool
gtpgDpm2Y gtpg =  do
  date@(DateYM year mon) <- asks _indtDateYM
  let begin = _gtpgBeginDateYM gtpg
  let twoYear = (begin <= date && begin > (DateYM (year - 2) mon))
  (&& twoYear) <$> isDpm gtpg



{- DPG has the right to trade on the wholesale electricity market as of the date of settlement -}
gtpgHasRight :: GTPG -> Reader InputData Bool
gtpgHasRight gtpg = do
  dateYM <- asks _indtDateYM
  return $ _gtpgBeginDateYM gtpg <= dateYM



{- as of the first day of the calculated month m of the year y
the date has come for the wholesale market entity to obtain the right to participate in the trade of electrical energy through the generation DPG
(and the specified date came no later than the first day of the estimated month m of the year y-1,
but later than the first day of the calculated month m of the year y-2) -}
gtpgHasRightOnly1Year :: GTPG -> Reader InputData Bool
gtpgHasRightOnly1Year gtpg = do
  let gtpgDateYM = _gtpgBeginDateYM gtpg
  dateYM@(DateYM year mon) <- asks _indtDateYM
  return $ (gtpgDateYM <= (DateYM (year - 1) mon)) && (gtpgDateYM > (DateYM (year - 2) mon))
  


{- Balance volume of others - i.e. all DPGs except for CDC less than a year -}
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
  
  
