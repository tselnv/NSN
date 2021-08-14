module NSNTypes where

import qualified Data.Map.Lazy as M

type InputData' = [(Int, String, String)]

type OutputData' = [(Int, String, String)]


type DateBegin = DateYM
type DateEnd = DateYM

type Fact = M.Map DateYM VolEE

type Month = Int

type Year = Int

data DateYM = DateYM Year Month deriving (Eq, Show)

type StDate = (Station, DateYM)

type VolEE = Double -- units kWh*hour

type Pust = Double


data InputData = InputData {
    _indtStation :: Station
  , _indtDateYM  :: DateYM
  , _indtEC    :: M.Map StDate VolEE
  , _indtBalEE :: M.Map BalRowEE VolEE
--  , _indtGtpgAttributes :: M.Map StDate GtpgAttribue
  }



data GTPG = GTPG {
    _gtpgBeginDateYM :: DateYM
  , _gtpgIsDpm :: Maybe (DateBegin, DateEnd)
  } deriving (Eq, Ord, Show)


data BalRowEE = BalRowEE {
    _balRowEeGtpgs :: [GTPG] -- TODO when filling, separate by installed capacity
  } deriving (Eq, Ord,Show)


data Station = Station {
    _stGtpgs :: [GTPG]
  , _stIsGES :: Bool
  , _stFact  :: Fact
  , _stPust :: Pust
  } deriving (Eq, Ord, Show)


dateInIntervals :: DateYM -> [(DateBegin, DateEnd)] -> Bool
dateInIntervals date = any (\(beg, end) -> date >= beg && date <= end)


instance Ord DateYM where
  (DateYM y m) <= (DateYM y' m')
    | y < y' = True
    | y > y' = False
    | otherwise = m <= m'
  

balRowsEeOfStation :: Station -> [BalRowEE]
balRowsEeOfStation st = undefined
