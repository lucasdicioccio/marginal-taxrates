
module Pages.Marginal.State where

-------------------------------------------------------------------------------
import Data.Maybe (Maybe)
import Minitools.Seqnum (Seqnum)

-------------------------------------------------------------------------------

-- | amount of income
type Income = Number

-- | how much of an income is taxed
type TaxRate = Number

-- | typical tax-bracket, defined as a half-segment
type TaxBracket =
  { seqnum :: Seqnum "bracket"
  , starting :: Income
  , rate :: TaxRate
  }

type Entities =
  { taxBrackets :: Array TaxBracket
  }

type UIState =
  { newTaxBracketStartString :: String
  , newTaxBracketRateString :: String
  , newTaxBracketStart :: Maybe Number
  , newTaxBracketRate :: Maybe Number
  , hourlyWage ::
    { yearlyHours1String :: String
    , yearlyHours2String :: String
    }
  }

type Config =
  { hourlyWage ::
    { yearlyHours1 :: Number
    , yearlyHours2 :: Number
    }
  }

type State =
  { seqnum :: Seqnum "state"
  , entities :: Entities
  , config :: Config
  , ui :: UIState
  }
