
module Pages.Marginal.Computations where

--------------------------------------------------------------------------------
import Prelude (($), (*), (/), (+), (-), (>), (<), (==), map, bind, pure, Unit)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Foldable as Foldable
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Ord as Ord

--------------------------------------------------------------------------------
import Pages.Marginal.State (TaxBracket)

type Value =
  { income :: Number
  , takenAt :: Number
  , takenBefore :: Number
  }

type EvaluatedBrackets =
  { sortedBrackets :: Array TaxBracket
  , maxBracket :: Maybe TaxBracket
  , analyzeBrackets :: Number -> { atIncome :: Maybe TaxBracket, slices::Array (Tuple TaxBracket TaxBracket) }
  , yzValue :: Number -> Value
  }

evaluateBrackets :: Array TaxBracket -> EvaluatedBrackets
evaluateBrackets inputBrackets =
    {sortedBrackets,maxBracket,analyzeBrackets, yzValue}
  where
    sortedBrackets :: Array TaxBracket
    sortedBrackets = Array.sortBy (Ord.comparing _.starting) inputBrackets

    maxBracket :: Maybe TaxBracket
    maxBracket = Array.last sortedBrackets

    bracketsForIncome :: Number -> Array TaxBracket
    bracketsForIncome income =
      Array.filter (\bracket -> bracket.starting < income) sortedBrackets

    analyzeBrackets :: Number -> { atIncome :: Maybe TaxBracket, slices::Array (Tuple TaxBracket TaxBracket) }
    analyzeBrackets income =
      let
        brackets = bracketsForIncome income
      in
      { atIncome: Array.last brackets
      , slices: Array.zip brackets (Array.drop 1 brackets)
      }

    yzValue :: Number -> Value
    yzValue income =
      let
        brackets = analyzeBrackets income
        rate = Maybe.maybe 0.0 _.rate brackets.atIncome
        start = Maybe.maybe 0.0 _.starting brackets.atIncome
        takenAt = rate * (income - start) / 100.0
        takenInSlice (Tuple s0 s1) = s0.rate * (s1.starting - s0.starting) / 100.0
        takenBefore = Foldable.sum $ map takenInSlice brackets.slices
      in
      {income, takenAt, takenBefore}

maxbracketTakeHome :: EvaluatedBrackets -> Maybe Number
maxbracketTakeHome evaluatedBrackets = do
  maxBracket <- evaluatedBrackets.maxBracket
  let {income,takenAt,takenBefore} = evaluatedBrackets.yzValue  maxBracket.starting
  pure $ income - takenAt - takenBefore

