
module Widgets.Charts.TaxedAmounts where

import Prelude (($), (*), (/), (+), (-), (>), (<), (==), map, Unit)
import Data.Array as Array
import Data.Tuple (Tuple(..))
import Data.Ord as Ord
import Data.Foldable as Foldable
import Data.Traversable as Traversable
import Data.Unfoldable as Unfoldable
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Foreign (unsafeFromForeign, unsafeToForeign) as Foreign
import Halogen.EChartsForeign as ECharts
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Data.Symbol (class IsSymbol)
import Effect.Aff.Class (class MonadAff)
import Prim.Row (class Cons)
import Type.Proxy (Proxy)
--------------------------------------------------------------------------------
import Pages.Marginal.State (TaxBracket)
import Utils (reportKey)
import Pages.Marginal.Computations (evaluateBrackets)
import Pages.Marginal.Computations as Marginal

type Slot =
  H.Slot (ECharts.Query) (ECharts.Output Unit) String

type ChartSpec =
  { taxBrackets :: Array TaxBracket
  }

type Actions a =
  { noop :: String -> a
  }

type PreparedGraph action =
  { echartOpts :: EChartsOpts
  , handler :: Actions action -> ECharts.Output Unit -> action
  }

type EChartsOpts =
  { tooltip :: { trigger :: String }
  , xAxis :: {"type" :: String}
  , yAxis :: {"type" :: String}
  , legend :: {}
  , series :: Array { name :: String, type :: String, "data" :: Array (Array Number) }
  }

type Props a =
  { chart :: ChartSpec
  , actions :: Actions a
  }

render ::
  forall action state s output m label _1 slots.
  MonadAff m =>
  IsSymbol label =>
  Cons label Slot _1 slots =>
  Proxy label ->
  Props action ->
  H.ComponentHTML action slots m
render _echarts props =
  HH.div
  [ reportKey "takedamount-chart"
  ]
  [ HH.slot _echarts "taxed-amount"
    ( ECharts.component
      { style: ECharts.style640x480
      , updateBehaviour: ECharts.ReplaceOnUpdate
      , onEvents: []
      }
    )
    { options: Foreign.unsafeToForeign echartOpts, modified: true }
    (handler props.actions)
  ]
  where
    {echartOpts, handler} = prepare props.chart

prepare :: forall action. 
  ChartSpec ->
  PreparedGraph action
prepare chart =
    { echartOpts, handler: handleEchartsClick }
  where
    evaluatedBrackets = evaluateBrackets chart.taxBrackets

    xEvaluationPoints :: Array Number
    xEvaluationPoints =
      let
        maxStart = Maybe.maybe 100.0 _.starting evaluatedBrackets.maxBracket
        effectiveEnd = maxStart * 1.25
        minStart = 0.0
        nSteps = 100.0
        stepSize = (effectiveEnd - minStart) / nSteps
        effectiveStepSize = Ord.max stepSize 1.0
        step v
          | v > effectiveEnd = Tuple v Nothing
          | true = Tuple v (Just (v + effectiveStepSize))
      in
      Unfoldable.unfoldr1 step minStart

    yzValues :: Array Marginal.Value
    yzValues = map evaluatedBrackets.yzValue xEvaluationPoints

    y1EvaluationPoints :: Array Number
    y1EvaluationPoints =
      map _.takenAt yzValues

    y2EvaluationPoints :: Array Number
    y2EvaluationPoints =
      map _.takenBefore yzValues

    y3EvaluationPoints :: Array Number
    y3EvaluationPoints =
      map (\{takenAt,takenBefore} -> takenAt+takenBefore) yzValues

    series :: Array { name :: String, type :: String, "data" :: Array (Array Number) }
    series =
      [ { name:"taxed amount within bracket"
        , "type": "line"
        , "data": Array.zipWith (\x y -> [x,y]) xEvaluationPoints y1EvaluationPoints
        }
      , { name:"cumulative amount in previous brackets"
        , "type": "line"
        , "data": Array.zipWith (\x y -> [x,y]) xEvaluationPoints y2EvaluationPoints
        }
      , { name:"cumulative amount"
        , "type": "line"
        , "data": Array.zipWith (\x y -> [x,y]) xEvaluationPoints y3EvaluationPoints
        }
      ]

    echartOpts =
      { xAxis: {"type": "value"}
      , yAxis: {"type": "value"}
      , legend: {}
      , tooltip: {trigger: "axis"}
      , series
      }

    handleEchartsClick :: Actions action -> ECharts.Output Unit -> action
    handleEchartsClick actions {key: unit} = actions.noop "no action"
