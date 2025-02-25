
module Widgets.Charts.TimeToAccumulate where

import Prelude (($), (*), (/), (+), (-), (>), (<=), (<), (==), map, pure, bind, Unit)
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
  , yAxis :: Array {"name" :: String, "type" :: String, max :: String, "position":: String}
  , legend :: {}
  , series :: Array { name :: String, type :: String, yAxisIndex :: Int, "data" :: Array (Array Number) }
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
  [ reportKey "timetobuy-chart"
  ]
  [ HH.slot _echarts "time-to-buy"
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

    y1EvaluationPoints :: Number -> Array Number
    y1EvaluationPoints target =
      map f yzValues
        where
          yearly {income,takenAt,takenBefore} = income - takenAt - takenBefore
          f yz
             | yearly yz <= 0.0 = 500.0
             | true             = target / yearly yz

    y2EvaluationPoints :: Array Number
    y2EvaluationPoints =
      map f yzValues
        where
          yearly {income,takenAt,takenBefore} = income - takenAt - takenBefore
          f yz
             | yearly yz <= 0.0 = 0.0
             | true             = 45.0 * yearly yz


    series :: Array { name :: String, yAxisIndex :: Int, type :: String, "data" :: Array (Array Number) }
    series =
      [ { name: "years to max-bracket starter"
        , "type": "line"
        , "data": Array.zipWith (\x y -> [x,y]) xEvaluationPoints (y1EvaluationPoints $ Maybe.fromMaybe 0.0 (Marginal.maxbracketTakeHome evaluatedBrackets))
        , "yAxisIndex": 0
        }
      , { name: "lifetime gains (45y)"
        , "type": "line"
        , "data": Array.zipWith (\x y -> [x,y]) xEvaluationPoints y2EvaluationPoints
        , "yAxisIndex": 1
        }
      ]

    echartOpts =
      { xAxis: {"type": "value"}
      , yAxis:
         [ {"name": "years to earn", "type": "value", "position":"left", "max": "150"}
         , {"name": "lifetime gains", "type": "value", "position":"right", "max": "dataMax"}
         ]
      , legend: {}
      , tooltip: {trigger: "axis"}
      , series
      }

    handleEchartsClick :: Actions action -> ECharts.Output Unit -> action
    handleEchartsClick actions {key: unit} = actions.noop "no action"

