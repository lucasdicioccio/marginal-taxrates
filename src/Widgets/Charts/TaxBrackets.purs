
module Widgets.Charts.TaxBrackets where

import Prelude ((==),map,Unit)
import Data.Array as Array
import Data.Ord as Ord
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
  [ reportKey "taxbrackets-chart"
  ]
  [ HH.slot _echarts "tax-bracket"
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
    taxBrackets :: Array TaxBracket
    taxBrackets = Array.sortBy (Ord.comparing _.starting) chart.taxBrackets

    hasZero :: Boolean
    hasZero = Array.any (\x -> x.starting == 0.0) taxBrackets

    xEvaluationPoints :: Array Number
    xEvaluationPoints
     | hasZero = map _.starting taxBrackets
     | true    = Array.cons 0.0 (map _.starting taxBrackets)

    yEvaluationPoints :: Array Number
    yEvaluationPoints
     | hasZero = map _.rate taxBrackets
     | true    = Array.cons 0.0 (map _.rate taxBrackets)

    series :: Array { name :: String, type :: String, "data" :: Array (Array Number) }
    series =
      [ {name:"tax bracket","type":"line","data": Array.zipWith (\x y -> [x,y]) xEvaluationPoints yEvaluationPoints }
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
