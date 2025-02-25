
module Pages.Marginal.Slots where

import Prelude (Unit)
import Halogen as H
import Halogen.EChartsForeign as ECharts
import Type.Proxy (Proxy(..))

type Slots =
  ( charts :: H.Slot (ECharts.Query) (ECharts.Output Unit) String
  )

_charts = Proxy :: Proxy "charts"
