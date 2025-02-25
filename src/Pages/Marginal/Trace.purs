
module Pages.Marginal.Trace where

-------------------------------------------------------------------------------
import Prelude (class Show)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Minitools.Seqnum as Seqnum
-------------------------------------------------------------------------------
import Pages.Marginal.Action

-------------------------------------------------------------------------------
data Trace
  = TraceAction (Seqnum.Seqnum "action") Action
derive instance genericTrace :: Generic Trace _
instance showTrace :: Show Trace where
  show = genericShow
