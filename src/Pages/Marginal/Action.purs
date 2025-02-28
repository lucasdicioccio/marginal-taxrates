
module Pages.Marginal.Action where

-------------------------------------------------------------------------------
import Prelude ((-),($),(*),(/),(=<<),class Show, Unit, bind, pure)
import Effect (Effect)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (for)
import Data.Int as Int
import Minitools.Seqnum (Seqnum)
import Web.Event.Internal.Types as Web
import Web.Event.Event as Web
import Web.UIEvent.MouseEvent as Mouse
import Web.HTML.HTMLElement as WebHtml

-------------------------------------------------------------------------------
import Pages.Marginal.State

-------------------------------------------------------------------------------
-- a newtype wrapper with a Show instance
newtype WebEvent = WebEvent Web.Event
instance showWebEvent :: Show WebEvent where
  show _ = "WebEvent"

cancelEventPropagation :: WebEvent -> Effect Unit
cancelEventPropagation (WebEvent ev) = Web.preventDefault ev

newtype MouseEvent = MouseEvent Mouse.MouseEvent
instance showMouseEvent :: Show MouseEvent where
  show _ = "MouseEvent"

computeTaxRateFraction :: MouseEvent -> Effect (Maybe Number)
computeTaxRateFraction (MouseEvent ev) = do
  let px = Int.toNumber $ Mouse.pageX ev
  let melt = WebHtml.fromEventTarget =<< Web.target (Mouse.toEvent ev)
  for melt \elt -> do
    ol <- WebHtml.offsetLeft elt
    let x = px - ol
    ow <- WebHtml.offsetWidth elt
    pure $ 100.0 * (x / ow)


data HourlyWageAction
  = SetHourlyWageString1 String
  | SetHourlyWageString2 String
derive instance genericHourlyWageAction :: Generic HourlyWageAction _
instance showHourlyWageAction :: Show HourlyWageAction where
  show = genericShow

data Action
  = Initialize
  | Noop String
  | SetTaxBracketStartString String
  | SetTaxBracketRateString String
  | HourlyWageAction HourlyWageAction
  | AddTaxBracket (Maybe Number) (Maybe Number) (Maybe WebEvent)
  | DeleteTaxBracket (Seqnum "bracket")
  | TaxBracketProgressBarClicked (Seqnum "bracket") MouseEvent
  | SetPresetTaxBracket String (Array TaxBracket)
derive instance genericAction :: Generic Action _
instance showAction :: Show Action where
  show = genericShow
