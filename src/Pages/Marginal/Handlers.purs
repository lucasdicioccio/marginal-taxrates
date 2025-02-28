
module Pages.Marginal.Handlers where

-------------------------------------------------------------------------------
import Prelude ((==),(/=),($),Unit,bind,discard,map,pure,unit)
import Effect.Class (liftEffect)
import Data.Number as Number
import Data.Foldable (traverse_)
import Data.Traversable (for_)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Array as Array
import Halogen as H
import Effect.Aff.Class (class MonadAff)
import Minitools.Seqnum as Seqnum
import Minitools.Tracer (Tracer, trace)

-------------------------------------------------------------------------------
import Pages.Marginal.Action
import Pages.Marginal.State
import Pages.Marginal.Trace

-------------------------------------------------------------------------------

handleAction
  :: forall m slots output. (MonadAff m)
  => Tracer Trace
  -> Action
  -> H.HalogenM State Action slots output m Unit
handleAction tracer action =
  case action of
    Initialize ->
      pure unit
    Noop _ ->
      pure unit
    SetPresetTaxBracket _ taxBrackets -> do
      H.modify_ (\st0 -> st0 { entities { taxBrackets = taxBrackets } })
    AddTaxBracket mstarting mrate event -> do
      liftEffect $ traverse_ cancelEventPropagation event
      for_ mstarting $ \starting -> do
        for_ mrate $ \rate -> do
          seqnum <- Seqnum.allocate
          let newbracket = {seqnum, starting, rate}
          let insertBracket st0 = Array.cons newbracket st0.entities.taxBrackets
          H.modify_ (\st0 -> st0 { entities { taxBrackets = insertBracket st0 } })
    DeleteTaxBracket seqnum -> do
      let filterBracket st0 = Array.filter (\x -> x.seqnum /= seqnum) st0.entities.taxBrackets
      H.modify_ (\st0 -> st0 { entities { taxBrackets = filterBracket st0 } })
    SetTaxBracketStartString str -> do
      let start = Number.fromString str
      H.modify_ (\st0 -> st0 { ui { newTaxBracketStartString = str, newTaxBracketStart = start } })
    SetTaxBracketRateString str -> do
      let rate = Number.fromString str
      H.modify_ (\st0 -> st0 { ui { newTaxBracketRateString = str, newTaxBracketRate = rate } })
    TaxBracketProgressBarClicked seqnum event -> do
      mfrac <- liftEffect $ computeTaxRateFraction event
      for_ mfrac $ \frac -> do
        let modifyTaxBracket taxBracket
              | taxBracket.seqnum == seqnum = taxBracket { rate = frac }
              | true                        = taxBracket
        let modifyTaxBrackets st0 =
              map (modifyTaxBracket) st0.entities.taxBrackets
        H.modify_ (\st0 -> st0 { entities { taxBrackets = modifyTaxBrackets st0 }})

    HourlyWageAction (SetHourlyWageString1 str) -> do
      let nhours st0 = Maybe.fromMaybe st0.config.hourlyWage.yearlyHours1 $ Number.fromString str
      H.modify_ (\st0 -> st0 { ui { hourlyWage { yearlyHours1String = str } }, config { hourlyWage { yearlyHours1 = nhours st0 } } })

    HourlyWageAction (SetHourlyWageString2 str) -> do
      let nhours st0 = Maybe.fromMaybe st0.config.hourlyWage.yearlyHours2 $ Number.fromString str
      H.modify_ (\st0 -> st0 { ui { hourlyWage { yearlyHours2String = str } }, config { hourlyWage { yearlyHours2 = nhours st0 } } })
