module Main where

-------------------------------------------------------------------------------
import Prelude
import Data.Argonaut.Core as Json
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..),fromMaybe)
import Data.Tuple as Tuple
import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Effect (Effect)
import Foreign.Object as Object
import Halogen.Aff (awaitBody, selectElement, runHalogenAff)
import Halogen as H
import Halogen.VDom.Driver (runUI)
import Minitools.PageEvents as PageEvents
import Minitools.Reports as Reports
import Minitools.Tracer as Minitools
import Unsafe.Coerce as Unsafe
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML (window)
import Web.HTML.Window (Window)

-------------------------------------------------------------------------------
import Pages.Marginal as Marginal
import Pages.Marginal.Action as Marginal
import Pages.Marginal.Trace as Marginal
-------------------------------------------------------------------------------
type Params =
  { recording :: Reports.Recording
  , pageEvents :: PageEvents.Config
  }

component
  :: forall query m. MonadAff m
  => Params
  -> (Reports.AnonymousPush -> Effect Unit)
  -> H.Component query Marginal.PageInput Void m
component params pushReport =
    Marginal.page {tracer}
  where
    tracer :: Marginal.Trace -> Effect Unit
    tracer = Minitools.traceBoth traceConsole traceReports

    traceConsole :: Marginal.Trace -> Effect Unit
    traceConsole item = 
      Console.log (show item)

    traceReports :: Marginal.Trace -> Effect Unit
    traceReports (Marginal.TraceAction _ ev) = 
      for_ (mappedEvent ev) $ \e ->
        pushReport {e}
          where
            mappedEvent =
              case _ of
                (Marginal.Initialize) ->
                   Just
                   $ object
                     [ kvStr "key" "initialize"
                     , kvStr "app" "marginal"
                     ]
                (Marginal.Noop str) ->
                   Just
                   $ object
                     [ kvStr "key" "noop"
                     , kvStr "reason" str
                     ]
                (Marginal.SetPresetTaxBracket name _) ->
                   Just
                   $ object
                     [ kvStr "key" "apply-preset"
                     , kvStr "preset" name
                     ]
                (Marginal.AddTaxBracket _ _ _) ->
                   Just $ key "add-tax-bracket"
                (Marginal.DeleteTaxBracket _) ->
                   Just $ key "rm-tax-bracket"
                (Marginal.SetTaxBracketStartString _) ->
                   Nothing
                (Marginal.SetTaxBracketRateString _) ->
                   Nothing
                (Marginal.TaxBracketProgressBarClicked _ _) ->
                   Just $ key "click-tax-rate"
                (Marginal.HourlyWageAction _) ->
                   Just
                   $ object
                     [ kvStr "key" "hourly-wage-action"
                     ]

    keyValStr key str =
      Json.jsonSingletonObject key (Json.fromString str)
    kvStr key str =
      Tuple.Tuple key (Json.fromString str)
    key str =
      keyValStr "key" str
    object = Json.fromObject <<< Object.fromFoldable

main :: Effect Unit
main = do
  w <- window
  params <- initWithDefaultParams
  doRun w params
  where
    doRun :: Window -> Params -> Effect Unit
    doRun w params = do
      -- initialize analytics
      pusher <- Reports.init params.recording
      PageEvents.tapElement
        (pusher <<< mappedAnnotation)
        params.pageEvents
        (Unsafe.unsafeCoerce w)

      -- run whole component
      runHalogenAff do
        body <- awaitBody
        spaElem <- selectElement (QuerySelector "#spa")
        let tgt = fromMaybe body spaElem
        runUI (component params pusher) {} tgt

    initWithDefaultParams :: Effect Params
    initWithDefaultParams = do
      refreshString <- Reports.generateRecordingRefresh
      let recording = { baseUrl: "https://dicioccio.fr/api/blog_reports/blog_reports" , refreshString}
      pure {recording, pageEvents: PageEvents.defaultConfig}


-------------------------------------------------------------------------------
mappedAnnotation :: PageEvents.Annotation -> Reports.AnonymousPush
mappedAnnotation ann = {e}
  where
    e :: Json.Json
    e = Json.fromObject
        $ Object.fromFoldable
        [ kvStr "key" "dom-event"
        , kvStr "elem:id" ann.elementID
        , kvStr "ev:t" ann.eventType
        , Tuple.Tuple "attrs:list" (Json.fromArray $ map Json.fromString attrs)
        , Tuple.Tuple "kvs" (Json.fromArray $ map (Json.fromObject <<< Object.fromFoldable <<< map kvTuple) kvs)
        ]

    attrs = map _.k ann.attributes
    kvs = map _.kvs ann.attributes

    kvStr key str =
      Tuple.Tuple key (Json.fromString str)
    kvTuple (Tuple.Tuple key str) = kvStr key str
