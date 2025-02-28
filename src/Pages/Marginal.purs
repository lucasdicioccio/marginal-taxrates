
module Pages.Marginal where

-------------------------------------------------------------------------------
import Prelude ((-),(*),($),(<>),(==),(<<<),(||),bind,const,discard,map,show)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.List as List
import Data.Monoid (mempty)
import DOM.HTML.Indexed.InputType as DOM
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Effect.Aff.Class (class MonadAff)
import Minitools.Tracer (Tracer, trace)
import Minitools.Seqnum (Seqnum)
import Minitools.Seqnum as Seqnum
import Minitools.Bricks.ActionButton as ActionButton
import Minitools.Bricks.Emojis as Emojis
import Minitools.Bricks.Message as Message
import Minitools.Bricks.SetSearchSelector as SetSearchSelector
import Minitools.Bricks.Table as Table

-------------------------------------------------------------------------------
import Pages.Marginal.Action (Action(..),HourlyWageAction(..),WebEvent(..),MouseEvent(..))
import Pages.Marginal.Trace (Trace(..))
import Pages.Marginal.State (State,TaxBracket)
import Pages.Marginal.Handlers as Handlers
import Pages.Marginal.Slots (Slots, _charts)
import Pages.Marginal.Computations as Computations
import Utils (reportKey)
import Widgets.Charts.HourlyRate as HourlyRateChart
import Widgets.Charts.TaxBrackets as TaxBracketsChart
import Widgets.Charts.TaxedAmounts as TaxedAmountsChart
import Widgets.Charts.TakeHome as TakeHomeChart
import Widgets.Charts.TimeToAccumulate as TimeToAccumulateChart
import Widgets.Charts.MarginalUtility as MarginalUtilityChart


frenchTaxBrackets2024 :: Array TaxBracket
frenchTaxBrackets2024 =
  [ { seqnum: Seqnum.Seqnum 10, starting: 0.0, rate: 0.0 }
  , { seqnum: Seqnum.Seqnum 11, starting: 11498.0, rate: 11.0 }
  , { seqnum: Seqnum.Seqnum 12, starting: 29316.0, rate: 30.0 }
  , { seqnum: Seqnum.Seqnum 13, starting: 83824.0, rate: 41.0 }
  , { seqnum: Seqnum.Seqnum 14, starting: 180294.0, rate: 45.0 }
  ]

frenchNfpBrackets2024 :: Array TaxBracket
frenchNfpBrackets2024 =
  [ { seqnum: Seqnum.Seqnum 10, starting: 0.0, rate: 1.0 }
  , { seqnum: Seqnum.Seqnum 11, starting: 10292.0, rate: 5.0 }
  , { seqnum: Seqnum.Seqnum 12, starting: 15438.0, rate: 10.0 }
  , { seqnum: Seqnum.Seqnum 13, starting: 20584.0, rate: 15.0 }
  , { seqnum: Seqnum.Seqnum 14, starting: 27789.0, rate: 20.0 }
  , { seqnum: Seqnum.Seqnum 15, starting: 30876.0, rate: 25.0 }
  , { seqnum: Seqnum.Seqnum 16, starting: 33964.0, rate: 30.0 }
  , { seqnum: Seqnum.Seqnum 17, starting: 38081.0, rate: 35.0 }
  , { seqnum: Seqnum.Seqnum 18, starting: 44256.0, rate: 40.0 }
  , { seqnum: Seqnum.Seqnum 19, starting: 61752.0, rate: 45.0 }
  , { seqnum: Seqnum.Seqnum 20, starting: 102921.0, rate: 50.0 }
  , { seqnum: Seqnum.Seqnum 21, starting: 144089.0, rate: 55.0 }
  , { seqnum: Seqnum.Seqnum 22, starting: 267594.0, rate: 60.0 }
  , { seqnum: Seqnum.Seqnum 23, starting: 411693.0, rate: 90.0 }
  ]


--------------------------------------------------------------------------------
state0 :: State
state0 =
  { seqnum: Seqnum.Seqnum 100
  , entities:
    { taxBrackets: frenchTaxBrackets2024
    }
  , ui:
    { newTaxBracketStart: Just 1.0
    , newTaxBracketRate: Just 5.0
    , newTaxBracketStartString: "1"
    , newTaxBracketRateString: "5"
    , hourlyWage:
      { yearlyHours1String: "1607"
      , yearlyHours2String: "1760"
      }
    }
  , config:
    { hourlyWage:
      { yearlyHours1: 1607.0
      , yearlyHours2: 1760.0
      }
    }
  }

type PageInput = { }

type Props =
  { tracer :: Tracer Trace
  }

page
  :: forall query output m. MonadAff m
  => Props
  -> H.Component query PageInput output m
page { tracer } =
    H.mkComponent
      { initialState: \_ -> state0
      , render
      , eval: H.mkEval $ H.defaultEval
          { handleAction = \act -> do
              seqnum <- Seqnum.allocate
              H.liftEffect (trace tracer $ TraceAction seqnum act)
              Handlers.handleAction tracer act
          , initialize = Just Initialize
          }
      }
  where
    render :: State -> H.ComponentHTML Action Slots m
    render state =
      HH.div_
      [ HH.div
        [ HP.class_ (HH.ClassName "section")
        , reportKey "laius"
        ]
        [ render_laius state
        ]
      , HH.div
        [ HP.class_ (HH.ClassName "section")
        , reportKey "app"
        ]
        [ HH.div
          [ HP.class_ (HH.ClassName "columns")
          ]
          [ HH.div
            [ HP.classes [ HH.ClassName "column", HH.ClassName "is-one-third" ]
            ]
            [ render_taxBrackets state
            ]
          , HH.div
            [ HP.class_ (HH.ClassName "column")
            ]
            [ render_simulations state
            ]
          ]
        ]
      ]

    render_laius state =
      HH.div
      [ HP.class_ (HH.ClassName "content")
      ]
      [ HH.h1_
        [ HH.text "income tax bracket explainator"
        ]
      , HH.p_
        [ HH.text "In France, among taxes we have an income tax. This income tax likely is the most misunderstood tax due to the bracketed-mechanism. As a person gets more income, they enter increasing tax brackets with larger income tax rates."
        ]
      , HH.p_
        [ HH.text "Some people believe that if you earn one more euro and change tax bracket then you suddenly are worse off. This is a commonly held misunderstanding. Indeed, the tax rates are marginal: only the one extra euro is subject to the increased tax rate."
        ]
      , HH.p_
        [ HH.text "This "
        , HH.em_ [ HH.text "explainator" ]
        , HH.text " serves a number of goals. "
        , HH.text "First, I'd like to illustrate the effect of the tax brackets. "
        , HH.text "Second, when a political party proposes new tax brackets you are out of tools to get a better grasp of the effect of the policy on your personal case. This simulator allows you to tweak values. "
        , HH.text "Finally, I want to illustrate the effect that marginal tax brackets may have on individuals' motivation. Indeed an extra work hour is not gonna move the needle for a person with already high income: hours worked in high tax brackets bring less money but cost the same amount of time. If these high-value work hours are forfeited, they become a net loss for the society."
        ]
      , HH.p_
        [ HH.text "This explainator is limited in scope as there is more to life than a single income tax (especially in France). In the future I plan to add a bit more features for the simulator to be immediately useful to more people (e.g., accounting for the number of 'family shares') and to enable better comparisons (e.g., adding a cost-of-living/benefits offsets). "
        , HH.text "Another venue for improvement would be to add population statistics with income distributions and an indifference curve to visualize the somewhat-debated Laffer effect."
        ]
      , HH.p_
        [ HH.text "If you have comments or feature requests, please "
        , HH.a [ HP.href "/about-me.html" ] [ HH.text "get in touch" ]
        , HH.text "."
        ]
      ]

    render_taxBrackets state =
      HH.div
      [ HP.class_ (HH.ClassName "content")
      , reportKey "policy editor"
      ]
      [ HH.h2_
        [ HH.text "tax brackets policy"
        ]
      , HH.p_
        [ HH.text "compose your tax brackets policy. "
        , HH.text "(initialized with current French tax brackets in 1000-Euros)"
        ]
      , render_taxBrackets_form state
      , render_taxBrackets_quickSetup state
      , render_taxBrackets_list state
      ]

    render_taxBrackets_form state =
      let
         invalidStart = Maybe.isNothing state.ui.newTaxBracketStart
         invalidRate = Maybe.isNothing state.ui.newTaxBracketRate
         invalidValues = invalidStart || invalidRate
      in
      HH.form
      [ reportKey "new-bracket-form"
      , HP.class_ (HH.ClassName "control")
      , HE.onSubmit (AddTaxBracket state.ui.newTaxBracketStart state.ui.newTaxBracketRate <<< Just <<< WebEvent)
      ]
      [ HH.div
        [ HP.class_ (HH.ClassName "field")
        ]
        [ HH.label
          [ HP.class_ (HH.ClassName "label")
          ]
          [ HH.text "starting value (in your currency)"
          ]
        , HH.input
          [ HP.class_ (HH.ClassName "input")
          , HP.type_ DOM.InputNumber
          , HE.onValueChange SetTaxBracketStartString
          , HP.value state.ui.newTaxBracketStartString
          ]
        ]
      , HH.div
        [ HP.class_ (HH.ClassName "field")
        ]
        [ HH.label
          [ HP.class_ (HH.ClassName "label")
          ]
          [ HH.text "bracket tax rate (in %)"
          ]
        , HH.input
          [ HP.class_ (HH.ClassName "input")
          , HP.type_ DOM.InputNumber
          , HE.onValueChange SetTaxBracketRateString
          , HP.value state.ui.newTaxBracketRateString
          ]
        ]
      , HH.div
        [ HP.class_ (HH.ClassName "field")
        ]
        [ HH.input
          [ HP.classes [ HH.ClassName "button", HH.ClassName "is-primary" ]
          , HP.type_ DOM.InputSubmit
          , HP.value "add"
          , HP.disabled invalidValues
          ]
        ]
      ]

    render_taxBrackets_quickSetup state =
      HH.div
      [ HP.class_ (HH.ClassName "control")
      ]
      [ HH.label
        [ HP.class_ (HH.ClassName "label")
        ]
        [ HH.text "quick fill from some preset"
        ]
      , HH.div
        [ HP.classes [ HH.ClassName "buttons" ]
        ]
        [ ActionButton.render
          { text: "French-2024"
          , disabled: false
          , action: SetPresetTaxBracket "france-2024" frenchTaxBrackets2024
          , info: "France in 2024"
          }
        , ActionButton.render
          { text: "NFP-2024"
          , disabled: false
          , action: SetPresetTaxBracket "nfp-2024" frenchNfpBrackets2024
          , info: "proposition of Nouveau Front Populaire 2024"
          }
        ]
      ]

    render_taxBrackets_list state =
      HH.div
      [ HP.class_ (HH.ClassName "section")
      ]
      [ HH.h4_ [ HH.text "current brackets" ]
      , HH.ul_
        $ map (render_taxBrackets_list_item state) state.entities.taxBrackets
      ]

    render_taxBrackets_list_item state taxBracket =
      HH.li
      [ reportKey "new-bracket-item"
      ]
      [ HH.div
        [ HP.class_ (HH.ClassName "columns")
        ]
        [ HH.div
          [ HP.class_ (HH.ClassName "column")
          ]
          [ HH.text "from: "
          , HH.text $ show taxBracket.starting
          ]
        , HH.div
          [ HP.class_ (HH.ClassName "column")
          ]
          [ HH.text "tax: "
          , HH.text $ show taxBracket.rate
          , HH.text "%"
          ]
        , HH.div
          [ HP.class_ (HH.ClassName "column")
          ]
          [ HH.progress
            [ HP.value taxBracket.rate
            , HP.max 100.0
            , HE.onClick (TaxBracketProgressBarClicked taxBracket.seqnum <<< MouseEvent)
            ]
            [
            ]
          ]
        , HH.div
          [ HP.class_ (HH.ClassName "column")
          ]
          [ ActionButton.render
            { text: Emojis.trashBin
            , action: DeleteTaxBracket taxBracket.seqnum
            , info: "remove this tax bracket"
            , disabled: false
            }
          ]
        ]
      ]

    render_simulations state =
      let
        evaluatedBrackets = Computations.evaluateBrackets state.entities.taxBrackets
      in
      HH.div
      [ HP.class_ (HH.ClassName "content")
      , reportKey "simulations"
      ]
      [ HH.h2_
        [ HH.text "graphs and simulations"
        ]
      , HH.p_
        [ HH.text "some simulations regarding the taxed amount, take home, and effective rates"
        ]
      , HH.h3_
        [ HH.text "tax brackets"
        ]
      , HH.p_
        [ HH.text "Visualizes tax brackets as defined in the configuration."
        ]
      , HH.div
        [ reportKey "tax-brackets-chart"
        ]
        [ TaxBracketsChart.render
          _charts
          { actions:
            { noop: Noop
            }
          , chart:
            { taxBrackets: state.entities.taxBrackets
            }
          }
        ]
      , HH.h3_
        [ HH.text "taxed amount"
        ]
      , HH.p_
        [ HH.text "Visualizes taxed quantity versus the income (how much the state gains)."
        ]
      , HH.p_
        [ HH.text "We show three curves as functions of income:"
        , HH.br_
        , HH.text "- the taxed amount within a bracket (i.e., the amount due because of the marginal rate)"
        , HH.br_
        , HH.text "- the cumulative amount from previous brackets"
        , HH.br_
        , HH.text "- the cumulative amount (i.e., the tax due)"
        ]
      , HH.p_
        [ HH.text "Note as the income grows and enters a bracket, the cumulative amount from previous brackets meet the cumulative amount, whereas the marginal amount grows at a (generally) steeper rate."
        ]
      , HH.div
        [ reportKey "taxed-amounts-chart"
        ]
        [ TaxedAmountsChart.render
          _charts
          { actions:
            { noop: Noop
            }
          , chart:
            { taxBrackets: state.entities.taxBrackets
            }
          }
        ]
      , HH.h3_
        [ HH.text "take home amount"
        ]
      , HH.p_
        [ HH.text "Visualizes taken home money (how much an individual gets after taxes)."
        ]
      , HH.p_
        [ HH.text "Also visualizes the effective tax rate vs. income."
        ]
      , HH.p_
        [ HH.text "We show four curves as functions of income:"
        , HH.br_
        , HH.text "- y=x, that is, the income before tax (left axis)"
        , HH.br_
        , HH.text "- the taken-home amount, which is the income minus the cumulative amount of the previous chart (left axis)"
        , HH.br_
        , HH.text "- the effective tax rate as if you had a single tax-rate based on your income (right axis, in percent)"
        , HH.br_
        , HH.text "- the take home percent, which is 100% - effective tax rate in percent (right axis)"
        ]
      , HH.div
        [ reportKey "take-home-chart"
        ]
        [ TakeHomeChart.render
          _charts
          { actions:
            { noop: Noop
            }
          , chart:
            { taxBrackets: state.entities.taxBrackets
            }
          }
        ]
      , HH.h3_
        [ HH.text "time to earn some total amount vs. income"
        ]
      , HH.p_
        [ HH.text "Given the yearly take-home from the previous chart, we can then ask the question: how long does it take to take-home enough to become a (fictional as other expenses are not yet taken into account) millionaire or such questions."
        ]
      , HH.p_
        [ HH.text "Helps to grasp the importance of a large expense is compared to a lifetime earning. "
        , HH.text "This works under the assumption that nothing changes across a lifetime, however I think the simulation is relevant to understand how life changes with larger incomes."
        ]
      , HH.p_
        [ HH.text "Of note: the 'years to max-bracket starter' gives the amount of time to make the same cumulated income as someone who just enters the richest tax bracket."
        , HH.text " With current settings, this value is a take-home of "
        , HH.strong_
          [ HH.text $ Maybe.maybe "(unspecified)" show $ map Int.round $ Computations.maxbracketTakeHome evaluatedBrackets
          ]
        , HH.text " for a person with income of "
        , HH.strong_
          [ HH.text $ Maybe.maybe "(unspecified)" show $ map (\b -> Int.round b.starting) $ evaluatedBrackets.maxBracket
          ]
        , HH.text ". "
        , HH.text "For instance, a value of 2.3 means that it takes you two years and four months, you earn as much as this person in one year."
        ]
      , HH.p_
        [ HH.text "We cap the left-axis at 150years for legibility and because it makes little sense to count years past a whole life."
        ]
      , HH.div
        [ reportKey "time-to-accumulate-chart"
        ]
        [ TimeToAccumulateChart.render
          _charts
          { actions:
            { noop: Noop
            }
          , chart:
            { taxBrackets: state.entities.taxBrackets
            }
          }
        ]
      , HH.h3_
        [ HH.text "marginal daily wage"
        ]
      , HH.p_
        [ HH.text "We can play a game: trying to capture the perceived value of the each working day. "
        , HH.text "Let's say you are able to modulate the amount of work you make every week at a same hourly/daily wage. "
        , HH.text "For instance, doctors who take as income some amount proportional of the number of patients they take. "
        , HH.text "Assuming a standard week is five days of work and you can pick how many days you want to work."
        ]
      , HH.p_
        [ HH.text "An extra assumption is that the perceived value is defined as how much you would earn if you were to work one extra day a week across the year. "
        , HH.text "Thus the first day corresponds to the share of your income filling lower tax brackets. "
        , HH.text "Then compare that to the perceived value of the last day of work, which is how much you would lose if you were to work four days a week across the year."
        , HH.text "If tax rates were flat, these values would be equal to one fifth (20%)."
        ]
      , HH.p_
        [ HH.text "With typical tax brackets of increasing tax rates, we can see that the perceived value of the first and last days of work are not commensurate. As workers enter new tax brackets, the marginal value of the last worked day drops (as the increasing rate of the effective tax rate is steeper right after entering a tax bracket). "
        ]
      , HH.div
        [ reportKey "marginal-utility-chart"
        ]
        [ MarginalUtilityChart.render
          _charts
          { actions:
            { noop: Noop
            }
          , chart:
            { taxBrackets: state.entities.taxBrackets
            }
          }
        ]
      , HH.h3_
        [ HH.text "hourly rate taken home comparisons"
        ]
      , HH.p_
        [ HH.text "Another recurring theme of talks about wages and the fairness (or lack of) fairness in income tax is that income tax does not capture well the effective wage for people working more than a 'legal' work week. We can observe the effect (or lack of effect) by comparing effective take home amounts normalized by the amount of work hours."
        ]
      , HH.p_
        [ HH.text "The following chart displays the effective hourly wage post tax for two work load (that you can adjust). By default, we compare 35hours/week during ~46 weeks (1607 hours) to 40hours/week during 220days (1760 hours), which are typical French arrangements for blue and white collar jobs."
        ]
      , HH.p_
        [ HH.text "This comparison is probably only useful around the first few tax brackets as only a handful of jobs may have income in the larger brackets only from work. Thus, consider deleting some of the large brackets to get a better data resolution around the lower brackets."
        ]
      , HH.p_
        [ HH.text "This simulation can be useful to gauge if a job promotion with higher wage but with an increased workload is really worth it (besides non-quantifiable aspects if you plan on making stepping stones in a career)."
        ]
      , HH.div
        [ HP.classes [ HH.ClassName "control", HH.ClassName "columns" ]
        , reportKey "hourly-wage-tweaks"
        ]
        [ HH.div
          [ HP.classes [ HH.ClassName "field", HH.ClassName "column" ]
          ]
          [ HH.label
            [ HP.class_ (HH.ClassName "label")
            ]
            [ HH.text "yearly worked hours for Group-1"
            ]
          , HH.input
            [ HP.class_ (HH.ClassName "input")
            , HP.type_ DOM.InputNumber
            , HE.onValueChange (HourlyWageAction <<< SetHourlyWageString1)
            , HP.value state.ui.hourlyWage.yearlyHours1String
            ]
          ]
        , HH.div
          [ HP.classes [ HH.ClassName "field", HH.ClassName "column" ]
          ]
          [ HH.label
            [ HP.class_ (HH.ClassName "label")
            ]
            [ HH.text "yearly worked hours for Group-2"
            ]
          , HH.input
            [ HP.class_ (HH.ClassName "input")
            , HP.type_ DOM.InputNumber
            , HE.onValueChange (HourlyWageAction <<< SetHourlyWageString2)
            , HP.value state.ui.hourlyWage.yearlyHours2String
            ]
          ]
        ]
      , HH.p_
        [ HH.text "The chart displays three curves: the two hourly-rate taken home post income tax for Group-1 and Group-2 (left axis). We also chart the"
        , HH.text " "
        , HH.em_ [ HH.text "situation equivalence" ]
        , HH.text " (right axis)"
        , HH.text ", which we define as the income at which point the group working fewer hours makes the same hourly wage taken home as the group working more hours. In another terms, this third chart reads the 'horizontal gap' at a given fixed Y-value."
        ]
      , HH.div
        [ reportKey "hourly-wage-chart"
        ]
        [ HourlyRateChart.render
          _charts
          { actions:
            { noop: Noop
            }
          , chart:
            { taxBrackets: state.entities.taxBrackets
            , yearlyHours1: state.config.hourlyWage.yearlyHours1
            , yearlyHours2: state.config.hourlyWage.yearlyHours2
            }
          }
        ]
      ]
