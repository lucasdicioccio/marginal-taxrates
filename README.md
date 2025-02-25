# A tool to help people grasp how marginal tax rates behave

See it [live on my blog](https://dicioccio.fr/marginal-taxrate.html) .

**The aim of this tool is to facilitate a constructive discussion about tax bracket policies, primarily focusing on income tax.**

Income tax is an important funding means for governments.  In numerous
countries, income tax is directly tied to earnings; the higher one's income,
the greater the absolute and proportional contribution they make.  Governments

Despite their straightforward nature, tax brackets often lead to
misunderstandings. The calculations involved introduce numerous variables,
making it challenging for most people to accurately determine the tax rate at
various income levels. Which in turns prevent people from empathizing with
others' financial situations. Even more so, discussions about policy changes,
such as those during elections, become prone to unchecked, incorrect
assertions. The goal of this tool is to improve on this unfortunate situation.

Henceforth, primary goals for this tool are to:
- help people grasp the notion of marginal tax rate and effective tax rate
- help people relate their own income situation to other income situations
- help people simulating what happens when changing the income-tax policy changes

## build

provided you have a recent PureScript + Spago installation

```console
npm i
spago bundle
```

the HTML source will need a `<div id="spa"></div>` element

recently built with

```
purs 0.15.15
spago 0.93.29
```

## rough idea/todo list

- i18n
- some "native" comparison mode to compare different policies
- possibility to add some cost-of-living/benefits offsets
- possibility to weight the number of "family shares" (e.g., in France a family of two parents +one kid will divide their shared income by 2.5 to find the brackets)
- extract and tests computations from charts
- more lists of tax brackets from different countries/years
- prettier input forms
- add a conversion into hourly-income given a number of worked-hours per week
- some indifference-curves at various (or user-tunable) incomes and worked-hours/off-hours per week
