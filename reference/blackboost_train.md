# Boosted trees via mboost

`blackboost_train()` is a wrapper for the `blackboost()` function in the
mboost package that fits tree-based models where all of the model
arguments are in the main function.

## Usage

``` r
blackboost_train(
  formula,
  data,
  family,
  weights = NULL,
  teststat = "quadratic",
  testtype = "Teststatistic",
  mincriterion = 0,
  minsplit = 10,
  minbucket = 4,
  maxdepth = 2,
  saveinfo = FALSE,
  ...
)
```

## Arguments

- formula:

  A symbolic description of the model to be fitted.

- data:

  A data frame containing the variables in the model.

- family:

  A [`mboost::Family()`](https://rdrr.io/pkg/mboost/man/Family.html)
  object.

- weights:

  An optional vector of weights to be used in the fitting process.

- teststat:

  A character specifying the type of the test statistic to be applied
  for variable selection.

- testtype:

  A character specifying how to compute the distribution of the test
  statistic. The first three options refer to p-values as criterion,
  `"Teststatistic"` uses the raw statistic as criterion. `"Bonferroni"`
  and `"Univariate"` relate to p-values from the asymptotic distribution
  (adjusted or unadjusted). Bonferroni-adjusted Monte-Carlo p-values are
  computed when both `"Bonferroni"` and `"MonteCarlo"` are given.

- mincriterion:

  The value of the test statistic or 1 - p-value that must be exceeded
  in order to implement a split.

- minsplit:

  The minimum sum of weights in a node in order to be considered for
  splitting.

- minbucket:

  The minimum sum of weights in a terminal node.

- maxdepth:

  The maximum depth of the tree. The default `maxdepth = Inf` means that
  no restrictions are applied to tree sizes.

- saveinfo:

  Logical. Store information about variable selection procedure in info
  slot of each partynode.

- ...:

  Other arguments to pass.

## Value

A fitted blackboost model.

## Examples

``` r
blackboost_train(Surv(time, status) ~ age + ph.ecog,
  data = lung[-14, ], family = mboost::CoxPH()
)
#> 
#>   Model-based Boosting
#> 
#> Call:
#> mboost::blackboost(formula = formula, data = data, family = family,     control = mboost::boost_control(), tree_controls = partykit::ctree_control(teststat = "quadratic",         testtype = "Teststatistic", mincriterion = 0, minsplit = 10,         minbucket = 4, maxdepth = 2, saveinfo = FALSE))
#> 
#> 
#>   Cox Partial Likelihood 
#> 
#> Loss function:  
#> 
#> Number of boosting iterations: mstop = 100 
#> Step size:  0.1 
#> Offset:  0 
#> Number of baselearners:  1 
#> 
```
