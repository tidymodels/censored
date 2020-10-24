# mboost helpers -----------------------------------------------------------------

#' Boosted trees via mboost
#'
#' `blackboost_train` is a wrapper for the `blackboost()` function in the
#' \pkg{mboost} package that fits tree-based models
#'  where all of the model arguments are in the main function.
#'
#' @param x A data frame or matrix of predictors.
#' @param y A factor vector with 2 or more levels
#' @param teststat a character specifying the type of the test statistic to be
#'   applied for variable selection.
#' @param testtype a character specifying how to compute the distribution of
#'   the test statistic. The first three options refer to p-values as criterion,
#'   Teststatistic uses the raw statistic as criterion. Bonferroni and
#'   Univariate relate to p-values from the asymptotic distribution (adjusted or
#'   unadjusted). Bonferroni-adjusted Monte-Carlo p-values are computed when
#'   both Bonferroni and MonteCarlo are given.
#' @param mincriterion the value of the test statistic or 1 - p-value that must
#'   be exceeded in order to implement a split.
#' @param minsplit the minimum sum of weights in a node in order to be
#'   considered for splitting.
#' @param minbucket the minimum sum of weights in a terminal node.
#' @param maxdepth maximum depth of the tree. The default maxdepth = Inf means
#'   that no restrictions are applied to tree sizes.
#' @param saveinfo logical. Store information about variable selection procedure
#'   in info slot of each partynode.
#' @param ... Other arguments to pass.
#' @return A fitted blackboost model.
#' @keywords internal
#' @export
blackboost_train <-
  function(formula, data, family, weights = NULL,
           teststat = "quad", testtype = "Teststatistic",
           mincriterion = 0, minsplit = 10,
           minbucket = 4, maxdepth = 2, saveinfo = FALSE, ...) {

    other_args <- list(...)
    protect_ct <- c("teststat", "testtype", "mincriterion", "minsplit",
                            "minbucket", "maxdepth", "saveinfo")
    protect_cb <- character()
    protect_fit <- c("formula", "data", "family", "weight")

    ct_names <- names(formals(getFromNamespace("ctree_control", "partykit")))
    cb_names <- names(formals(getFromNamespace("boost_control", "mboost")))
    fit_names <- names(formals(getFromNamespace("blackboost", "mboost")))

    other_args <- other_args[!(other_args %in%
                                 c(protect_ct, protect_cb, protect_fit))]

    ct_args <- other_args[names(other_args) %in% ct_names]
    cb_args <- other_args[names(other_args) %in% cb_names]
    fit_args <- other_args[names(other_args) %in% fit_names]

    # partykit::ctree_control
    ct_ctrl <- call2("ctree_control", .ns = "partykit")
    ct_ctrl$teststat <- teststat
    ct_ctrl$testtype <- testtype
    ct_ctrl$mincriterion <- mincriterion
    ct_ctrl$minsplit <- minsplit
    ct_ctrl$minbucket <- minbucket
    ct_ctrl$maxdepth <- maxdepth
    ct_ctrl$saveinfo <- saveinfo
    ct_ctrl <- rlang::call_modify(ct_ctrl, !!!ct_args)

    # mboost::boost_control
    cb_ctrl <- call2("boost_control", .ns = "mboost")
    cb_ctrl <- rlang::call_modify(cb_ctrl, !!!cb_args)

    # Fit
    fit_call <- call2("blackboost", .ns = "mboost")
    fit_call$formula <- expr(formula)
    fit_call$data <- expr(data)
    fit_call$family <- expr(family)
    fit_call$control <- cb_ctrl
    fit_call$tree_controls <- ct_ctrl
    if (!is.null(weights)) {
      fit_call$weights <- quote(weights)
    }
    fit_call <- rlang::call_modify(fit_call, !!!fit_args)

    eval_tidy(fit_call)
  }
