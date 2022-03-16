#' Boosted trees via xgboost
#'
#' `xgb_train` is a wrapper for `xgboost` tree-based models where all of the
#'  model arguments are in the main function.
#'
#' @param x A data frame or matrix of predictors
#' @param y A vector (factor or numeric) or matrix (numeric) of outcome data.
#' @param max_depth An integer for the maximum depth of the tree.
#' @param nrounds An integer for the number of boosting iterations.
#' @param eta A numeric value between zero and one to control the learning rate.
#' @param colsample_bytree Subsampling proportion of columns for each tree.
#' See the `counts` argument below. The default uses all columns.
#' @param colsample_bynode Subsampling proportion of columns for each node
#' within each tree. See the `counts` argument below. The default uses all
#' columns.
#' @param min_child_weight A numeric value for the minimum sum of instance
#'  weights needed in a child to continue to split.
#' @param gamma A number for the minimum loss reduction required to make a
#'  further partition on a leaf node of the tree
#' @param subsample Subsampling proportion of rows. By default, all of the
#' training data are used.
#' @param validation The _proportion_ of the data that are used for performance
#' assessment and potential early stopping.
#' @param early_stop An integer or `NULL`. If not `NULL`, it is the number of
#' training iterations without improvement before stopping. If `validation` is
#' used, performance is base on the validation set; otherwise, the training set
#' is used.
#' @param counts A logical. If `FALSE`, `colsample_bynode` and
#' `colsample_bytree` are both assumed to be _proportions_ of the proportion of
#' columns affects (instead of counts).
#' @param objective A single string (or NULL) that defines the loss function that
#' `xgboost` uses to create trees. See [xgboost::xgb.train()] for options. If left
#' NULL, an appropriate loss function is chosen.
#' @param ... Other options to pass to `xgb.train`.
#' @return A fitted `xgboost` object.
#' @keywords internal
#' @export

#' @export
xgb_train_censored <- function(
    x, y,
    max_depth = 6, nrounds = 15, eta  = 0.3, colsample_bynode = NULL,
    colsample_bytree = NULL, min_child_weight = 1, gamma = 0, subsample = 1,
    validation = 0, early_stop = NULL, objective = NULL, counts = TRUE, ...) {

  others <- list(...)


  if (!is.numeric(validation) || validation < 0 || validation >= 1) {
    rlang::abort("`validation` should be on [0, 1).")
  }

  if (!is.null(early_stop)) {
    if (early_stop <= 1) {
      rlang::abort(paste0("`early_stop` should be on [2, ",  nrounds, ")."))
    } else if (early_stop >= nrounds) {
      early_stop <- nrounds - 1
      rlang::warn(paste0("`early_stop` was reduced to ", early_stop, "."))
    }
  }

  objective <- 'survival:aft'

  n <- nrow(x)
  p <- ncol(x)

  x <- as_xgb_data(x, y, validation, event_level)


  if (!is.numeric(subsample) || subsample < 0 || subsample > 1) {
    rlang::abort("`subsample` should be on [0, 1].")
  }

  # initialize
  if (is.null(colsample_bytree)) {
    colsample_bytree <- 1
  } else {
    colsample_bytree <- recalc_param(colsample_bytree, counts, p)
  }
  if (is.null(colsample_bynode)) {
    colsample_bynode <- 1
  } else {
    colsample_bynode <- recalc_param(colsample_bynode, counts, p)
  }

  if (min_child_weight > n) {
    msg <- paste0(min_child_weight, " samples were requested but there were ",
                  n, " rows in the data. ", n, " will be used.")
    rlang::warn(msg)
    min_child_weight <- min(min_child_weight, n)
  }

  arg_list <- list(
    eta = eta,
    max_depth = max_depth,
    gamma = gamma,
    colsample_bytree = colsample_bytree,
    colsample_bynode = colsample_bynode,
    min_child_weight = min(min_child_weight, n),
    subsample = subsample,
    objective = objective
  )

  main_args <- list(
    data = quote(x$data),
    watchlist = quote(x$watchlist),
    params = arg_list,
    nrounds = nrounds,
    early_stopping_rounds = early_stop
  )

  call <- make_call(fun = "xgb.train", ns = "xgboost", main_args)

  # override or add some other args

  others <-
    others[!(names(others) %in% c("data", "weights", "nrounds", names(arg_list)))]
  if (!(any(names(others) == "verbose"))) {
    others$verbose <- 0
  }
  if (length(others) > 0) {
    call <- rlang::call_modify(call, !!!others)
  }

  eval_tidy(call, env = rlang::current_env())
}

recalc_param <- function(x, counts, denom) {
  nm <- as.character(match.call()$x)
  if (is.null(x)) {
    x <- 1
  } else {
    if (counts) {
      maybe_proportion(x, nm)
      x <- min(denom, x)/denom
    }
  }
  x
}




as_xgb_data <- function(x, y, validation = 0, ...) {
  n <- nrow(x)

  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }

  if (inherits(x, "xgb.DMatrix")) {stop('what')}

  if (validation > 0) {
    m <- floor(n * (1 - validation)) + 1
    trn_index <- sample(1:n, size = max(m, 2))
    x_not_train <- x[-trn_index, ]
    y_not_train <- y[-trn_index]
    d_not_train <- xgboost::xgb.DMatrix(x_not_train,missing = NA)
    xgboost::setinfo(d_not_train,'label_lower_bound', y_not_train[,1,drop = TRUE])
    y_temp <- y_not_train
    y_temp[y_not_train[,2,drop = TRUE] == 0] <- +Inf
    xgboost::setinfo(d_not_train,'label_upper_bound', y_temp[,1,drop = TRUE])
    wlist <-
      list(validation = d_not_train)
    x_train <- x[trn_index, ]
    y_train <- y[trn_index]
    dtrain <- xgboost::xgb.DMatrix(x_train,missing = NA)
    xgboost::setinfo(dtrain,'label_lower_bound', y_train[,1,drop = TRUE])
    y_temp <- y_train
    y_temp[y_train[,2,drop = TRUE] == 0] <- +Inf
    xgboost::setinfo(d_train,'label_upper_bound', y_temp[,1,drop = TRUE])
    dat <- dtrain

  } else {
    x_train <- x
    y_train <- y
    dtrain <- xgboost::xgb.DMatrix(x_train,missing = NA)
    xgboost::setinfo(dtrain,'label_lower_bound',y_train[,1,drop = TRUE])
    y_temp <- y_train
    y_temp[y_train[,2,drop = TRUE] == 0] <- +Inf
    xgboost::setinfo(dtrain,'label_upper_bound', y_temp[,1,drop = TRUE])
    dat <- dtrain
    wlist <- list(training = dat)
  }

  list(data = dat, watchlist = wlist)
}
