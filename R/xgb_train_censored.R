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
xgb_train_censored <- function(x, y,
                               max_depth = 6, nrounds = 15, eta = 0.3, colsample_bynode = NULL,
                               colsample_bytree = NULL, min_child_weight = 1, gamma = 0, subsample = 1,
                               validation = 0, early_stop = NULL, objective = NULL, counts = TRUE, weights = NULL, ...) {
  others <- list(...)


  if (!is.numeric(validation) || validation < 0 || validation >= 1) {
    rlang::abort("`validation` should be on [0, 1).")
  }

  if (!is.null(early_stop)) {
    if (early_stop <= 1) {
      rlang::abort(paste0("`early_stop` should be on [2, ", nrounds, ")."))
    } else if (early_stop >= nrounds) {
      early_stop <- nrounds - 1
      rlang::warn(paste0("`early_stop` was reduced to ", early_stop, "."))
    }
  }

  n <- nrow(x)
  p <- ncol(x)

  x <-
    as_xgb_data(x, y,
                validation = validation,
                event_level = event_level,
                weights = weights,
                objective = objective)


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
    msg <- paste0(
      min_child_weight, " samples were requested but there were ",
      n, " rows in the data. ", n, " will be used."
    )
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
      x <- min(denom, x) / denom
    }
  }
  x
}

as_xgb_data <- function(x, y, validation = 0, weights = NULL, event_level = "first",objective, ...) {
  lvls <- levels(y)
  n <- nrow(x)

  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }

  if (is.factor(y)) {
    if (length(lvls) < 3) {
      if (event_level == "first") {
        y <- -as.numeric(y) + 2
      } else {
        y <- as.numeric(y) - 1
      }
    } else {
      if (event_level == "second") rlang::warn("`event_level` can only be set for binary variables.")
      y <- as.numeric(y) - 1
    }
  }

  if (objective == "survival:aft") {
    if (validation > 0) {

      # Split data
      m <- floor(n * (1 - validation)) + 1
      trn_index <- sample(1:n, size = max(m, 2))

      # Train and validation
      x_val <- x[-trn_index, ]
      y_val <- y[-trn_index]
      x_train <- x[trn_index, ]
      y_train <- y[trn_index]

      # Validation prep

      val_data <- xgboost::xgb.DMatrix(x_val, missing = NA)

      xgboost::setinfo(val_data, "label_lower_bound", y_val[, 1, drop = TRUE])
      y_val[y_val[, 2, drop = TRUE] == 0] <- +Inf
      xgboost::setinfo(val_data, "label_upper_bound", y_val[, 1, drop = TRUE])

      watch_list <- list(validation = val_data)




      # Training prep



      train_data <- xgboost::xgb.DMatrix(x_train, missing = NA)

      if (!is.null(weights)) {
        xgboost::setinfo(train_data, "weight",weights)
      }

      xgboost::setinfo(train_data, "label_lower_bound", y_train[, 1, drop = TRUE])
      y_train[y_train[, 2, drop = TRUE] == 0] <- +Inf
      xgboost::setinfo(train_data, "label_upper_bound", y_train[, 1, drop = TRUE])

      # Save training data for return

      dat <- train_data


    } else {

      # Regular prep

      x_train <- x
      y_train <- y

      train_data <- xgboost::xgb.DMatrix(x_train, missing = NA)

      if (!is.null(weights)) {
        xgboost::setinfo(train_data, "weight",weights)
      }

      xgboost::setinfo(train_data, "label_lower_bound", y_train[, 1, drop = TRUE])
      y_train[y_train[, 2, drop = TRUE] == 0] <- +Inf
      xgboost::setinfo(train_data, "label_upper_bound", y_train[, 1, drop = TRUE])

      # Create training data and watch list for return

      dat <- train_data
      watch_list <- list(training = train_data)

    }

    return(list(data = dat, watchlist = watch_list))

  } else if (objective == "survival:cox") {
    if (validation > 0) {
      m <- floor(n * (1 - validation)) + 1
      trn_index <- sample(1:n, size = max(m, 2))

      # Train and validation
      x_val <- x[-trn_index, ]
      y_val <- y[-trn_index]
      x_train <- x[trn_index, ]
      y_train <- y[trn_index]

      # Validation prep


      val_data <- xgboost::xgb.DMatrix(x_val, missing = NA)

      y_val[y_val[, 2, drop = TRUE] == 0] <- y_val[y_val[, 2, drop = TRUE] == 0][,1] * -1
      xgboost::setinfo(val_data, "label", y_temp[, 1, drop = TRUE])
      watch_list <- list(validation = val_data)

      # Training prep

      info_list <- list(label = y_train)
      if (!is.null(weights)) {
        info_list$weight <- weights[trn_index]
      }

      train_data <- xgboost::xgb.DMatrix(x_train, missing = NA,info = info_list)

      y_train[y_train[, 2, drop = TRUE] == 0] <- y_train[y_train[, 2, drop = TRUE] == 0][,1] * -1
      xgboost::setinfo(train_data, "label", y_temp[, 1, drop = TRUE])

      # Save training data for return
      dat <- train_data

    } else {

      # Regular prep

      x_train <- x
      y_train <- y

      info_list <- list(label = y_train)
      if (!is.null(weights)) {
        info_list$weight <- weights[trn_index]
      }

      train_data <- xgboost::xgb.DMatrix(x_train, missing = NA,info = info_list)

      y_train[y_train[, 2, drop = TRUE] == 0] <- y_train[y_train[, 2, drop = TRUE] == 0][,1] * -1
      xgboost::setinfo(train_data, "label", y_train[, 1, drop = TRUE])

      # Create training data and watch list for return
      dat <- train_data
      watch_list <- list(training = train_data)
    }
    return(list(data = dat, watchlist = watch_list))
  } else {
    if (!inherits(x, "xgb.DMatrix")) {
      if (validation > 0) {
        # Split data
        m <- floor(n * (1 - validation)) + 1
        trn_index <- sample(1:n, size = max(m, 2))
        val_data <- xgboost::xgb.DMatrix(x[-trn_index,], label = y[-trn_index], missing = NA)
        watch_list <- list(validation = val_data)

        info_list <- list(label = y[trn_index])
        if (!is.null(weights)) {
          info_list$weight <- weights[trn_index]
        }
        dat <- xgboost::xgb.DMatrix(x[trn_index,], missing = NA, info = info_list)


      } else {
        info_list <- list(label = y)
        if (!is.null(weights)) {
          info_list$weight <- weights
        }
        dat <- xgboost::xgb.DMatrix(x, missing = NA, info = info_list)
        watch_list <- list(training = dat)
      }
    } else {
      dat <- xgboost::setinfo(x, "label", y)
      if (!is.null(weights)) {
        dat <- xgboost::setinfo(x, "weight", weights)
      }
      watch_list <- list(training = dat)
    }
    return(list(data = dat, watchlist = watch_list))
  }
}
