flexsurv_post <- function(pred, object) {
  if (utils::packageVersion("flexsurv") < "2.3") {
    pred <- flexsurv_rename_time(pred)
  }

  # if there's only one observation in new_data,
  # flexsurv output isn't nested
  if (!(".pred" %in% names(pred))) {
    pred <- pred %>%
      dplyr::mutate(.row = seq_len(nrow(pred))) %>%
      tidyr::nest(.by = .row) %>%
      dplyr::select(-.row)
  }
  pred
}

flexsurv_rename_time <- function(pred){
  if (".pred" %in% names(pred)) {
    pred %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        .pred = list(dplyr::rename(.pred, .eval_time = .time))
      ) %>%
      dplyr::ungroup()
  } else {
    pred %>%
      dplyr::rename(.eval_time = .time)
  }
}

# ------------------------------------------------------------------------------
# Conversion of quantile predictions to the vctrs format

# For single quantile levels, flexsurv returns a data frame with column
# ".pred_quantile" and perhaps also ".pred_lower" and ".pred_upper"

# With mutiple quantile levels, flexsurv returns a data frame with a ".pred"
# column with co.lumns ".quantile" and  ".pred_quantile" and perhaps
# ".pred_lower" and ".pred_upper"

flexsurv_to_quantile_pred <- function(x, object) {
  # if one level, convert to nested format
  if(!identical(names(x), ".pred")) {
    # convert to the same format as predictions with mulitplel levels
    x <- re_nest(x)
  }

  # Get column names to convert to vctrs encoding
  nms <- names(x$.pred[[1]])
  possible_cols <- c(".pred_quantile", ".pred_lower", ".pred_upper")
  existing_cols <- intersect(possible_cols, nms)

  # loop over prediction column names
  res <- list()
  for (col in existing_cols) {
    res[[col]] <- purrr::map_vec(x$.pred, nested_df_iter, col = col)
  }
  tibble::new_tibble(res)

}

re_nest <- function(df) {
  .row <- 1:nrow(df)
  df <- vctrs::vec_split(df, by = .row)
  df$key <- NULL
  names(df) <- ".pred"
  df
}

nested_df_iter <- function(df, col) {
  hardhat::quantile_pred(matrix(df[[col]], nrow = 1), quantile_levels = df$.quantile)
}
