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
