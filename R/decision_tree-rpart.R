#' A wrapper for survival probabilities with pecRpart models
#' @param object A fitted `_pecRpart` object.
#' @param new_data Data for prediction.
#' @param eval_time A vector of integers for prediction times.
#' @return A tibble with a list column of nested tibbles.
#' @keywords internal
#' @export
#' @examples
#' mod <- decision_tree() %>% 
#'   set_mode("censored regression") %>%
#'     set_engine("rpart") %>%
#'     fit(Surv(time, status) ~ ., data = lung)
#' survival_prob_pecRpart(mod, new_data = lung[1:3, ], eval_time = 300)
survival_prob_pecRpart <- function(object, new_data, eval_time) {
  n_obs <- nrow(new_data)
  n_eval_time <- length(eval_time) 
    
  pred <- pec::predictSurvProb(object$fit, newdata = new_data, times = eval_time)

  if (n_obs < 2) {
    pred <- matrix(pred, nrow = 1)
  }

  res <- tibble::new_tibble(
    list(
        .row = rep(seq_len(n_obs), times = n_eval_time),
        .eval_time = rep(eval_time, each = n_obs),
        .pred_survival =  as.numeric(pred)
    )
  ) %>%
    tidyr::nest(.pred = c(-.row)) %>%
    dplyr::select(-.row)

  res
}
