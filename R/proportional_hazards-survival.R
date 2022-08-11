#' @export
predict_linear_pred._coxph <- function(object,
                                       new_data,
                                       ...,
                                       increasing = TRUE) {
  res <- NextMethod()
  if (increasing) {
    # For consistency with other models, we want the lp to increase with
    # time. For this, we change the sign
    res <- -res
  }
  res
}
