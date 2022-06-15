# error for flexsurv non-dev version

    Code
      predict(f_fit, head(lung), type = "time")
    Condition
      Error in `predict()`:
      ! This prediction type needs the development version of the `flexsurv` package.
      i You can install it with `remotes::install_github('chjackson/flexsurv-dev')`

---

    Code
      predict(f_fit, head(lung), type = "survival", time = c(0, 500, 1000))
    Condition
      Error in `predict()`:
      ! This prediction type needs the development version of the `flexsurv` package.
      i You can install it with `remotes::install_github('chjackson/flexsurv-dev')`

---

    Code
      predict(f_fit, head(lung), type = "hazard", time = c(0, 500, 1000))
    Condition
      Error in `predict()`:
      ! This prediction type needs the development version of the `flexsurv` package.
      i You can install it with `remotes::install_github('chjackson/flexsurv-dev')`

---

    Code
      predict(f_fit, head(lung), type = "linear_pred")
    Condition
      Error in `predict()`:
      ! This prediction type needs the development version of the `flexsurv` package.
      i You can install it with `remotes::install_github('chjackson/flexsurv-dev')`

---

    Code
      predict(fit_s, new_data = bladder[1:3, ], type = "quantile")
    Condition
      Error in `predict()`:
      ! This prediction type needs the development version of the `flexsurv` package.
      i You can install it with `remotes::install_github('chjackson/flexsurv-dev')`

