#' We'll model the survival of lung cancer patients.

#+ results = "hide", messages = FALSE
library(tidymodels)
library(censored)
tidymodels_prefer()

data(cancer)

lung <- lung %>% drop_na()
lung_train <- lung[-c(1:5), ]
lung_test <- lung[1:5, ]
