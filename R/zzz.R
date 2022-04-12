# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {

  make_bag_tree_rpart()

  make_boost_tree_mboost()

  make_decision_tree_rpart()
  make_decision_tree_partykit()

  make_proportional_hazards_survival()
  make_proportional_hazards_glmnet()

  make_rand_forest_partykit()

  make_survival_reg_survival()
  make_survival_reg_flexsurv()

}
