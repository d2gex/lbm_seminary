library("R6")
source("lbm_method.R")

Lbspr <- R6Class("Lbspr", inherit = Lbm, public = list(

  initialize = function(biology_params, explotation_params, catchweight_data) {
    super$initialize(biology_params, explotation_params, catchweight_data)
  }
))