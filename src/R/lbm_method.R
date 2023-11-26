library("R6")

Lbm <- R6Class("Lbm", public = list(

  biology_params = NULL,
  explotation_params = NULL,
  catchweight_data = NULL,
  initialize = function(biology_params, explotation_params, catchweight_data) {
    self$biology_params <- biology_params
    self$explotation_params <- explotation_params
    self$catchweight_data <- catchweight_data
  }
))