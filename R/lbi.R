library("R6")
source("utils.R")
source("lbm_method.R")
source("lbi_library.R")

Lbi <- R6Class("Lbi", inherit = Lbm, public = list(

  initialize = function(biology_params, explotation_params, catch_weight_data) {
    super$initialize(biology_params, explotation_params, catch_weight_data)
  },
  run = function() {

    results <- lb_ind(data = self$catchweight_data$catch,
                      binwidth = 1,
                      linf = self$biology_params$linf,
                      lmat = self$biology_params$l50,
                      mk_ratio = self$biology_params$M_K,
                      weight = self$catchweight_data$weight)

    output <- data.frame(
      Lc_Lmat = results$Lc_Lmat,
      L25_Lmat = results$L25_Lmat,
      Lmax5_Linf = results$Lmax5_Linf,
      Pmega = results$Pmega,
      Lmean_Lopt = results$Lmean_Lopt,
      Lmean_Lfem = results$Lmean_LFeM
    )

    return(list(
      years = results$Year,
      estimates = output
    ))
  }
))



