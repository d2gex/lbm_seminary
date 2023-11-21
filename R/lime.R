library("R6")
library("LIME")
source("utils.R")
source("lbm_method.R")

Lime <- R6Class("Lime", inherit = Lbm, public = list(

  initialize = function(biology_params, explotation_params, catch_weight_data) {
    super$initialize(biology_params, explotation_params, catch_weight_data)
  },
  run = function() {

    lc_matrix_t <- transpose(self$catchweight_data$catch)
    years <- as.integer(rownames(lc_matrix_t))

    lh <- create_lh_list(
      linf = self$biology_params$linf,
      vbk = self$biology_params$k,
      t0 = self$biology_params$t0,
      lwa = self$biology_params$lwa,
      lwb = self$biology_params$lwb,
      M50 = self$biology_params$l50,
      M = self$biology_params$M,
      S50 = self$explotation_params$s50,
      S95 = self$explotation_params$s95,
      SigmaR = self$biology_params$rec_variability_sd,
      SigmaF = self$explotation_params$sigmaF,
      binwidth = 1,
      selex_input = "length",
      selex_type = c("logistic"),
      maturity_input = "length",
      nseasons = 1,
      nfleets = 1)

    data_inputs <- list(years = years, LF = LFreq_df(lc_matrix_t))
    data_avail <- "LC"
    lfh_inputs <- create_inputs(lh = lh, input_data = data_inputs)
    result <- run_LIME(modpath = NULL,
                       input = lfh_inputs,
                       SigRprior = c(self$biology_params$rec_variability_mean,
                                     self$biology_params$rec_variability_sd),
                       derive_quants = TRUE,
                       data_avail = data_avail)

    output <- data.frame(
      SL50 = result$Report$S50_f,
      SL95 = result$Report$S95_f,
      SPR = result$Report$SPR_t,
      F = result$Report$F_t,
      R = result$Report$R,
      max_gradient = result$opt$max_gradient,
      hessian = result$Sdreport$pdHess,
      convergence = result$opt$max_gradient <= 0.001 & result$Sdreport$pdHess == TRUE
    )
    return(list(
      years = years,
      estimates = output
    ))
  }
))



