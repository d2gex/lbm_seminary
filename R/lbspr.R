library("R6")
library("LBSPR")
source("R/utils.R")
source("R/lbm_method.R")

Lbspr <- R6Class("Lbspr", inherit = Lbm, public = list(

  initialize = function(biology_params, explotation_params, catch_weight_data) {
    super$initialize(biology_params, explotation_params, catch_weight_data)
  },
  run = function() {

    lc_matrix <- df_to_unname_matrix(self$catchweight_data$catch, from_col = 2)
    lc_matrix_t <- transpose(self$catchweight_data$catch)
    years <- as.integer(rownames(lc_matrix_t))
    mid_points <- as.numeric(colnames(lc_matrix_t))


    # --> Provide algorithm with expected parameters
    input_params <- new("LB_pars")
    input_params@Linf <- self$biology_params$linf
    input_params@L50 <- self$biology_params$l50
    input_params@L95 <- self$biology_params$l95
    input_params@MK <- self$biology_params$M_K
    input_params@L_units <- "cm"
    input_params@Walpha <- self$biology_params$lwa
    input_params@Wbeta <- self$biology_params$lwb
    input_params@BinWidth <- 1

    # --> Provide algorithms with expected details about length and weight compsositions
    LB_lengths <- new("LB_lengths")
    LB_lengths@LMids <- mid_points
    LB_lengths@LData <- lc_matrix
    LB_lengths@Years <- years
    LB_lengths@NYears <- length(years)

    # --> Run the algorithm
    result <- LBSPRfit(LB_pars = input_params, LB_lengths = LB_lengths, verbose = TRUE)
    return(list(
      years = result@Years,
      estimates = as.data.frame(result@Ests)
    ))
  }
))