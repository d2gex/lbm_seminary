library("R6")

BiologyParameters <- R6Class("BiologyParameters", public = list(
  linf = NULL,
  k = NULL,
  t0 = NULL,
  l50 = NULL,
  l95 = NULL,
  M = NULL,
  M_K = NULL,
  lwa = NULL,
  lwb = NULL,
  rec_variability_mean = NULL,
  rec_variability_sd = NULL,
  initialize = function(linf, k, t0, l50, l95, M, M_K, lwa, lwb, rec_variability_mean, rec_variability_sd) {
    self$linf <- linf
    self$k <- k
    self$t0 <- t0
    self$l50 <- l50
    self$l95 <- l95
    self$M <- M
    self$M_K <- M_K
    self$lwa <- lwa
    self$lwb <- lwb
    self$rec_variability_mean <- rec_variability_mean
    self$rec_variability_sd <- rec_variability_sd
  }
))

ExplotationParameters <- R6Class("ExplotationParameters", public = list(
  s50 = NULL,
  s95 = NULL,
  sigmaF = NULL,
  initialize = function(s50 = NULL, s95 = NULL, sigmaF = NULL) {
    self$s50 <- s50
    self$s95 <- s95
    self$sigmaF <- sigmaF
  }
))

CatchWeightMatrices <- R6Class("CatchWeightMatrices", public = list(
  catch = NULL,
  catch_long = NULL,
  weight = NULL,
  weight_long = NULL,
  initialize = function(catch, catch_long, weight, weight_long) {
    self$catch <- catch
    self$catch_long <- catch_long
    self$weight <- weight
    self$weight_long <- weight_long
  }
))
