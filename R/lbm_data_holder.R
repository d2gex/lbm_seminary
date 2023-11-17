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
  initialize = function(linf, k, t0, l50, l95, M, M_K, lwa, lwb) {
    self$linf <- linf
    self$k <- k
    self$t0 <- t0
    self$l50 <- l50
    self$l95 <- l95
    self$M <- M
    self$M_K <- M_K
    self$lwa <- lwa
    self$lwb <- lwb
  }
))

ExplotationParameters <- R6Class("ExplotationParameters", public = list(
  s50 = NULL,
  s95 = NULL,
  initialize = function(s50 = NULL, s95 = NULL) {
    self$s50 <- s50
    self$s95 <- s95
  }
))

CatchWeightMatrices <- R6Class("CatchWeightMatrices", public = list(
  catch = NULL,
  weight = NULL,
  initialize = function(catch, weight) {
    self$catch <- catch
    self$weight <- weight
  }
))
