source("lbm_data_holder.R")

bio_params <- BiologyParameters$new(
  linf = 48.4,
  k = 0.18,
  t0 = -0.58,
  l50 = 22.5,
  l95 = 23.115,
  M = 0.3105,
  M_K = 1.725,
  lwa = 0.021,
  lwb = 2.955
)

exp_params <- ExplotationParameters$new()
