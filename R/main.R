source("R/config.R")
source("R/utils.R")
source("R/plot_utils.R")
source("R/lbm_data_holder.R")
source("R/lbspr.R")
source("R/lime.R")
source("R/lbi.R")
source("R/algo_plotter.R")

diplodus_data <- read_sheets_from_excel(FREQUENCY_DATA_PATH)
catch_weight_data <- CatchWeightMatrices$new(catch = diplodus_data$catch,
                                             catch_long = diplodus_data$catch_long,
                                             weight = diplodus_data$weight,
                                             weight_long = diplodus_data$weight_long)
bio_params <- BiologyParameters$new(
  linf = 48.4,
  k = 0.18,
  t0 = -0.58,
  l50 = 22.5,
  l95 = 23.115,
  M = 0.3105,
  M_K = 1.725,
  lwa = 0.021,
  lwb = 2.955,
  rec_variability_mean = 0.737, # Prior recruitment variability obtained from meta-analysis study
  rec_variability_sd = 0.353 # Prior fishing penalisation obtained from meta-analysis study
)

exp_params <- ExplotationParameters$new()
lbspr_algo <- Lbspr$new(bio_params, exp_params, catch_weight_data)
lbspr_results <- lbspr_algo$run()

exp_params$s50 <- mean(lbspr_results$estimates$SL50)
exp_params$s95 <- mean(lbspr_results$estimates$SL95)
exp_params$sigmaF <- 0.2
lime_algo <- Lime$new(bio_params, exp_params, catch_weight_data)
lime_results <- lime_algo$run()


lbi_algo <- Lbi$new(bio_params, exp_params, catch_weight_data)
lbi_results <- lbi_algo$run()


data <- lbspr_results$estimates
data$years <- lbspr_results$years
lbspr_plotter <- LbsprOutputPlotter$new(data, bio_params$M)
spr_g <- lbspr_plotter$build_spr_plot(d_colour = "steelblue")
fm_g <- lbspr_plotter$build_fm_plot(d_colour = "steelblue")
grid <- lbspr_plotter$build_parallell_plots(list(spr_g, fm_g), "SPR and F/M estimates for LBSPR", size = 15, just = 'centre')
grid

data <- lime_results$estimates
data$years <- lime_results$years
lime_plotter <- LimeOutputPlotter$new(data, bio_params$M)
spr_g <- lime_plotter$build_spr_plot(d_colour = "steelblue")
f_g <- lime_plotter$build_score(d_colour = "steelblue", 'F')
r_g <- lime_plotter$build_score(d_colour = "steelblue", 'Recruitment')
grid <- lime_plotter$build_parallell_plots(list(spr_g, f_g, r_g), "SPR, F and Recruitment estimates for LIME", size = 15, just = 'centre')
grid


data <- lbi_results$estimates
data$years <- lbi_results$years
thresholds <- list(
  Lc_Lmat = 1,
  L25_Lmat = 1,
  Lmax5_Linf = 0.8,
  Pmega = 0.3,
  Lmean_Lopt = 0.9,
  Lmean_Lfem = 1)
lbi_plotter <- LbiOutputPlotter$new(data)
grid <- lbi_plotter$build_lbi_tower("Lbi scores", thresholds)
grid


# Draw length-frequency distribution
plot_context <- PlotContext$new()
plot_context <- PlotContext$new()
plot_context$title_size <- 10
plot_context$face_text_size <- 2
plot_context$x_text_size <- 8
plot_context$y_text_size <- 8
plot_context$x_lab <- "Length (cm)"
plot_context$y_lab <- "Count"

g <- generate_species_yearly_arte_length_plot(catch_weight_data$catch_long,
                                              'MeanLength',
                                              'catch',
                                              plot_context)


plot_context$title <- "Length frequency distribution for years 2001 and 2021"
g <- generate_biannual_distribution(catch_weight_data$catch_long,
                                    'MeanLength',
                                    'catch',
                                    plot_context,
                                    2001,
                                    2022,
                                    bar_filled_colours = c("lightblue4", "red"),
                                    alpha_values = c(.8, .3))
g

