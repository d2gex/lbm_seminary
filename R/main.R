source("config.R")
source("utils.R")
source("plot_utils.R")
source("lbm_data_holder.R")
source("lbspr.R")
source("algo_plotters/algo_plotter.R")

diplodus_data <- read_sheets_from_excel(FREQUENCY_DATA_PATH)
catch_weight_data <- CatchWeightMatrices$new(catch = diplodus_data$catch,
                                             catch_long = diplodus_data$catch_long,
                                             weight = diplodus_data$weight,
                                             weight_long = diplodus_data$weight_long)
exp_params <- ExplotationParameters$new()
lbspr_algo <- Lbspr$new(bio_params, exp_params, catch_weight_data)
results <- lbspr_algo$run()

data <- as.data.frame(results$results)
data$years <- results$years
lbspr_plotter <- LbsprOutputPlotter$new(data, bio_params$M)
spr_g <- lbspr_plotter$build_spr_plot(d_colour = "steelblue")
fm_g <- lbspr_plotter$build_fm_plot(d_colour = "steelblue")
grid <- lbspr_plotter$build_parallell_plots(spr_g, fm_g, "SPR and F/M estimates for LBSPR", size = 15, just = 'centre')
grid




# # Draw length-frequency distribution
# plot_context <- PlotContext$new()
# plot_context <- PlotContext$new()
# plot_context$title_size <- 10
# plot_context$face_text_size <- 2
# plot_context$x_text_size <- 8
# plot_context$y_text_size <- 8
# plot_context$x_lab <- "Length (cm)"
# plot_context$y_lab <- "Count"
#
# g <- generate_species_yearly_arte_length_plot(catch_weight_data$catch_long,
#                                               'MeanLength',
#                                               'catch',
#                                               plot_context)


# plot_context$title <- "Length frequency distribution for years 2001 and 2021"
# g <- generate_biannual_distribution(catch_weight_data$catch_long,
#                                     'MeanLength',
#                                     'catch',
#                                     plot_context,
#                                     2001,
#                                     2022,
#                                     bar_filled_colours = c("lightblue4", "red"),
#                                     alpha_values = c(.8, .3))
# g

