source("config.R")
source("utils.R")
source("plot_utils.R")
source("lbm_data_holder.R")
source("lbspr.R")


diplodus_data <- read_sheets_from_excel(FREQUENCY_DATA_PATH)
catch_weight_data <- CatchWeightMatrices$new(catch = diplodus_data$catch,
                                             catch_long = diplodus_data$catch_long,
                                             weight = diplodus_data$weight,
                                             weight_long = diplodus_data$weight_long)
exp_params <- ExplotationParameters$new()
# lbspr_algo <- Lbspr$new(bio_params, exp_params, catch_weight_data)
# results <- lbspr_algo$run()

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
g
