source("config.R")
source("utils.R")
source("lbm_data_holder.R")
source("lbspr.R")


diplodus_data <- read_sheets_from_excel(FREQUENCY_DATA_PATH)
catch_weight_data <- CatchWeightMatrices$new(catch = diplodus_data$catch, weight = diplodus_data$weight)
exp_params <- ExplotationParameters$new()
lbspr_algo <- Lbspr$new(bio_params, exp_params, catch_weight_data)
results <- lbspr_algo$run()

