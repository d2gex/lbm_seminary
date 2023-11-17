library("R6")
source("lbm_data_holder.R")

DATA_FOLDER_PATH <- '../data'
FREQUENCY_DATA_PATH <- file.path(DATA_FOLDER_PATH, 'Diplodus_sargus.xlsx')


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

PlotContext <- R6Class("PlotContext", public = list(
  title = NULL,
  x_lab = NULL,
  y_lab = NULL,
  second_y_lab = NULL,
  legend_title = NULL,
  x_angle = NULL,
  title_size = NULL,
  face_text_size = NULL,
  x_text_size = NULL,
  y_text_size = NULL,
  legend_position = NULL
))

