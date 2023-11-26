library("R6")

DATA_FOLDER_PATH <- '../data'
FREQUENCY_DATA_PATH <- file.path(DATA_FOLDER_PATH, 'Diplodus_sargus.xlsx')

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
  x_title_size = NULL,
  y_title_size = NULL,
  legend_position = NULL
))