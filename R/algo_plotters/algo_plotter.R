library("R6")
library("tidyverse")
library("ggpubr")

AlgoOutputPlotter <- R6Class("AlgoOutputPlotter", public = list(

  data = NULL,
  M = NULL,
  initialize = function(data, M) {
    self$data <- data
    self$M <- M
  },
  build_spr_plot = function(d_colour) {
    years_midpoint <- self$data$years[length(self$data$years) / 2]
    spr_data <- self$data %>%
      select(years, SPR)

    # Generate the plot
    g <- ggplot(spr_data, aes(x = years, y = SPR)) +
      geom_point(aes(colour = d_colour)) +
      geom_line(aes(colour = d_colour)) +
      scale_color_manual(values = c(d_colour)) +
      geom_hline(yintercept = 0.3, linetype = 'dashed', col = 'red') +
      geom_hline(yintercept = 0.4, linetype = 'dotted', col = 'green') +
      annotate(geom = "text",
               label = "SPR 30",
               x = years_midpoint,
               y = 0.3,
               size = 3,
               hjust = 1) +
      annotate(geom = "text",
               label = "SPR 40",
               x = years_midpoint,
               y = 0.4,
               size = 3,
               hjust = 1) +
      xlab(str_to_title('Years')) +
      ylab("SPR") +
      theme_bw() +
      theme(legend.position = "none",
            legend.key.size = unit(1.2, "lines"))
    return(g)
  },
  build_parallell_plots = function(plots, title, size, just) {
    title <- as_ggplot(text_grob(title, size = size, just = just))
    algo_outputs <-
      ggarrange(
        plotlist = plots,
        ncol = 2,
        nrow = ifelse(length(plots) <= 2, 1, 2)
      )
    outer_grid <- ggarrange(
      plotlist = list(title, algo_outputs),
      ncol = 1,
      nrow = 2,
      heights = c(1, 10)
    )
    return(outer_grid)
  }
))

LbsprOutputPlotter <- R6Class("LbsprOutputPlotter", inherit = AlgoOutputPlotter, public = list(

  initialize = function(data, M) {
    super$initialize(data, M)
  },
  build_fm_plot = function(d_colour) {
    g <- self$data %>%
      select(years, FM) %>%
      ggplot(aes(x = years, y = FM)) +
      geom_point(aes(color = d_colour)) +
      geom_line(aes(color = d_colour)) +
      scale_color_manual(values = c(d_colour)) +
      xlab(str_to_title('Years')) +
      ylab("FM") +
      theme_bw() +
      theme(legend.position = "none",
            legend.key.size = unit(1.2, "lines"))
    return(g)
  }

))

LimeOutputPlotter <- R6Class("LimeOutputPlotter", inherit = AlgoOutputPlotter, public = list(

  initialize = function(data, M) {
    super$initialize(data, M)
  },
  build_score = function(d_colour, column) {
    g <- self$data %>%
      select_at(.vars = c('years', column)) %>%
      ggplot(aes(x = years, y = .data[[column]])) +
      geom_point(aes(color = d_colour)) +
      geom_line(aes(color = d_colour)) +
      scale_color_manual(values = c(d_colour)) +
      xlab(str_to_title('Years')) +
      ylab(column) +
      theme_bw() +
      theme(legend.position = "none",
            legend.key.size = unit(1.2, "lines"))
    return(g)
  }

))