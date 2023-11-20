library("R6")
library("tidyverse")

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