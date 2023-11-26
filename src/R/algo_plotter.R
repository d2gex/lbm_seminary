library("R6")
library("tidyverse")
library("ggpubr")
source("R/utils.R")

AlgoOutputPlotter <- R6Class("AlgoOutputPlotter", public = list(

  data = NULL,
  initialize = function(data) {
    self$data <- data
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

  initialize = function(data) {
    super$initialize(data)
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

  initialize = function(data) {
    super$initialize(data)
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

LbiOutputPlotter <- R6Class("LbiOutputPlotter", inherit = AlgoOutputPlotter, public = list(

  initialize = function(data) {
    super$initialize(data)
  },
  build_lbi_tower = function(grid_title, thresholds) {
    plots <- private$build_all_lbi_plots(self$data, thresholds)
    tower <- ggarrange(plotlist = rev(plots),
                       ncol = 1,
                       nrow = length(thresholds))
    tower_title <- build_grid_title(grid_title, size = 22)
    return(ggarrange(
      plotlist = list(tower_title, tower),
      ncol = 1,
      nrow = 2,
      heights = c(0.5, 10)
    ))
  }
), private = list(
  rename_lbi_y_axis = function(name) {
    return(switch(name,
                  'Lc_Lmat' = bquote(L[c] / L[mat]),
                  'L25_Lmat' = bquote(L['25%'] / L[mat]),
                  'Lmax5_Linf' = bquote(L['max5%'] / L[inf]),
                  'Lmean_Lopt' = bquote(L[mean] / L[opt]),
                  'Lmean_Lfem' = bquote(L[mean] / L[FEM]),
                  'Pmega' = bquote(P[mega])))

  },
  build_single_lbi_row_plot = function(data, colname, threshold, is_bottom) {
    g <- ggplot(data, aes(x = factor(years), y = .data[[colname]])) +
      geom_line(aes(group = 1)) +
      geom_point(aes(colour = fitness)) +
      geom_hline(yintercept = threshold, linetype = 'dotted', col = 'green', linewidth = 1.2) +
      scale_color_manual(values = c(yes = "limegreen", no = 'red4')) +
      theme_bw() +
      xlab('Years') +
      ylab(private$rename_lbi_y_axis(colname))

    if (is_bottom) {
      g <- g +
        theme(
          axis.text.x = element_text(angle = 90, size = 15),
          axis.title.x = element_text(size = 18),
          legend.position = "none"
        )
    }
    else {
      g <- g +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              legend.position = "none")
    }
    g <- g + theme(axis.title.y = element_text(size = 18))
    return(g)
  },
  build_all_lbi_plots = function(data, thresholds) {
    col_names <- names(data)
    col_names <- col_names[col_names != 'years']
    plots <- list()
    for (i in seq_along(col_names)) {
      c_name <- col_names[[i]]
      col_threshold <- thresholds[[c_name]]
      col_data <- data.frame(years = data$years)
      col_data[[c_name]] <- data[[c_name]]
      if (str_detect(c_name, 'Lfem')) {
        col_data$fitness <- ifelse(col_data[[c_name]] >= col_threshold, 'yes', 'no')
      }
      else {
        col_data$fitness <- ifelse(col_data[[c_name]] > col_threshold, 'yes', 'no')
      }
      plots[[i]] <- private$build_single_lbi_row_plot(col_data,
                                                      c_name,
                                                      col_threshold,
                                                      is_bottom = ifelse(i == 1, TRUE, FALSE))
    }
    return(plots)
  }

))
