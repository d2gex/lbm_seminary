library("R6")
library("ggplot2")
library("ggpubr")
library("stringr")
source("utils.R")

AlgoOutputPlotter <- R6Class("AlgoOutputPlotter", public = list(
  all_species_data = NULL,
  lbi_thresholds = NULL,
  initialize = function(all_species_data, lbi_thresholds) {
    self$all_species_data <- all_species_data
    self$lbi_thresholds <- lbi_thresholds
  },
  build_all_species_grids = function() {
    pages <- list()
    for (ind_species in names(self$all_species_data)) {
      species_data <- self$all_species_data[[ind_species]]
      pages[[length(pages) + 1]] <- private$build_species_grid(species_data)
    }
    return(pages)
  }
), private = list(
  build_species_grid = function(data) {

    # (1) Build title for grid
    grid_title <- build_grid_title(data$title)
    f_plot <- private$build_f_plot(data$F)
    sl_plot <- private$build_sl_plot(data$SL)
    spr_plot <- private$build_spr_plot(data$SPR)
    lbi_plot <- private$generate_lbi_tower(data$LBI, "Lbi scores", self$lbi_thresholds)
    empty_plot <- create_empty_plot(empty_message = "", title = "")
    if (is.null(data$R)) {
      lbspr_graphs <- ggarrange(
        plotlist = list(spr_plot, f_plot, empty_plot, sl_plot),
        ncol = 2,
        nrow = 2)
      grid_plots <- ggarrange(
        plotlist = list(lbspr_graphs, lbi_plot),
        ncol = 2,
        nrow = 1
      )
    }
    else {
      rec_plot <- private$build_rec_plot(data$R)
      lbspr_lime_graphs <- ggarrange(
        plotlist = list(spr_plot, f_plot, rec_plot, sl_plot),
        ncol = 2,
        nrow = 2)
      grid_plots <- ggarrange(
        plotlist = list(lbspr_lime_graphs, lbi_plot),
        ncol = 2,
        nrow = 1
      )
    }
    return(ggarrange(
      plotlist = list(grid_title, grid_plots),
      nrow = 2,
      ncol = 1,
      heights = c(1, 10)
    ))
  },
  build_spr_plot = function(data) {
    years_midpoint <- data$years[length(data$years) / 2]
    is_lime <- FALSE
    # Calculate average if lime entry exist
    if (TRUE %in% grepl('LIME', names(data))) {
      is_lime <- TRUE
    }
    # Catch only algo names and make a long dataframe
    spr_data <- data %>%
      rename_with(stringr::str_replace, # Leave only the name of the algorithms
                  pattern = "SPR_", replacement = "",
                  matches("_")) %>%
      gather(key = "variable", value = "value", -years)

    # Generate the plot
    g <- ggplot(spr_data, aes(x = years, y = value)) +
      geom_point(aes(color = variable)) +
      geom_line(aes(color = variable, linetype = variable))

    if (is_lime) {
      g <- g + scale_color_manual(values = c(LIME = "orangered", LBSPR = "steelblue"))
    }
    else {
      g <- g + scale_color_manual(values = c(LBSPR = "steelblue"))
    }

    g <- g +
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
      theme(legend.title = element_blank(),
            legend.position = "top",
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            legend.text = element_text(size = 8),
            legend.key.size = unit(1.2, "lines"))
    return(g)
  },
  build_f_plot = function(data) {

    is_lime <- FALSE
    # Calculate average if lime entry exist
    if (TRUE %in% grepl('LIME', names(data))) {
      is_lime <- TRUE
    }

    f_data <- data %>%
      select(years, starts_with("F_L")) %>%
      rename_with(stringr::str_replace, # Leave only the name of the algorithms
                  pattern = "F_", replacement = "",
                  matches("_"))
    if (is_lime) {
      f30_lime <- unique(data$F30_LIME)
      f40_lime <- unique(data$F40_LIME)
      years_midpoint <- data$years[length(data$years) / 2]
      f_data <- f_data %>%
        gather(key = "variable", value = "value", -years)
    }
    else {
      f_data <- f_data %>%
        mutate(variable = 'LBSPR', value = LBSPR)
    }

    g <- ggplot(f_data, aes(x = years, y = value)) +
      geom_point(aes(color = variable)) +
      geom_line(aes(color = variable, linetype = variable))

    if (is_lime) {
      g <- g +
        scale_color_manual(values = c(LIME = "orangered", LBSPR = "steelblue")) +
        geom_hline(yintercept = f30_lime, linetype = 'dashed', col = 'red') +
        geom_hline(yintercept = f40_lime, linetype = 'dotted', col = 'green') +
        annotate(geom = "text",
                 label = "Fspr30",
                 x = years_midpoint,
                 y = f30_lime,
                 size = 3,
                 hjust = 1) +
        annotate(geom = "text",
                 label = "Fspr40",
                 x = years_midpoint,
                 y = f40_lime,
                 size = 3,
                 hjust = 1)
    }
    else {
      g <- g + scale_color_manual(values = c(LBSPR = "steelblue"))
    }

    g <- g +
      xlab(str_to_title('Years')) +
      ylab("F") +
      theme_bw() +
      theme(legend.title = element_blank(),
            legend.position = "top",
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            legend.text = element_text(size = 8),
            legend.key.size = unit(1.2, "lines"))
    return(g)
  },
  build_fm_plot = function(data) {
    f_data <- data %>%
      select(years, starts_with("FM_")) %>%
      rename_with(stringr::str_replace, # Leave only the name of the algorithms
                  pattern = "FM_", replacement = "",
                  matches("_"))

    if (length(names(f_data)) == 3) {
      is_lime <- TRUE
      f_data <- f_data %>%
        gather(key = "variable", value = "value", -years)
    }
    else {
      is_lime <- FALSE
      f_data <- f_data %>%
        mutate(variable = 'LBSPR') %>%
        rename(c(value = 'LBSPR'))
    }
    g <- ggplot(f_data, aes(x = years, y = value)) +
      geom_point(aes(color = variable)) +
      geom_line(aes(color = variable))

    # Is LIME in the output?
    if (is_lime) {
      g <- g + scale_color_manual(values = c(LIME = "orangered", LBSPR = "steelblue"))
    }
      # Otherwise just use LBSPR
    else {
      g <- g + scale_color_manual(values = c(LBSPR = "steelblue"))
    }
    g <- g +
      xlab(str_to_title('Years')) +
      ylab("FM") +
      theme_bw() +
      theme(legend.title = element_blank(),
            legend.position = "top",
            legend.text = element_text(size = 8),
            legend.key.size = unit(1.2, "lines"))
    return(g)
  },
  build_rec_plot = function(data) {
    r_data <- data %>%
      mutate(method = 'LIME')
    g <- ggplot(r_data, aes(x = years, y = R_LIME)) +
      geom_point(aes(colour = method)) +
      geom_line(aes(colour = method)) +
      scale_color_manual(values = c(LIME = "orangered")) +
      xlab(str_to_title('Years')) +
      ylab("Recruitment Variability") +
      theme_bw() +
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.size = unit(1.2, "lines"))
    return(g)
  },
  build_single_lbi_row_plot = function(data, colname, threshold, is_bottom) {
    g <- ggplot(data, aes(x = factor(years), y = .data[[colname]])) +
      geom_line(aes(group = 1)) +
      geom_point(aes(colour = fitness)) +
      geom_hline(yintercept = threshold, linetype = 'dotted', col = 'green') +
      scale_color_manual(values = c(yes = "limegreen", no = 'red4')) +
      theme_bw() +
      xlab('Years')

    if (is_bottom) {
      g <- g +
        theme(
          axis.text.x = element_text(angle = 45),
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
    return(g)
  },
  build_all_lbi_plots = function(data, thresholds) {
    col_names <- names(data)[-1]
    plots <- list()
    for (i in seq_along(col_names)) {
      c_name <- col_names[[i]]
      tokens <- unlist(str_split(c_name, "_"))
      if (length(tokens) == 3) {
        new_c_name <- paste(tokens[1:2], collapse = "_")
      }
      else {
        new_c_name <- tokens[1]
      }
      col_threshold <- thresholds[[new_c_name]]
      col_data <- data.frame(years = data$years)
      col_data[[new_c_name]] <- data[[c_name]]
      if (str_detect(new_c_name, 'Lfem')) {
        col_data$fitness <- ifelse(col_data[[new_c_name]] >= col_threshold, 'yes', 'no')
      }
      else {
        col_data$fitness <- ifelse(col_data[[new_c_name]] > col_threshold, 'yes', 'no')
      }
      plots[[i]] <- private$build_single_lbi_row_plot(col_data,
                                                      new_c_name,
                                                      col_threshold,
                                                      is_bottom = ifelse(i == 1, TRUE, FALSE))
    }
    return(plots)
  },
  generate_lbi_tower = function(data, grid_title, thresholds) {
    plots <- private$build_all_lbi_plots(data, thresholds)
    tower <- ggarrange(plotlist = rev(plots),
                       ncol = 1,
                       nrow = length(thresholds))
    tower_title <- build_grid_title(grid_title, size = 10)
    return(ggarrange(
      plotlist = list(tower_title, tower),
      ncol = 1,
      nrow = 2,
      heights = c(0.5, 10)
    ))
  },
  build_sl_plot = function(data) {
    is_lime <- FALSE
    # Calculate average if lime entry exist
    if (TRUE %in% grepl('LIME', names(data))) {
      is_lime <- TRUE
    }

    # Catch only algo names and make a long dataframe
    sl50 <- data %>%
      select(matches("years") | starts_with("SL50_", ignore.case = FALSE)) %>%
      rename_with(stringr::str_replace, # Leave only the name of the algorithms
                  pattern = "SL50_", replacement = "",
                  matches("_")) %>%
      gather(key = "variable", value = "value", -years) %>%
      rename(c(method = as.factor('variable'))) %>%
      mutate(selectivity = as.factor('SL50'))

    sl95 <- data %>%
      select(matches("years") | starts_with("SL95_", ignore.case = FALSE)) %>%
      rename_with(stringr::str_replace, # Leave only the name of the algorithms
                  pattern = "SL95_", replacement = "",
                  matches("_")) %>%
      gather(key = "variable", value = "value", -years) %>%
      rename(c(method = as.factor('variable'))) %>%
      mutate(selectivity = as.factor('SL95'))

    sl <- rbind(sl50, sl95) %>%
      mutate(sel_method = interaction(selectivity, method, sep = "_")) %>%
      select(-c(selectivity, method))

    circle_shape <- 16
    trinagle_shape <- 17
    # Generate the plot
    g <- ggplot(data = sl, aes(x = years,
                               y = value,
                               colour = sel_method,
                               shape = sel_method)) +
      geom_point() +
      geom_line()

    if (is_lime) {
      g <- g +
        scale_color_manual(name = 'unused', values = c(SL50_LIME = "orangered",
                                                       SL50_LBSPR = "steelblue",
                                                       SL95_LIME = "orangered",
                                                       SL95_LBSPR = "steelblue"),
                           labels = c('S50', 'S95', 'S50', 'S95')) +
        scale_shape_manual(name = 'unused', values = c(SL50_LIME = circle_shape,
                                                       SL50_LBSPR = circle_shape,
                                                       SL95_LIME = trinagle_shape,
                                                       SL95_LBSPR = trinagle_shape),
                           labels = c('S50', 'S95', 'S50', 'S95'))
    }
    else {
      g <- g +
        scale_color_manual(name = 'unused',
                           values = c(SL50_LBSPR = "steelblue", SL95_LBSPR = "steelblue"),
                           labels = c('S50', 'S95')) +
        scale_shape_manual(name = 'unused',
                           values = c(SL50_LBSPR = circle_shape, SL95_LBSPR = trinagle_shape),
                           labels = c('S50', 'S95'))
    }

    g <- g +
      xlab(str_to_title('Years')) +
      ylab("Selectivity (cm)") +
      theme_bw() +
      theme(legend.title = element_blank(),
            legend.position = "top",
            legend.text = element_text(size = 8),
            legend.key.size = unit(1.2, "lines"))

    return(g)
  }
))