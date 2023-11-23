library("dplyr")
library("ggplot2")

generate_yearly_length_distribution <- function(data, x, y, plot_context) {

  # Build number of indiduals per year
  year_num_individuals <- data %>%
    group_by(year) %>%
    summarise(num_ind = sum(catch)) %>%
    ungroup() %>%
    mutate(num_ind = paste0("n = ", num_ind))
  x_label <- median(data[[x]])

  nyears <- length(unique(data$year))
  ncol <- ceiling(sqrt(nyears))
  g <- ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
    facet_wrap(~year, ncol = ncol, scales = 'fixed') +
    geom_bar(stat = "identity", orientation = 'x') +
    geom_text(data = year_num_individuals, aes(x = x_label, y = Inf, label = num_ind, vjust = 1.1)) +
    ggtitle(plot_context$title) +
    xlab(plot_context$x_lab) +
    ylab(plot_context$y_lab) +
    theme_bw() +
    theme(axis.text = element_text(size = plot_context$x_text_size),
          axis.title = element_text(size = plot_context$x_text_size, face = "bold"),
          plot.title = element_text(hjust = 0.5, size = plot_context$title_size, face = 'bold'))

  return(g)
}

generate_biannual_distribution <- function(data, x, y, plot_context, year_a, year_b, bar_filled_colours, alpha_values) {

  data_a <- data %>%
    filter(year == year_a) %>%
    select(-interval)
  data_b <- data %>%
    filter(year == year_b) %>%
    select(-interval)
  plot_data <- rbind(data_a, data_b) %>%
    mutate(year = factor(year))

  g <- ggplot(plot_data, aes(x = .data[[x]], y = .data[[y]], fill = year, alpha = year)) +
    geom_bar(stat = "identity", position = "identity") +
    scale_fill_manual(values = c("lightblue", "red")) +
    scale_alpha_manual(values = alpha_values) +
    ggtitle(plot_context$title) +
    xlab(plot_context$x_lab) +
    ylab(plot_context$y_lab) +
    theme_bw() +
    theme(axis.text = element_text(size = plot_context$x_text_size),
          axis.title = element_text(size = plot_context$x_text_size, face = "bold"),
          plot.title = element_text(hjust = 0.5, size = plot_context$title_size, face = 'bold'))
  return(g)

}
