graphDistribution <- function(x, y, bins = 25, legend = 'IQR', save = FALSE,
  name = NULL, round = FALSE){

  # initiate plot object
  p <- ggplot() +
    geom_histogram(
      aes(x = x, fill = 'grey79'), bins = bins, alpha = .5, color = 'black'
    )

    # define break parameters
  if (round == TRUE) {round <- 3; prop <- 1.5}
  else               {round <- 1; prop <- 1.2}

  # define graphical parameters for y
  y_min  <- 0
  y_max  <- prop * max(ggplot_build(p)$data[[1]]['y'])
  y_incr <- round((y_max - y_min) / 10, 0)
  y_col  <- y_max - y_incr

  # define graphical parameters for x
  x_max  <- max(ggplot_build(p)$data[[1]]['x'])
  x_min  <- min(ggplot_build(p)$data[[1]]['x'])
  x_incr <- round((x_max - x_min) / 5, 3)
  width  <- ggplot_build(p)$data[[1]] %>%
            {.['xmax'] - .['xmin']} %>%
            unlist() %>%
            unname() %>%
            {.[1]}

  # define label parameters
  signif  <- quantile(x, probs = .05)
  pvalue  <- ecdf(x)
  x_value <- pvalue(y)
  IQR_value <- as.character(round(y, 3))
  IQR_value <- ifelse(nchar(IQR_value) > 7, str_sub(IQR_value, 1, 6), IQR_value)
  label <- paste0(legend, ' = ', IQR_value, '\n p-value = ', round(x_value, 3))
  label <- str_replace_all(label, '(\\b0\\.)', '\\.')
  x_label_pos <- ifelse(x_value < .4, .7 * x_incr, -.7 * x_incr)

  # set breaks based on round
  x_breaks <- round(seq(x_min, x_max, x_incr), round)
  y_breaks <- round(seq(y_min, y_max, y_incr), round)

  # finish graph
  p <- p +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    geom_col(
      aes(x = y, y = y_col, fill = 'grey25'), color = 'black', width = width
    ) +
    geom_text(
      aes(x = y, y = y_col), label = label, family = 'LM Roman 10',
      position = position_nudge(x = x_label_pos, y = .25 * y_incr)
    ) +
    scale_fill_manual(
      name = element_blank(), values = c('grey25', 'grey79'),
      labels = paste0(c('Empirical ', 'Simulated '), legend)
    ) +
    labs(y = 'Frequency', x = paste0('Simulated ', legend)) +
    theme_bw() +
    theme(
      axis.title = element_text(size = 10),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 10, lineheight = 1.1, face = 'bold'),
      axis.text.x = element_text(size = 10, lineheight = 1.1, face = 'bold'),
      text = element_text(family = 'LM Roman 10'),
      panel.border = element_rect(color = 'black', size = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = 'grey79'),
      legend.position = 'bottom'
    )
p
  # save plot if requested
  if (save == TRUE & !is.null(name)) {
    ggsave(
      filename = paste0(name, '.pdf'), plot = p, device = cairo_pdf,
      path = 'plots', dpi = 100, width = 7, height = 5
    )
  }

  # return plot
  return(p)
}

