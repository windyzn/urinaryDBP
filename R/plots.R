# Boxplot -----------------------------------------------------------------


#' Boxplot
#'
#' @param data Cleaned data
#' @param xvar Variable on x-axis
#' @param yvar Variable on y-axis
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param facet Split the plot by visit number
#'
#' @return Plot
#'
#' @examples
#' ds %>%
#'   filter(fVN == "Baseline") %>%
#'   select(acrStatus, udbpCrRatio) %>%
#'   na.omit() %>%
#'   box_plot("acrStatus", "log(udbpCrRatio)",
#'            "Albuminuria",
#'            "log uVDBP:Creatinine")

box_plot <- function(data, xvar, yvar, xlab="", ylab="", facet = FALSE) {
  myboxplot <- ggplot2::ggplot(data, ggplot2::aes_string(x = xvar, y = yvar)) +
    ggplot2::geom_boxplot(ggplot2::aes_string(colour = xvar, fill = xvar),
                          outlier.shape = NA) +
    ggplot2::stat_summary(geom = "crossbar", width = 0.65, fatten = 0, color = "white",
                          fun.data = function(x){
                            return(c(y = median(x), ymin = median(x), ymax = median(x)))
                          }) +
    ggplot2::geom_jitter(ggplot2::aes_string(),
                         position = ggplot2::position_jitter(width = 0.15, height = 0),
                         alpha = 0.3,
                         size = 1,
                         colour = "grey30") +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_brewer(palette = "Dark2") +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major.x = ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(colour = "grey"),
                   axis.ticks.y = ggplot2::element_line(colour = "grey"),
                   axis.text.x = ggplot2::element_text(colour = "grey30"), #angle = 45
                   axis.title = ggplot2::element_text(size = 10)) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)

  if(facet == TRUE) {
    myboxplot +
      ggplot2::facet_grid(~fVN, switch = "x")
  } else {
    myboxplot
  }
}


#' Boxplots where the colour theme matches health template slides
#'
#' @param data
#' @param xvar
#' @param yvar
#' @param xlab
#' @param ylab
#' @param facet
#'
#' @return Plot
#'
#' @examples
box_plot_slides <- function(data, xvar, yvar, xlab="", ylab="", facet = FALSE) {
  myboxplot <- ggplot2::ggplot(data, ggplot2::aes_string(x = xvar, y = yvar)) +
    ggplot2::geom_boxplot(ggplot2::aes_string(colour = xvar, fill = xvar),
                          outlier.shape = NA) +
    ggplot2::stat_summary(geom = "crossbar", width = 0.65, fatten = 0, color = "white",
                          fun.data = function(x){
                            return(c(y = median(x), ymin = median(x), ymax = median(x)))
                          }) +
    ggplot2::geom_jitter(ggplot2::aes_string(),
                         position = ggplot2::position_jitter(width = 0.15, height = 0),
                         alpha = 0.3,
                         size = 1,
                         colour = "grey30") +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c("#0db7c4", "#f24745", "#a9d039")) +
    ggplot2::scale_fill_manual(values = c("#0db7c4", "#f24745", "#a9d039")) +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major.x = ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_blank(),
                   text = ggplot2::element_text(size = 14, family = "Open Sans"),
                   axis.text.y = ggplot2::element_text(colour = "grey30"),
                   axis.ticks.y = ggplot2::element_line(colour = "grey"),
                   axis.text.x = ggplot2::element_text(colour = "grey30"),
                   axis.title = ggplot2::element_text(family = "Dosis")) + #angle = 45
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)

  if(facet == TRUE) {
    myboxplot +
      ggplot2::facet_grid(~fVN, switch = "x")
  } else {
    myboxplot
  }
}


#' Boxplots where the colour theme matches ada poster
#'
#' @param data
#' @param xvar
#' @param yvar
#' @param xlab
#' @param ylab
#' @param facet
#'
#' @return Plot
#'
#' @examples
box_plot_poster <- function(data, xvar, yvar, xlab="", ylab="", facet = FALSE) {
  myboxplot <- ggplot2::ggplot(data, ggplot2::aes_string(x = xvar, y = yvar)) +
    ggplot2::geom_boxplot(ggplot2::aes_string(colour = xvar, fill = xvar),
                          outlier.shape = NA) +
    ggplot2::stat_summary(geom = "crossbar", width = 0.65, fatten = 0, color = "white",
                          fun.data = function(x){
                            return(c(y = median(x), ymin = median(x), ymax = median(x)))
                          }) +
    ggplot2::geom_jitter(ggplot2::aes_string(),
                         position = ggplot2::position_jitter(width = 0.15, height = 0),
                         alpha = 0.3,
                         size = 1,
                         colour = "grey30") +
    ggplot2::scale_color_manual(values = c("#2b3443", "#adc9b7", "#ffd91e")) +
    ggplot2::scale_fill_manual(values = c("#2b3443", "#adc9b7", "#ffd91e")) +
    ggplot2::theme_grey() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major.x = ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_blank(),
                   plot.background = ggplot2::element_rect(colour = "grey60"),
                   text = ggplot2::element_text(size = 14, family = "Calibri Light"),
                   axis.text.y = ggplot2::element_text(colour = "grey30"),
                   axis.ticks.y = ggplot2::element_line(colour = "grey"),
                   axis.text.x = ggplot2::element_text(colour = "grey30"),
                   axis.title = ggplot2::element_text(family = "Calibri")) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)

  if(facet == TRUE) {
    myboxplot +
      ggplot2::facet_grid(~fVN, switch = "x")
  } else {
    myboxplot
  }
}

# Scatterplot -------------------------------------------------------------


#' Scatterplot
#'
#' @param data Clearned data
#' @param xvar Variable on x-axis
#' @param yvar Variable on y-axis
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param line Line of best fit
#' @param facet Split the plot by visit number
#'
#' @return Plot
#'
#' @examples
scatter_plot = function(data, xvar, yvar, xlab='', ylab='', line = TRUE, facet = FALSE) {
  myplot <- ggplot2::ggplot(data, ggplot2::aes_string(x=xvar, y=yvar)) +
    ggplot2::geom_point(colour = "#0db7c4", size = 1) + #mapping=aes(color=mcr_status)
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_blank(),
                   text = ggplot2::element_text(size = 14, family = "Open Sans"),
                   axis.text.y = ggplot2::element_text(colour = "grey30"), #"grey" normally
                   axis.ticks.y = ggplot2::element_line(colour = "grey"),
                   axis.text.x = ggplot2::element_text(colour = "grey30"), #angle = 45
                   axis.title = ggplot2::element_text(family = "Dosis")) + #size = 10 normally
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)
  if(line == TRUE & facet == FALSE) {
    myplot +
      ggplot2::geom_smooth(method = lm, colour = "grey50")
  } else if(line == TRUE & facet == TRUE) {
    myplot +
      ggplot2::geom_smooth(method = lm, colour = "grey50") +
      ggplot2::facet_grid(~fVN, switch = "x")
  } else if(line == FALSE & facet == TRUE) {
    myplot +
      ggplot2::facet_grid(~fVN, switch = "x")
  } else {
    myplot
  }
}

#' Scatterplot where the colour theme matches ada poster
#'
#' @param data Clearned data
#' @param xvar Variable on x-axis
#' @param yvar Variable on y-axis
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param line Line of best fit
#' @param facet Split the plot by visit number
#'
#' @return Plot
#'
#' @examples
scatter_plot_poster = function(data, xvar, yvar, xlab='', ylab='', line = TRUE, facet = FALSE) {
  myplot <- ggplot2::ggplot(data, ggplot2::aes_string(x=xvar, y=yvar)) +
    ggplot2::geom_point(colour = "#adc9b7", size = 1) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   plot.background = ggplot2::element_rect(colour = "grey60"),
                   axis.line.y = ggplot2::element_blank(),
                   text = ggplot2::element_text(size = 14, family = "Montserrat Light"),
                   axis.text.y = ggplot2::element_text(colour = "grey30"),
                   axis.ticks.y = ggplot2::element_line(colour = "grey"),
                   axis.text.x = ggplot2::element_text(colour = "grey30"), #angle = 45
                   axis.title = ggplot2::element_text(family = "Montserrat Medium")) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)
  if(line == TRUE & facet == FALSE) {
    myplot +
      ggplot2::geom_smooth(method = loess, colour = "grey50")
  } else if(line == TRUE & facet == TRUE) {
    myplot +
      ggplot2::geom_smooth(method = loess, colour = "grey50") +
      ggplot2::facet_grid(~fVN, switch = "x")
  } else if(line == FALSE & facet == TRUE) {
    myplot +
      ggplot2::facet_grid(~fVN, switch = "x")
  } else {
    myplot
  }
}

# Histogram ---------------------------------------------------------------


#' Histogram
#'
#' @param data Cleaned data
#' @param variable Variable of interest
#' @param bin Width of bars
#' @param xlab x-axis label
#' @param facet Separate the graph by visit number
#'
#' @return Plot
#'
#' @examples
histo_plot = function(data, variable, bin, xlab='', facet = FALSE) {
  histoplot <- ggplot2::ggplot(data, ggplot2::aes_string(x=variable)) +
    ggplot2::geom_histogram(binwidth=bin,
                            colour='#0db7c4', fill='#0db7c4') +
    ggplot2::xlab(xlab) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_brewer(palette = "Dark2") +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(colour = "grey"),
                   axis.ticks.y = ggplot2::element_line(colour = "grey"),
                   axis.text.x = ggplot2::element_text(colour = "grey30"), #angle = 45
                   axis.title = ggplot2::element_text(size = 10))
  if(facet == TRUE) {
    histoplot +
      ggplot2::facet_grid(~fVN, switch = "x")
  } else {
    histoplot
  }
}


# Line Plot ---------------------------------------------------------------


#' Line Plot
#'
#' @param data Dataset (cleaned)
#' @param xvar Variable on x axis (continous or factor)
#' @param yvar Variable on y axis (continuous or factor)
#' @param xlab Axis label for x axis
#' @param ylab Axis label for y axis
#'
#' @return Plot
#'
#' @examples
#' ds %>%
#'   dplyr::filter(SID < 1050 ) %>%
#'   line_plot("fVN", "UDBP", "SID", "Visit Number", "UDBP")
line_plot = function(data, xvar, yvar, byvar, xlab='', ylab='') {
  ggplot2::ggplot(data, ggplot2::aes_string(x=xvar, y=yvar,
                                            group=byvar, colour=byvar)) +
    ggplot2::geom_line(alpha = 0.5) +
    # ggplot2::geom_point(alpha = 0.5) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major.x = ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(colour = "grey"),
                   axis.ticks.y = ggplot2::element_line(colour = "grey"),
                   axis.text.x = ggplot2::element_text(colour = "grey30"),
                   axis.title = ggplot2::element_text(size = 10)) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)
}


# GEE Plot ----------------------------------------------------------------

#' GEE Plot
#'
#' @return
#' @export
#'
#' @examples
gee_plot = function(gee_results, xlab = "") {
  seer::view_main_effect(gee_results, "dot.size", group.label.switch = 'y') +
    ggplot2::theme_grey() +
    ggplot2::theme(legend.position = 'right',
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.spacing = ggplot2::unit(2, "lines"),
                   strip.background = ggplot2::element_rect(colour = "#adc9b7", fill = "#adc9b7"),
                   strip.text.x = ggplot2::element_text(colour = "white", face = "bold"),
                   text = ggplot2::element_text(family = "Calibri"),
                   axis.line.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(colour = "grey"),
                   axis.ticks.y = ggplot2::element_line(colour = "grey"),
                   axis.text.x = ggplot2::element_text(colour = "grey40"),
                   axis.title = ggplot2::element_text(size = 10, family = "Calibri")) +
    ggplot2::ylab('') +
    ggplot2::xlab(xlab)
}
