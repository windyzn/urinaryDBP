#' Plot progression of UDBP over time (visit number) grouped by variables
#'
#' @param data
#' @param summary_data
#' @param xvar
#' @param yvar
#' @param byvar
#' @param groupby
#'
#' @export
#'
#' @examples
plot_progress_by <- function(data,
                               xvar = "fVN", yvar = "log(UDBP)",
                               ylab = "log(UDBP)",
                               byvar, groupby = "SID") {
  # m <- data %>%
  #   dplyr::group_by_(byvar, xvar) %>%
  #   dplyr::summarise(UDBP=mean(UDBP))

  data %>%
    ggplot2::ggplot(ggplot2::aes_string(x = xvar,
                               y = yvar,
                               colour = byvar)) +
    ggplot2::geom_line(ggplot2::aes_string(group = groupby), alpha = 0.1) +
    ggplot2::geom_line(ggplot2::aes_string(group = byvar),
                       alpha = 0.7, size = 2,
                       stat = "summary", fun.y = mean) +
    ggplot2::ylab(ylab)
}


#' Plot progression of UDBP over time (visit number) with no grouping variable
#'
#' @param data
#' @param xvar
#' @param yvar
#' @param ylab
#' @param groupby
#'
#' @return
#'
#' @examples
plot_progress <- function(data = project_data,
                               xvar = "fVN", yvar,
                               ylab,
                               groupby = "SID") {

  data %>%
    ggplot2::ggplot(ggplot2::aes_string(x = xvar,
                                        y = yvar)) +
    ggplot2::geom_line(data = data, ggplot2::aes_string(group = groupby), alpha = 0.7,
                       colour = "#cccccc") +
    ggplot2::geom_line(ggplot2::aes(group = 1), alpha = 0.7, size = 2, colour = "#050202",
                       stat = "summary", fun.y = mean) +
    ggplot2::ylab(ylab)
}


# Boxplot -----------------------------------------------------------------

plot_progress_boxplot <- function (data,
                                   xvar = "fVN",
                                   yvar, ylab,
                                   groupby = "SID") {

  data %>%
  ggplot2::ggplot(ggplot2::aes_string(x = xvar,
                                      y = yvar)) +
  ggplot2::geom_line(data = data, ggplot2::aes_string(group = groupby), alpha = 0.7,
                     colour = "#cccccc") +

  ggplot2::geom_boxplot(ggplot2::aes_string(colour = xvar, fill = xvar)) +
  ggplot2::stat_summary(geom = "crossbar", width = 0.65, fatten = 0, color = "white",
                        fun.data = function(x){
                          return(c(y = median(x), ymin = median(x), ymax = median(x)))
                        }) +

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
  ggplot2::ylab(ylab)
}
