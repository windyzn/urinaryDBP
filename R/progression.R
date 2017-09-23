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
