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
plot_progress_data <- function(data,
                               xvar = "fVN", yvar = "log(UDBP)",
                               byvar, groupby = "SID") {
  m <- data %>%
    dplyr::group_by_(byvar, xvar) %>%
    dplyr::summarise(UDBP=mean(log(UDBP)))

  data %>%
    ggplot2::ggplot(ggplot2::aes_string(x = xvar,
                               y = yvar,
                               colour = byvar)) +
    ggplot2::geom_line(ggplot2::aes_string(group = groupby), alpha = 0.1) +
    ggplot2::geom_line(data = m, ggplot2::aes_string(group = byvar),
                       alpha = 0.7, size = 2) +
    ggplot2::ylab("log(UDBP)")
}
