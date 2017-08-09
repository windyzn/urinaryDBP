#' Summarise mean UDBP by a factor variable
#'
#' @param data
#' @param by factor variable to summarise mean UDBP
#'
#' @export
#'
#' @examples
#' trend <- summarise_progress_data(ds, by = "fMedsBP")
summarise_progress_data <- function(data, by) {
  data %>%
    dplyr::group_by_(by, "fVN") %>%
    dplyr::summarise(UDBP=mean(log(UDBP)))
}

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
plot_progress_data <- function(data, summary_data,
                               xvar = "fVN", yvar = "log(UDBP)",
                               byvar, groupby = "SID") {
  data %>%
    ggplot2::ggplot(ggplot2::aes_string(x = xvar,
                               y = yvar,
                               colour = byvar)) +
    ggplot2::geom_line(ggplot2::aes_string(group = groupby), alpha = 0.1) +
    ggplot2::geom_line(data = summary_data, ggplot2::aes_string(group = byvar),
                       alpha = 0.7, size = 2) +
    ggplot2::ylab("log(UDBP)")
}
