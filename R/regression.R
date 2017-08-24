#' Multiple linear regression
#'
#' @param data
#' @param y
#' @param x
#' @param covars
#' @param intvar
#'
#' @return
#'
#' @examples
mason_glm <- function(data = project_data,
                        y = outcomes,
                        x = predictors,
                        covars = NULL) {

  co <- !is.null(covars)

  data %>%
    mason::design("glm") %>%
    mason:::add_settings(family = stats::gaussian()) %>%
    mason::add_variables("yvars", y) %>%
    mason::add_variables("xvars", x) %>%
    mason::construct() %>% {
      if (co) {
        mason::add_variables(., "covariates", covars) %>%
        mason::construct()
      } else {
        .
      }
    } %>%
    mason::scrub()
}
