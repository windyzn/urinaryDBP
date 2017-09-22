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
                        covars = NULL,
                        intvar = NULL) {

  co <- !is.null(covars)

  int <- !is.null(intvar)
  if (int) {
    extract_term <- ":"
  } else {
    extract_term <- "Xterm$"
  }

  data %>%
    mason::design("glm") %>%
    mason:::add_settings(family = stats::gaussian()) %>%
    mason::add_variables("yvars", y) %>%
    mason::add_variables("xvars", x) %>%
    mason::construct() %>% {
      if (co) {
        mason::add_variables(., "covariates", covars) %>% {
          if (int) {
            mason::add_variables(., "interaction", intvar)
          } else {
            .
          }
        }
      } else {
        .
      }
    } %>%
    mason::construct() %>%
    mason::scrub()
}
