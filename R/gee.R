# Functions for the GEE analysis
#

# Grab or combine data ----------------------------------------------------


#' Prepare the project data for analysis through GEE
#'
#' @param data project data
#' @export

prep_gee_data <- function(data) {
  data %>%
    dplyr::mutate(
      udbpBase = ifelse(fVN == "Baseline", UDBP, NA),
      ageBase = ifelse(fVN == "Baseline", Age, NA),
      fDM = relevel(as.factor(DM), "1")
      # Ethnicity = ifelse(Ethnicity == "European", Ethnicity, "Other"),
    ) %>%
    dplyr::filter(!(fVN == "Baseline" &
                      acr_status == "Macroalbuminuria")) %>%
    dplyr::filter(!(fVN == "Baseline" &
                      eGFR_status == "Moderate")) %>%
    dplyr::arrange(SID, fVN) %>%
    dplyr::group_by(SID) %>%
    tidyr::fill(udbpBase, ageBase) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(UDBP = UDBP / 1000) %>%
    dplyr::arrange(SID, VN)
}


# Analyze -----------------------------------------------------------------

#' Run GEE on prepared project data
#'
#' @param data
#' @export
#'
#' @examples
analyze_gee <- function(data) {
  data %>%
    mason::mason_gee(
      yvars = c("ACR", "eGFR"),
      xvars = "UDBP",
      covars = c("VN", "ageBase", "fDM")
    )
}


# Plotting ----------------------------------------------------------------

#' Plot GEE results in a forest plot-style
#'
#' @param results
#'
#' @return
#' @export
#'
#' @examples
plot_gee_main <- function(results) {
  results %>%
    seer::view_main_effect(
      groups = '~Yterms',
      legend.title = 'P-value',
      xlab = 'Standard deviation difference with 95% CI in the outcomes\nfor every 1 unit increase in the component',
      ylab = 'PLS components',
      group.label.switch = 'both'
    ) +
    graph_theme(ticks = FALSE)
}
