# Functions for the GEE analysis


# Grab or combine data ----------------------------------------------------

#' Prepare the project data for analysis through GEE. The output is a dataframe
#' with baseline values.
#'
#' @param data
#'
#' @export
prep_mason_data <- function(data) {
  data %>%
    dplyr::mutate(
      UDBP = UDBP/1000, # udbp units to ug/mL
      udbpBase = ifelse(fVN == "Baseline", UDBP, NA),
      ageBase = ifelse(fVN == "Baseline", Age, NA),
      DM = ifelse(DM == 1, "diabetes", "non_dia"),
      fDM = relevel(as.factor(DM), "non_dia"),
      Ethnicity = ifelse(Ethnicity == "European", Ethnicity, "Other"),
      Ethnicity = relevel(as.factor(Ethnicity), "Other")
    ) %>%
    dplyr::arrange(SID, fVN) %>%
    dplyr::group_by(SID) %>%
    tidyr::fill(udbpBase, ageBase) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(SID, VN)
}

#' Prepare the project data for analysis through GEE. The output is a dataframe
#' with baseline values. Subjects with macroalbuminuria and moderate kidney
#' disease at baseline is omitted.
#'
#' @param data project data
#' @export
prep_mason_data_kidney <- function(data) {
  data %>%
    dplyr::mutate(
      UDBP = UDBP/1000, # udbp units to ug/mL
      udbpBase = ifelse(fVN == "Baseline", UDBP, NA),
      ageBase = ifelse(fVN == "Baseline", Age, NA),
      DM = ifelse(DM == 1, "diabetes", "non_dia"),
      fDM = relevel(as.factor(DM), "non_dia"),
      PreDM = ifelse(dmStatus == "PreDM", "PreDM", "nPreDm"),
      fPreDM = relevel(as.factor(PreDM), "nPreDM"),
      Ethnicity = ifelse(Ethnicity == "European", Ethnicity, "Other"),
      Ethnicity = relevel(as.factor(Ethnicity), "Other")
    ) %>%
    dplyr::filter(!(fVN == "Baseline" &
                      acrStatus == "Macroalbuminuria")) %>%
    dplyr::filter(!(fVN == "Baseline" & eGFRStatus == "Moderate")) %>%
    dplyr::filter(!(fVN == "Baseline" & dmStatus == "DM")) %>%
    dplyr::arrange(SID, fVN) %>%
    dplyr::group_by(SID) %>%
    tidyr::fill(udbpBase, ageBase) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(SID, VN)
}



#' Prepare the project data for analysis through GEE. The output is a dataframe
#' with baseline values. Subjects with deficient vitamin D status at baseline
#' is omitted.
#'
#' @param data
#'
#' @export
prep_mason_data_vitd <- function(data) {
  data %>%
    dplyr::mutate(
      UDBP = UDBP/1000, # udbp units to ug/mL
      udbpBase = ifelse(fVN == "Baseline", UDBP, NA),
      ageBase = ifelse(fVN == "Baseline", Age, NA),
      DM = ifelse(DM == 1, "diabetes", "non_dia"),
      fDM = relevel(as.factor(DM), "non_dia"),
      Ethnicity = ifelse(Ethnicity == "European", Ethnicity, "Other"),
      Ethnicity = relevel(as.factor(Ethnicity), "Other")
    ) %>%
    dplyr::filter(!(fVN == "Baseline" &
                      vitdStatus == "Deficient")) %>%
    dplyr::arrange(SID, fVN) %>%
    dplyr::group_by(SID) %>%
    tidyr::fill(udbpBase, ageBase) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(SID, VN)
}

# Analyze -----------------------------------------------------------------

#' Run GEE on prepared project data
#'
#' @param data Cleaned data ready for GEE analysis
#' @export
#'
#' @examples
mason_gee <- function(data = project_data,
                        y = outcomes,
                        x = predictors,
                        covars = covariates,
                        intvar = NULL) {

  int <- !is.null(intvar)
  if (int) {
    extract_term <- ":"
  } else {
    extract_term <- "Xterm$"
  }

  data %>%
    mason::design("gee") %>%
    mason:::add_settings(family = stats::gaussian(),
                         corstr = "ar1", cluster.id = "SID") %>%
    mason::add_variables("yvars", y) %>%
    mason::add_variables("xvars", x) %>%
    mason::add_variables("covariates", covars) %>% {
      if (int) {
        mason::add_variables(., "interaction", intvar)
      } else {
        .
      }
    } %>%
    mason::construct() %>%
    mason::scrub()
  # %>%
  #   mason::polish_filter(extract_term, "term")
}


explore_gee <- function(data = project_data,
                        covars = covariates, caption = NULL, graph = FALSE) {
  m <- analyze_gee(data = data, covars = covars)
  m <- filter(m, p.value <= 0.05)
  print(nrow(m))
  if (graph) {
    print(caption)
    plot_gee_main(m)
  } else {
    table_gee_main(m, caption = caption)
  }
}


# Plotting ----------------------------------------------------------------

#' Plot GEE results in a forest plot-style
#'
#' @param results Results of GEE analysis using mason package
#'
#' @export
#'
#' @examples
plot_gee_results_kidney <- function(results, yvars,
                     xlab = "Unit difference with 95% CI in outcome for every unit increase in uVDBP and covariates") {
  results %>%
    dplyr::mutate(Xterms = term) %>%
    dplyr::filter(!term == "(Intercept)") %>%
    dplyr::mutate(Yterms = factor(Yterms,
                                  levels = yvars,
                                  ordered = TRUE),
                  Xterms = factor(Xterms,
                                  levels = rev(c("<-Xterm",
                                                 "VN",
                                                 "ageBase",
                                                 "SexMale",
                                                 "EthnicityEuropean",
                                                 "BMI",
                                                 "fDMdiabetes")),
                                  labels = rev(c("Baseline uVDBP (ug/mL)",
                                                 "Follow-up Duration (Years)",
                                                 "Baseline Age (Years)",
                                                 "Sex (male)",
                                                 "Ethnicity (European",
                                                 "BMI (kg/m^2)",
                                                 "Diabetes")),
                                  ordered = TRUE)) %>%
    arrange(Xterms) %>%
    gee_plot(xlab = xlab)
}





#' Plot vitamin D GEE results in a forest plot-style
#'
#' @param results
#' @param yvars
#' @param xlab
#'
#' @return
#' @export
#'
#' @examples
plot_gee_results_vitd <- function(results, yvars,
                                    xlab = "Unit difference with 95% CI in outcome for every unit increase in uVDBP and covariates") {
  results %>%
    dplyr::mutate(Xterms = term) %>%
    dplyr::filter(!term == "(Intercept)") %>%
    dplyr::mutate(Yterms = factor(Yterms,
                                  levels = yvars,
                                  ordered = TRUE),
                  Xterms = factor(Xterms,
                                  levels = rev(c("<-Xterm",
                                                 "VN",
                                                 "ageBase",
                                                 "SexMale",
                                                 "EthnicityEuropean",
                                                 "BMI",
                                                 "PTH",
                                                 "fDMdiabetes")),
                                  labels = rev(c("Baseline uVDBP (ug/mL)",
                                                 "Follow-up Duration (Years)",
                                                 "Baseline Age (Years)",
                                                 "Sex (male)",
                                                 "Ethnicity (European",
                                                 "BMI (kg/m^2)",
                                                 "Parathyroid Hormone (pmol/L)",
                                                 "Diabetes")),
                                  ordered = TRUE)) %>%
    arrange(Xterms) %>%
    gee_plot(xlab = xlab)
}
