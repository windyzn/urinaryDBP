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
      udbpCrBase = ifelse(fVN == "Baseline", udbpCr, NA),
      ageBase = ifelse(fVN == "Baseline", Age, NA),
      DM = ifelse(DM == 1, "DM", "notDM"),
      fDM = relevel(as.factor(DM), "notDM"),
      fDysglycemia = ifelse(!(dmStatus == "NGT"), "Dysglycemia", "notDysglycemia"),
      Ethnicity = ifelse(Ethnicity == "European", Ethnicity, "Other"),
      Ethnicity = relevel(as.factor(Ethnicity), "Other"),
      dmStatus = factor(dmStatus, ordered = FALSE)
    ) %>%
    dplyr::arrange(SID, fVN) %>%
    dplyr::group_by(SID) %>%
    tidyr::fill(udbpCrBase, ageBase) %>%
    # dplyr::mutate_at(dplyr::vars(-SID, -fVN), dplyr::funs(as.numeric(scale(.)))) %>%
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
      udbpBase = ifelse(fVN == "Baseline", UDBP, NA),
      udbpCrBase = ifelse(fVN == "Baseline", udbpCr, NA),
      ageBase = ifelse(fVN == "Baseline", Age, NA),
      DM = ifelse(DM == 1, "DM", "notDM"),
      fDM = relevel(as.factor(DM), "notDM"),
      dmStatus = factor(dmStatus, ordered = FALSE),
      Ethnicity = ifelse(Ethnicity == "European", Ethnicity, "Other"),
      Ethnicity = relevel(as.factor(Ethnicity), "Other"),

      lACR = log(ACR),
      leGFR = log(eGFR)
    ) %>%

    dplyr::filter(!(fVN == "Baseline" &
                      acrStatus == "Macroalbuminuria")) %>%
    dplyr::filter(!(fVN == "Baseline" & eGFRStatus == "Moderate")) %>%
    dplyr::filter(!(fVN == "Baseline" & dmStatus == "DM")) %>%

    dplyr::mutate_each(dplyr::funs(as.numeric(scale(.))),
                       UDBP,
                       udbpBase,
                       udbpCr,
                       udbpCrBase,
                       MonthsFromBaseline,
                       ageBase) %>%

    dplyr::arrange(SID, fVN) %>%
    dplyr::group_by(SID) %>%
    tidyr::fill(udbpBase, ageBase) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(SID, fVN)
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
      # UDBP = UDBP/1000, # udbp units to ug/mL
      udbpBase = ifelse(fVN == "Baseline", UDBP, NA),
      udbpCrBase = ifelse(fVN == "Baseline", udbpCr, NA),
      ageBase = ifelse(fVN == "Baseline", Age, NA),
      DM = ifelse(DM == 1, "DM", "notDM"),
      fDM = relevel(as.factor(DM), "notDM"),
      Ethnicity = ifelse(Ethnicity == "European", Ethnicity, "Other"),
      Ethnicity = relevel(as.factor(Ethnicity), "Other"),
      dmStatus = factor(dmStatus, ordered = FALSE),

      lVitD = log(VitaminD)
      # lPTH = log(PTH)
    ) %>%

    # dplyr::filter(!(fVN == "Baseline" & vitdStatus == "Deficient")) %>%
    dplyr::filter(!(fVN == "Baseline" & dmStatus == "DM")) %>%

    dplyr::mutate_each(dplyr::funs(as.numeric(scale(.))),
                       UDBP,
                       udbpBase,
                       udbpCr,
                       udbpCrBase,
                       MonthsFromBaseline,
                       ageBase,
                       MET,
                       BMI) %>%

    dplyr::arrange(SID, fVN) %>%
    dplyr::group_by(SID) %>%
    tidyr::fill(udbpBase, ageBase) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(SID, fVN)
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
                        covars = NULL,
                        intvar = NULL) {

  int <- !is.null(intvar)
  if (int) {
    extract_term <- ":"
  } else {
    extract_term <- "Xterm$"
  }

  co <- !is.null(covars)

  data %>%
    mason::design("gee") %>%
    mason:::add_settings(family = stats::gaussian(),
                         corstr = "ar1", cluster.id = "SID") %>%
    mason::add_variables("yvars", y) %>%
    mason::add_variables("xvars", x) %>%
    mason::construct() %>% {
      if (co) {
        mason::add_variables(., "covariates", covars) %>%
        mason::construct() %>% {
          if (int) {
            mason::add_variables(., "interaction", intvar) %>%
            mason::construct()
          } else {
            .
          }
        }
      } else {
        .
      }
    } %>%
    mason::scrub()
    # mason::polish_transform_estimates(function(x)
    #   (exp(x) - 1) * 100) <- only if log transformed y var (so you can
    #   intepret them as % increase)
}


#' Run GEE on prepared project data simplified for graphical purposes (plot_gee_results)
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
mason_geeplot <- function(data = project_data,
                      y = outcomes,
                      x = predictors,
                      covars = NULL,
                      intvar = NULL) {

  int <- !is.null(intvar)
  if (int) {
    extract_term <- ":"
  } else {
    extract_term <- "Xterm$"
  }

  co <- !is.null(covars)

  data %>%
    mason::design("gee") %>%
    mason:::add_settings(family = stats::gaussian(),
                         corstr = "ar1", cluster.id = "SID") %>%
    mason::add_variables("yvars", y) %>%
    mason::add_variables("xvars", x) %>% {
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
    mason::scrub() %>%
    mason::polish_transform_estimates(function(x) (exp(x) - 1) * 100)
}


# Table -------------------------------------------------------------------

#' Display GEE results in a table. Use pander::pander() to print as a table when knitting document.
#'
#' @param results
#'
#' @return
#'
#' @examples
gee_results_table <- function(results, table = TRUE) {
  results %>%
    dplyr::filter(!term == "(Intercept)") %>%
    dplyr::mutate(p = round(p.value, 2),
                  p = ifelse(p == "0", "<0.001", p),
                  estCI = paste0(round(estimate, 2), " (",
                                 round(conf.low, 2), ", ",
                                 round(conf.high, 2), ")")) %>%
    dplyr::select(Yterms, Xterms, term, estCI, p)
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
                     xlab = "Percent difference with 95% CI in the outcomes
                     for each SD increase in uVDBP and covariates",
                     terms = c("Baseline uVDBP (ug/mmol)",
                               "Follow-up Duration (months)",
                               "Baseline Age (years)",
                               "SexMale",
                               "EthnicityEuropean",
                               "dmStatusPreDiabetes",
                               "dmStatusDiabetes"),
                     labels = c("Baseline uVDBP (ug/mL)",
                                "Follow-up Duration (Months)",
                                "Baseline Age (Years)",
                                "Sex (male)",
                                "Ethnicity (European)",
                                "Prediabetes",
                                "Diabetes")) {
  results %>%
    dplyr::mutate(Xterms = term) %>%
    dplyr::filter(!term == "(Intercept)") %>%
    dplyr::mutate(Yterms = factor(Yterms,
                                  levels = yvars,
                                  ordered = TRUE),
                  Xterms = factor(Xterms,
                                  levels = rev(terms),
                                  labels = rev(labels),
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
                                    xlab = "Percent difference with 95% CI in the outcomes for\n
                                  each SD increase in uVDBP and covariates") {
  results %>%
    dplyr::mutate(Xterms = term) %>%
    dplyr::filter(!term == "(Intercept)") %>%
    dplyr::mutate(Yterms = factor(Yterms,
                                  levels = yvars,
                                  ordered = TRUE),
                  Xterms = factor(Xterms,
                                  levels = rev(c("uVDBP:cr (ug/mmol)",
                                                 "Follow-up Duration (months)",
                                                 "Baseline Age (years)",
                                                 "SexMale",
                                                 "EthnicityEuropean",
                                                 "MET (kcal/kg/h)",
                                                 "BMI (kg/m^2)",
                                                 "dmStatusPreDiabetes",
                                                 "dmStatusDiabetes",
                                                 "SeasonWinter",
                                                 "uVDBP:cr (ug/mmol):SeasonWinter")),
                                  labels = rev(c("uVDBP:cr (ug/mL)",
                                                 "Follow-up Duration (months)",
                                                 "Baseline Age (years)",
                                                 "Sex (male)",
                                                 "Ethnicity (European)",
                                                 "MET (kcal/kg/h)",
                                                 "BMI (kg/m^2)",
                                                 "Prediabetes",
                                                 "Diabetes",
                                                 "Seasonality (winter)",
                                                 "Season:Baseline uVDBP:cr interaction")),
                                  ordered = TRUE)) %>%
    arrange(Xterms) %>%
    gee_plot(xlab = xlab)
}
