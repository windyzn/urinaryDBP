#' Fetch data from the original source
#'
#' This function fetchs the main dataset, keeps variables relevant to
#' the analysis, restrict the sample size as needed, and lastly save
#' the new dataset as an `.RData` file.
#'
#' @return Saves the wrangled data into the data/ folder.
#' @export
#'
#' @examples
#' fetch_data()
#'
fetch_data <- function() {
  # Load the master dataset,
  ds.prep <- PROMISE::PROMISE %>%

  # Data wrangling commands
    dplyr::mutate(
      UDBP = ifelse(UDBP == 0.01, NA, UDBP),
      UrineCreatinine = ifelse(SID == 2028, 9, UrineCreatinine),
      ACR = round(UrineMicroalbumin / UrineCreatinine, digits = 2),
      Ethnicity = as.character(Ethnicity),
      isAfrican = ifelse(Ethnicity == 'African', 1, 0),
      Ethnicity = ifelse(
        Ethnicity %in% c('African', 'First Nations', 'Other'),
        'Other',
        Ethnicity
      ),
      fVN = factor(
        VN,
        levels = c(1, 3, 6),
        labels = c("Baseline", "3Year", "6Year"),
        ordered = TRUE
      ),
      fMedsBP = factor(
        MedsBloodPressure,
        levels = c(0, 1),
        labels = c("No", "Yes"),
        ordered = TRUE
      ),
      dmStatus = ifelse(DM == 1, "DM",
                         ifelse(IFG == 1 |
                                  IGT == 1, "PreDM",
                                "NGT")),
      dmStatus = factor(dmStatus,
                        levels = c("NGT", "PreDM", "DM"),
                        ordered = TRUE),
      acrStatus = ifelse(
        ACR < 2,
        'Normoalbuminuria',
        ifelse(ACR > 20, 'Macroalbuminuria',
               "Microalbuminuria")
      ),
      acrStatus = factor(
        acrStatus,
        levels = c("Normoalbuminuria", "Microalbuminuria", "Macroalbuminuria"),
        ordered = TRUE
      ),
      creat.mgdl = Creatinine * 0.011312,
      eGFR = nephro::CKDEpi.creat(creat.mgdl, as.numeric(Sex) -
                                    1, Age, isAfrican),
      eGFRStatus = cut(eGFR,
                       breaks = c(-Inf, 60, 90, Inf),
                       labels = c("Moderate", "Mild", "Normal")),
      eGFRStatus = factor(eGFRStatus,
                          levels = c("Normal", "Mild", "Moderate"),
                          ordered = TRUE),
      UDBPStatus = cut(UDBP,
                       breaks = c(0, 1.23, 60, Inf),
                       labels = c("Trace", "Normal", "High"),
                       ordered_result = TRUE),
      udbpCrRatio = UDBP / UrineCreatinine,
      logUDBPRatio = log(udbpCrRatio)
    ) %>%
    dplyr::filter(UDBP < 10000) %>%
    dplyr::filter(eGFR < 200) %>%
    dplyr::filter(Creatinine < 200) %>%
    dplyr::select(
      SID,
      BMI,
      Waist,
      Age,
      Sex,
      Ethnicity,
      VN,
      fVN,
      Glucose0,
      Glucose120,
      DM,
      IFG,
      IGT,
      dmStatus,
      acrStatus,
      eGFRStatus,
      UDBPStatus,
      eGFR,
      ACR,
      UrineMicroalbumin,
      UrineCreatinine,
      Creatinine,
      UDBP,
      udbpCrRatio,
      VitaminD,
      MeanArtPressure,
      Systolic,
      Diastolic,
      PTH,
      ALT,
      UrinaryCalcium,
      fMedsBP,
      SmokeCigs,
      Canoe,
      BirthControl,
      PeriodsStopped,
      PeriodsStoppedAge,
      dplyr::matches("meds")
    )

  # Final dataset object
  project_data <- ds.prep

  # Save the dataset to the data/ folder.
  devtools::use_data(project_data, overwrite = TRUE)
}
