
# Renaming ----------------------------------------------------------------

#' Renames variables in data set to improve understanding and include units
#'
#' @param x

#' @export
#'
#' @examples
rename_table_rows <- function(x) {
  gsub('Age', 'Age (years)', x) %>%
    gsub('Waist', 'Waist Circumference (cm)', .) %>%
    gsub('eGFR', 'Estimated GFR (ml/min/1.73m^2)', .) %>%
    gsub('ACR', 'Urinary Albumin-to-Creatinine Ratio (mg/mmol)', .) %>%
    gsub('MicroalbCreatRatio', 'Microalbumin:Creatinine', .) %>%
    gsub('UrineCreatinine', 'Urinary Creatinine (mmol/L)', .) %>%
    gsub('UrineMicroalbumin', 'Urinary Microalbumin (mg/L)', .) %>%
    gsub('VitaminD', 'Serum 25(OH)D (nmol/L)', .) %>%
    gsub('UDBP', 'Urinary VDBP (ng/mL)', .) %>%
    gsub('Diastolic', 'Diastolic Blood Pressure (mmHg)', .) %>%
    gsub('MeanArtPressure', 'Mean Arterial Pressure (mmHg)', .) %>%
    gsub('Systolic', 'Systolic Blood Pressure (mmHg)', .) %>%
    gsub('PTH', 'Parathyroid Hormone (pmol/L)', .) %>%
    gsub('ALT', 'Serum ALT (U/L)', .) %>%
    gsub('Glucose0', 'Fasting', .) %>%
    gsub('Glucose120', '2h OGTT', .) %>%
    gsub('dm_status', 'Diabetic Status', .) %>%
    gsub('DM', 'Diabetes', .) %>%
    gsub('NGT', 'Normal', .)
}
