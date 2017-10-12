
#' Renames variables in data set to improve understanding and include units
#'
#' @param x

#' @export
#'
#' @examples
rename_variables <- function(x) {
  x %>%
    gsub("VN", "Follow-up Duration (years)", .) %>%
    gsub("MonthsFromBaseline", "Follow-up Duration (months)", .) %>%
    gsub('Age', 'Age (years)', .) %>%
    gsub("ageBase", "Baseline Age (years)", .) %>%
    gsub("BMI", "BMI (kg/m^2)", .) %>%
    gsub('Waist', 'Waist Circumference (cm)', .) %>%
    gsub('eGFR', 'Estimated GFR (ml/min/1.73m^2)', .) %>%
    gsub('ACR', 'ACR (mg/mmol)', .) %>%
    gsub('MicroalbCreatRatio', 'Microalbumin:Creatinine', .) %>%
    gsub('UrineCreatinine', 'Urinary Creatinine (mmol/L)', .) %>%
    gsub('UrineMicroalbumin', 'Urinary Microalbumin (mg/L)', .) %>%
    gsub('VitaminD', 'Serum 25(OH)D (nmol/L)', .) %>%
    gsub('UDBP', 'Urinary VDBP (ng/mL)', .) %>%
    gsub("udbpBase", "Baseline uVDBP (μg/mL)", .) %>%
    gsub('udbpCr', 'uVDBP:creatinine (μg/mmol)', .) %>%
    gsub('Diastolic', 'Diastolic Blood Pressure (mmHg)', .) %>%
    gsub('MeanArtPressure', 'Mean Arterial Pressure (mmHg)', .) %>%
    gsub('Systolic', 'Systolic Blood Pressure (mmHg)', .) %>%
    gsub('PTH', 'Parathyroid Hormone (pmol/L)', .) %>%
    gsub('ALT', 'Serum ALT (U/L)', .) %>%
    gsub('Glucose0', 'Fasting', .) %>%
    gsub('Glucose120', '2h OGTT', .) %>%
    gsub('dm_status', 'Diabetic Status', .) %>%
    gsub('DM', 'Diabetes', .) %>%
    gsub('NGT', 'Normal Glucose Tolerance', .)
}


#' Renames variables in kidney measures GEE results
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
rename_gee_kidney <- function(x) {
  x %>%
    gsub("<-Xterm", "uVDBP:cr (ug/mmol)", .) %>%
    gsub("VN", "Follow-up Duration (years)", .) %>%
    gsub("MonthsFromBaseline", "Follow-up Duration (months)", .) %>%
    gsub('Age', 'Age (years)', .) %>%
    gsub("ageBase", "Baseline Age (years)", .) %>%
    gsub('leGFR', 'eGFR (ml/min/1.73m^2)', .) %>%
    gsub('lACR', 'ACR (mg/mmol)', .) %>%
    gsub('UDBP', 'Urinary VDBP (ng/mL)', .) %>%
    gsub("udbpCrBase", "Baseline uVDBP:cr (ug/mmol)", .) %>%
    gsub('udbpCrRatio', 'uVDBP:creatinine (ug/mmol)', .) %>%
    gsub("fPreDMPreDM", "Prediabetes", .) %>%
    gsub('DM', 'Diabetes', .) %>%
    gsub('NGT', 'Normal Glucose Tolerance', .) %>%
    gsub("fDysglycemiaDysglycemia", "Dysglycemia", .)
}


#' Renames variables in vitamin D GEE results
#'s
#' @param x
#'
#' @return
#' @export
#'
#' @examples
rename_gee_vitd <- function(x) {
  x %>%
    gsub("<-Xterm", "uVDBP:cr (ug/mmol)", .) %>%
    gsub("VitaminD", "Serum 25(OH)D (nmol/L)", .) %>%
    gsub("VN", "Follow-up Duration (years)", .) %>%
    gsub("MonthsFromBaseline", "Follow-up Duration (months)", .) %>%
    gsub('Age', 'Age (years)', .) %>%
    gsub("ageBase", "Baseline Age (years)", .) %>%
    gsub("BMI", "BMI (kg/m^2)", .) %>%
    gsub('UDBP', 'Urinary VDBP (ng/mL)', .) %>%
    gsub("udbpCrBase", "Baseline uVDBP:cr (ug/mmol)", .) %>%
    gsub('udbpCrRatio', 'uVDBP:cr (ug/mmol)', .) %>%
    gsub("fPreDMPreDM", "Prediabetes", .) %>%
    gsub('DM', 'Diabetes', .) %>%
    gsub('NGT', 'Normal Glucose Tolerance', .) %>%
    gsub("fDysglycemiaDysglycemia", "Dysglycemia", .) %>%
    gsub("MET", "MET (kcal/kg/h)", .) %>%
    gsub("lVitD", "Serum 25(OH)D (nmol/L)", .) %>%
    gsub("lPTH", "PTH (pmol/L)", .)
}
