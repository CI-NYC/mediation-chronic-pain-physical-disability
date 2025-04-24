# READ ME -----------------------------------------------------------------
#
#       Author: Kat Hoffman (Updated by Nick)
# Last updated: 26 January 2024
# 
# -------------------------------------------------------------------------

library(tidyverse)
library(fuzzyjoin)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

opioids <- readRDS(file.path(drv_root, "opioids_with_strength.rds"))

mme_conversion <- tibble::tribble(
    ~opioid, ~conversion,
    # From CDC
    "codeine", 	0.15,
    "hydrocodone",	1.0,
    "hydromorphone",	5.0,
    "methadone",	4.7,
    "morphine",	1.0,
    "oxycodone",	1.5,
    "oxymorphone",	3.0,
    "tapentadol",	0.4,
    "tramadol",	0.2,
    # From https://www.ohiopmp.gov/Documents/MorphineEquivalentDailyDoseConversionTable.pdf
    "pentazocine", 0.37,
    "opium", 1,
    "meperidine", 0.1,
    "levorphanol", 11,
    "levomethadyl", 8,
    "dihydrocodeine", 0.25,
    "butorphanol", 7,
    # From https://www.accessdata.fda.gov/drugsatfda_docs/label/2016/018024s041lbl.pdf 
    # "Its analgesic potency is essentially equivalent to that of morphine on a  milligram basis up 
    # to a dosage of approximately 30 mg.
    "nalbuphine", 1,
    #  From WA.gov morphine milligram equivalent calculator excel spreadsheet
    "propoxyphene", 0.23,
    # Buprenorphine and Fentanyl have different conversion factors based on form type
    "buprenorphine", NA,
    "fentanyl",	NA, 
    # Remifentanil, alfentanil, and sufentanil are fentanyl derivatives
    "remifentanil", NA,
    "alfentanil", NA,
    "sufentanil", NA,
)

ci_str_detect <- function(x, y) str_detect(x, regex(y, ignore_case = TRUE))

opioids_mme <- 
    filter(opioids, flag_opioid_analgesic == TRUE | flag_opioid_anesthetics == TRUE) |> 
    unnest(cols = "strength") |> 
    fuzzy_inner_join(mme_conversion, 
                     by = c("activeIngredientName" = "opioid"), 
                     match_fun = ci_str_detect)

# Listed as codeine and dihydrocodeine, but really just dihydrocodeine
remove_codeine <- c("42195084010", "57664041988", "66992084010", "55887045690")

opioids_mme <- filter(opioids_mme, !(NDC %in% remove_codeine & opioid == "codeine"))

# Should only be drugs used for MOUD
no_mme <- anti_join(opioids, opioids_mme, by = "NDC")
unnest(no_mme, cols = "strength") |> 
    select(activeIngredientName) |> 
    unique()

opioids_mme <- opioids_mme |> 
    mutate(
        conversion = case_when(
            str_detect(activeIngredientName, "buprenorphine") &
                str_detect(dose_form, "Film|Tablet") ~ 30,
            str_detect(activeIngredientName, "buprenorphine") &
                str_detect(dose_form, "Trans") ~ 12.6,
            str_detect(activeIngredientName, "buprenorphine") &
                str_detect(dose_form, "Implant|Cartridge|Syringe|Inject") ~ 75,
            str_detect(activeIngredientName, "fentanyl|remifentanil|alfentanil|sufentanil") & 
                str_detect(dose_form, "Lozenge|Tablet") ~ 130,
            str_detect(activeIngredientName, "fentanyl|remifentanil|alfentanil|sufentanil") & 
                str_detect(dose_form, "Spray") ~ 160,
            str_detect(activeIngredientName, "fentanyl|remifentanil|alfentanil|sufentanil") & 
                str_detect(dose_form, "Transdermal") ~ 720,
            str_detect(activeIngredientName, "fentanyl|remifentanil|alfentanil|sufentanil") & 
                str_detect(dose_form, "Syringe|Injection|Cartridge|Pack") ~ 100,
            TRUE ~ conversion
        )
    )

saveRDS(opioids_mme, file.path(drv_root, "opioids_mme.rds"))
