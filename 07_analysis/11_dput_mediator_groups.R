# -------------------------------------
# Script: 14_dput_mediator_groups.R
# Author: Nick Williams
# Purpose:
# Notes:
# -------------------------------------

library(purrr)
library(stringr)
library(fs)
library(dplyr)

root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_results_final_r1/"

directory <- path(root, "nodepanx_icd")

res <- list.files(directory, full.names = TRUE)

map(res, \(x) readRDS(x)[[1]]) |> 
    list_rbind(names_to = "mediator") |> 
    round(4) -> values

mediator <- str_extract(res, "(?<=mediator_).*?(?=_icd_results)")
exposure <- str_extract(res, "(?<=results_exposure_).*?(?=_nodepanx)")

values$mediator <- mediator
values$exposure <- exposure

select(values, mediator, exposure, 
       ate, ci_ate_low, ci_ate_high, 
       total, ci_total_low, ci_total_high, 
       direct, ci_direct_low, ci_direct_high, 
       indirect, ci_indirect_low, ci_indirect_high) |> 
    dput()
