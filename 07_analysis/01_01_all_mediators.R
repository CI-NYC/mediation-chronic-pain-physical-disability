# -------------------------------------
# Script: 01_01_all_mediators.R
# Author: Nick Williams
# Purpose: Run 01_00_mediators.R using `callr`
# Notes:
# -------------------------------------

library(callr)

script <- "projects/mediation_unsafe_pain_mgmt/07_analysis/01_00_all_mediators.R"

dp <- rscript_process$new(
    rscript_process_options(
        script = script, 
        cmdargs = "exposure_disability_pain")
)

d <- rscript_process$new(
    rscript_process_options(
        script = script, 
        cmdargs = "exposure_disability_only")
)

p <- rscript_process$new(
    rscript_process_options(
        script = script, 
        cmdargs = "exposure_pain_only")
)

dp$get_status()
d$get_status()
p$get_status()
