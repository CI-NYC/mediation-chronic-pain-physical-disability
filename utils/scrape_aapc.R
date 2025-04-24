# READ ME -----------------------------------------------------------------
#
#       Author: Nick Williams
# Last updated: 6 July 2023
#
# Scrape codes and code definitions from https://www.aapc.com/codes/
# 
# -------------------------------------------------------------------------

library(tidyverse)
library(rvest)
library(httr)
library(glue)
library(yaml)
library(clipr)
library(corpus)

read_cpt_from_page <- function(code_range, 
                               base = "https://www.aapc.com/codes/cpt-codes-range/") {
    page <- read_html(paste0(base, code_range))
    page |>   
        html_elements(".list-code-range a") |> 
        html_text() |> 
        str_extract("\\d+") |> 
        as.numeric()
}

select_cpt_definition <- function(code, 
                                  base = "https://www.aapc.com/codes/cpt-codes/", 
                                  element = "h1") {
    page <- read_html(paste0(base, code))
    page |> 
        html_elements(element) |> 
        html_text() |> 
        str_remove(glue("CPT®{code},")) |> 
        str_trim()
}

select_hcpcs_defintion <- function(code) {
    base <- "https://www.aapc.com/codes/hcpcs-modifiers/"
    def <- read_html(paste0(base, code)) |> 
        html_elements("h1") |> 
        html_text() |> 
        str_remove(glue("HCPCS  Code for  ")) |> 
        str_remove(code) |> 
        str_trim()
    write_clip(def)
    invisible(def)
}

# usage
# cpt <- read_cpt_from_page("64400-64489")
cpt <- c(...)

definitions <- map_chr(cpt, select_cpt_definition, element = "strong+ p")
names(definitions) <- cpt

definitions <- map_chr(definitions, function(x) {
    text <- text_split(x, "sentences")
    str_trim(text$text[[1]])
})

data.frame(as.list(definitions), check.names = F) |> 
    as.yaml() |> 
    write_clip()
