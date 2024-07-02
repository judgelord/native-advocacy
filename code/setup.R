options(stringsAsFactors = FALSE)

requires <- c("magrittr",
              "here",
              "tidyverse",
              "kableExtra",
              "googledrive",
              "googlesheets4")
to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )
rm(requires, to_install)


# basic
library(magrittr)
library(here)
library(tidyverse)

# Plots
library(ggplot2); theme_set(theme_bw())
options(
  ggplot2.continuous.color = "viridis",
  ggplot2.continuous.fill = "viridis"
)
scale_color_discrete <- function(...)
  scale_color_viridis_d(...)
scale_fill_discrete <- function(...)
  scale_fill_viridis_d(...)

library(scales)

# api
library(googledrive)
library(googlesheets4)

# knit defaults
knitr::opts_chunk$set(echo = TRUE,
                      cache = FALSE,
                      fig.width=8.5, fig.align = 'center', fig.path='Figs/',
                      warning=FALSE, message=FALSE)



#functions for case sensitive string manipulation
str_rm_all <- function(string, pattern) {
  str_remove_all(string, regex(pattern, ignore_case = TRUE))
}

str_rpl <- function(string, pattern, replacement) {
  str_replace(string, regex(pattern, ignore_case = TRUE), replacement)
}

str_rm <- function(string, pattern) {
  str_remove(string, regex(pattern, ignore_case = TRUE))
}

str_dct <- function(string, pattern) {
  str_detect(string, regex(pattern, ignore_case = TRUE))
}

str_ext <- function(string, pattern) {
  str_extract(string, regex(pattern, ignore_case = TRUE))
}

str_spl <- function(string, pattern) {
  str_split(string, regex(pattern, ignore_case = TRUE))
}



# rename regulations.gov
namingthings <- function(x){
  names(x)  <- names(x) %>%
    str_replace_all("([A-Z])", "_\\1") %>%
    str_to_lower() %>%
    # rename old data for new API results
    str_replace("agency_acronym", "agency_id") %>%
    str_replace("document_id", "id")


  x %<>% mutate(across(where(is.factor), as.character))

  # x$allow_late_comment %<>% as.logical()
  # x$attachment_count %<>% as.integer() #TODO get this from metadata
  # x$number_of_comments_received %<>% as.integer()
  # x$open_for_comment <- NA %>% as.logical()
  #x$posted_date %<>% as.Date()

  return(x)
}


# Table formatting
library(kableExtra)
kablebox <- . %>%
  slice_head(n = 100) %>%
  knitr::kable() %>%
  kable_styling() %>%
  scroll_box(height = "400px")

