# set-up -----------------------------------------------------------------
library(dplyr)
library(readr)
library(govhr)
library(here)

source(
  here("R", "parse_whours.R")
)

source(
  here("R", "harmonize.R")
)

# read-in ----------------------------------------------------------------
funcionarios_files <- fs::dir_ls(
  "data/data-raw",
  regexp = "funcionarios_\\d{4}_\\d{1,2}\\.csv\\.gz$"
)

# extract years
years <- funcionarios_files |>
  fs::path_file() |>
  stringr::str_extract("\\d{4}") |>
  as.integer() |>
  unique()

years[1] |>
  purrr::walk(
    harmonize_hr
  )
