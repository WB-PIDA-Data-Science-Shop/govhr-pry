# set-up -----------------------------------------------------------------
library(dplyr)
library(readr)
library(govhr)
library(furrr)
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

# reset the duckdb database, so every run rebuilds it from scratch
# (harmonize_hr() appends each year to the module tables)
db_path <- here("data", "data-clean", "govhr_pry_test.duckdb")

fs::file_delete(
  fs::path(c(db_path, paste0(db_path, ".wal"))) |>
    purrr::keep(fs::file_exists)
)

plan(multisession, workers = 4)

years |>
  furrr::future_walk(
    harmonize_hr,
    db_path = db_path
  )

plan(sequential)
