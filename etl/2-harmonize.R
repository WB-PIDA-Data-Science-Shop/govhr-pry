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

plan(multisession, workers = 4)

years |>
  furrr::future_walk(
    harmonize_hr
  )

# combine ----------------------------------------------------------------
pry_est <- fs::dir_ls(
  here("data", "data-clean", "full"),
  regexp = "pry_est_\\d{4}\\.csv\\.gz$"
) |>
  purrr::map_dfr(
    readr::read_csv,
    col_types = readr::cols(
      .default = readr::col_character()
    )
  ) |>
  mutate(
    country_code = "PRY",
    country_name = "Paraguay"
  )

pry_personnel <- fs::dir_ls(
  here("data", "data-clean", "full"),
  regexp = "pry_personnel_\\d{4}\\.csv\\.gz$"
) |>
  purrr::map_dfr(
    readr::read_csv,
    col_types = readr::cols(
      .default = readr::col_character()
    )
  )

pry_contracts <- fs::dir_ls(
  here("data", "data-clean", "full"),
  regexp = "pry_contracts_\\d{4}\\.csv\\.gz$"
) |>
  purrr::map_dfr(
    readr::read_csv,
    col_types = readr::cols(
      .default = readr::col_character()
    )
  )

# load to duckdb ---------------------------------------------------------
con <- DBI::dbConnect(
  duckdb::duckdb(),
  here("data", "data-clean", "full", "govhr_pry.duckdb")
)

DBI::dbWriteTable(con, "pry_est", pry_est, overwrite = TRUE)
DBI::dbWriteTable(con, "pry_personnel", pry_personnel, overwrite = TRUE)
DBI::dbWriteTable(con, "pry_contracts", pry_contracts, overwrite = TRUE)
