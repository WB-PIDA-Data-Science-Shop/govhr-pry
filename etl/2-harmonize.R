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

# reset the duckdb database, so every run rebuilds it from scratch
# (harmonize_hr() appends each year to the module tables)
db_path <- here("data", "data-clean", "govhr_pry.duckdb")

fs::file_delete(
  fs::path(c(db_path, paste0(db_path, ".wal"))) |>
    purrr::keep(fs::file_exists)
)

years |>
  purrr::walk(
    harmonize_hr,
    files = funcionarios_files,
    db_path = db_path
  )

  
# set primary keys -------------------------------------------------------
con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = db_path,
  read_only = FALSE
)

DBI::dbExecute(
  con,
  "ALTER TABLE pry_est ADD PRIMARY KEY (est_id, ref_date)"
)

DBI::dbExecute(
  con,
  "ALTER TABLE pry_personnel ADD PRIMARY KEY (personnel_id, ref_date)"
)

DBI::dbExecute(
  con,
  "ALTER TABLE pry_contracts ADD PRIMARY KEY (contract_id, ref_date)"
)
