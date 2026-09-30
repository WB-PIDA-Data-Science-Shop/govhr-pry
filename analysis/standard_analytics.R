# set-up -----------------------------------------------------------------
library(dplyr)
library(duckplyr)
library(ggplot2)
library(govhr)

# read-in ----------------------------------------------------------------
con <- DBI::dbConnect(
  duckdb::duckdb(),
  here::here("data", "data-clean", "govhr_pry.duckdb")
)

pry_est <- tbl(con, "pry_est")
pry_personnel <- tbl(con, "pry_personnel")
pry_contracts <- tbl(con, "pry_contracts")

# workforce analytics ----------------------------------------------------
# headcount
headcount <- pry_contracts |>
  left_join(
    pry_est,
    by = c("est_id", "ref_date")
  ) |>
  summarise(
    headcount = n_distinct(personnel_id),
    .by = c(ref_date, adm1)
  )



# wagebill analytics -----------------------------------------------------


# integrated analytics ---------------------------------------------------

