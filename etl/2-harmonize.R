# set-up -----------------------------------------------------------------
library(dplyr)
library(readr)
library(govhr)
library(here)

# read-in ----------------------------------------------------------------
funcionarios_files <- fs::dir_ls(
  "data/data-raw", 
  regexp = "funcionarios_\\d{4}_\\d{1,2}\\.csv\\.gz$"
)

funcionarios <- funcionarios_files |>
  purrr::map_dfr(
    readr::read_csv,
    col_types = readr::cols(
      .default = readr::col_character()
    ),
    n_max = 100
  )

# harmonize --------------------------------------------------------------
funcionarios <- funcionarios |>
  mutate(
    ref_date = lubridate::ymd(paste0(anho, "-", mes, "-01"))
  )

# module 1. establishments
pry_est <- funcionarios |>
  select(
    ref_date,
    adm1_code = nivel,
    adm1 = descripcion_nivel,
    adm2_code = entidad,
    adm2 = descripcion_entidad,
    adm3_code = oee,
    adm3 = descripcion_oee
  ) |>
  distinct()

pry_est <- pry_est |>
  mutate(
    est_id = paste(adm1_code, adm2_code, adm3_code, sep = "-"),
    est_name_native = adm3
  )

# module 2. personnel
pry_personnel <- funcionarios |>
  transmute(
    ref_date,
    personnel_id = documento,
    birth_date = fecha_nacimiento,
    age = lubridate::interval(fecha_nacimiento, ref_date) / lubridate::years(1),
    employment_status = "active",
    first_employment_date = anho_ingreso
  ) |>
  # only consider earliest employment date as the first
  mutate(
    first_employment_date = dplyr::first(first_employment_date, order_by = ref_date),
    .by = personnel_id
  ) |>
  distinct()

# module 3. contracts
pry_contracts <- funcionarios |>
  transmute(
    ref_date,
    personnel_id = documento,
    est_id = paste(nivel, entidad, oee, sep = "-"),
    contract_id = paste(est_id, personnel_id, sep = "-"),
    contract_type = case_when(
      estado == "PERMANENTE" ~ "permanent",
      estado == "CONTRATADO" ~ "fixed-term",
      estado == "COMISSIONADO" ~ "fixed-term"
    ),
    paygrade = categoria,
    occupation = cargo,
    concepto,
    family_gasto = stringr::str_sub(objeto_gasto, 1, 2),
    presupuestado = as.numeric(presupuestado),
    devengado = as.numeric(devengado),
  )

# reference: https://www.mef.gov.py/sites/default/files/2025-08/CLASIFICADOR%20PRESUPUESTARIO%20PGN%202026_0.pdf
pry_paycheck <- pry_contracts |> 
  # collapse salary rows and assign them to gross_salary and allowances
  mutate(
    concepto = stringr::str_to_upper(concepto),
    paycheck_type = case_when(
      family_gasto == 11 ~ "base_salary",
      TRUE ~ "allowance"
    ),
    paycheck_amount = pmax(devengado, presupuestado, na.rm = TRUE)
  ) |>
  summarise(
    base_salary_lcu = sum(paycheck_amount[paycheck_type == "base_salary"], na.rm = TRUE),
    allowance_lcu = sum(paycheck_amount[paycheck_type == "allowance"], na.rm = TRUE),
    gross_salary_lcu = sum(paycheck_amount, na.rm = TRUE),
    .by = c(contract_id, ref_date)
  )

pry_contracts <- pry_contracts |>
  distinct(
    ref_date,
    contract_id,
    .keep_all = TRUE
  ) |>
  left_join(
    pry_paycheck,
    by = c("contract_id", "ref_date")
  )

# load -------------------------------------------------------------------
pry_est |>
  write_csv(
    here("data", "data-clean", "pry_est.csv.gz"),
  )

pry_personnel |>
  write_csv(
    here("data", "data-clean", "pry_personnel.csv.gz"),
  )

pry_contracts |>
  write_csv(
    here("data", "data-clean", "pry_contracts.csv.gz"),
  )