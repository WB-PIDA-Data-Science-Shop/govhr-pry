harmonize_hr <- function(year, db_path){
  funcionarios <- funcionarios_files |>
    stringr::str_subset(
      as.character(year)
    ) |>
    purrr::map_dfr(
      readr::read_csv,
      col_types = readr::cols(
        .default = readr::col_character()
      ),
      # only read the columns used below
      col_select = c(
        anho, mes,
        nivel, descripcion_nivel, entidad, descripcion_entidad, oee, descripcion_oee,
        documento, fecha_nacimiento, anho_ingreso, sexo,
        estado, categoria, cargo, carga_horaria,
        objeto_gasto, concepto, presupuestado, devengado
      )
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
      est_name_native = adm3,
      country_code = "PRY",
      country_name = "Paraguay"
    )

  # module 2. personnel
  # (collapse to one row per person-month first, so the steps below run on far fewer rows)
  pry_personnel <- funcionarios |>
    distinct(
      ref_date,
      personnel_id = documento,
      birth_date = fecha_nacimiento,
      first_employment_date = anho_ingreso,
      gender = sexo
    )

  # only consider earliest employment date as the first
  first_employment <- pry_personnel |>
    arrange(ref_date) |>
    distinct(personnel_id, .keep_all = TRUE) |>
    select(personnel_id, first_employment_date)

  pry_personnel <- pry_personnel |>
    select(-first_employment_date) |>
    left_join(first_employment, by = "personnel_id") |>
    distinct() |>
    mutate(
      age = lubridate::interval(birth_date, ref_date) / lubridate::years(1),
      employment_status = "active",
      .after = birth_date
    )

  # module 3. contracts
  pry_contracts <- funcionarios |>
    transmute(
      ref_date,
      personnel_id = documento,
      est_id = paste(nivel, entidad, oee, sep = "-"),
      contract_type = case_when(
        estado == "PERMANENTE" ~ "permanent",
        estado == "CONTRATADO" ~ "fixed-term",
        estado == "COMISSIONADO" ~ "fixed-term"
      ),
      paygrade = if_else(
        categoria == "SIN CATEGORÍA",
        NA_character_,
        categoria
      ),
      occupation = cargo,
      concepto,
      whours_raw = carga_horaria,
      family_gasto = stringr::str_sub(objeto_gasto, 1, 2),
      presupuestado = as.numeric(presupuestado) %>% # if 0, replace with NA
        if_else(
          . == 0,
          NA_real_,
          .
        ),
      devengado = as.numeric(devengado) %>%
        if_else(
          . == 0,
          NA_real_,
          .
        )
    )

  # generate a new contract ID for each spell of a contract,
  # defined as a consecutive sequence of rows with the same paygrade and occupation
  pry_contracts <- pry_contracts |>
    arrange(personnel_id, est_id, ref_date) |>
    mutate(
      spell_id = consecutive_id(paygrade, occupation),
      .by = c(personnel_id, est_id)
    ) |>
    mutate(
      contract_id = paste(est_id, personnel_id, spell_id, sep = "-")
    ) |>
    select(-spell_id)

  # reference: https://www.mef.gov.py/sites/default/files/2025-08/CLASIFICADOR%20PRESUPUESTARIO%20PGN%202026_0.pdf
  pry_payroll <- pry_contracts |>
    # collapse salary rows and assign them to gross_salary and allowances
    # (split amounts into columns first, so the summary is plain sums)
    mutate(
      paycheck_amount = coalesce(devengado, presupuestado), # if devengado is NA, use presupuestado
      base_salary = coalesce(family_gasto == 11, FALSE),
      base_salary_lcu = if_else(base_salary, paycheck_amount, NA_real_),
      allowance_lcu = if_else(base_salary, NA_real_, paycheck_amount)
    ) |>
    summarise(
      base_salary_lcu = sum(base_salary_lcu, na.rm = TRUE),
      allowance_lcu = sum(allowance_lcu, na.rm = TRUE),
      gross_salary_lcu = sum(paycheck_amount, na.rm = TRUE),
      .by = c(contract_id, ref_date)
    )

  # convert free-text schedules into weekly working hours
  whours_lookup <- pry_contracts |>
    distinct(whours_raw) |>
    mutate(
      whours = parse_whours(
        whours_raw
      )
    )

  # sum work hours across the distinct schedules of each contract-month
  # (allowance lines repeat a salary line's schedule, so duplicates are dropped first)
  pry_whours <- pry_contracts |>
    distinct(contract_id, ref_date, whours_raw) |>
    left_join(whours_lookup, by = "whours_raw") |>
    filter(!is.na(whours)) |>
    summarise(
      whours = sum(whours),
      .by = c(contract_id, ref_date)
    )

  pry_contracts <- pry_contracts |>
    select(
      ref_date,
      est_id,
      personnel_id,
      contract_id,
      contract_type,
      paygrade,
      occupation
    ) |>
    distinct(
      contract_id,
      ref_date,
      .keep_all = TRUE
    ) |>
    left_join(
      pry_payroll,
      by = c("contract_id", "ref_date")
    ) |>
    left_join(
      pry_whours,
      by = c("contract_id", "ref_date")
    )

  # load -------------------------------------------------------------------
  append_to_duckdb(
    list(
      pry_est = pry_est,
      pry_personnel = pry_personnel,
      pry_contracts = pry_contracts
    ),
    db_path = db_path
  )

  invisible(year)
}

# append each table to duckdb; only one process can write at a time,
# so retry until the file lock is released
append_to_duckdb <- function(tables, db_path){
  repeat {
    con <- tryCatch(
      DBI::dbConnect(duckdb::duckdb(), db_path),
      error = function(e) NULL
    )
    if (!is.null(con)) break
    Sys.sleep(5)
  }

  purrr::iwalk(
    tables,
    \(data, name) DBI::dbWriteTable(con, name, data, append = TRUE)
  )

  DBI::dbDisconnect(con, shutdown = TRUE)
}
