harmonize_hr <- function(year, files, db_path){
  message(
    "Harmonizing data for year ", year, "..."
  )

  funcionarios <- files |>
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
        estado, categoria, funcion, cargo, carga_horaria,
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
      # normalize spelling variants (case, accents, spacing), so the same
      # occupation maps to the same contract
      occupation = cargo |>
        stringi::stri_trans_general("Latin-ASCII") |>
        stringr::str_to_upper() |>
        stringr::str_squish(),
      concepto,
      whours_raw = carga_horaria,
      objeto_gasto,
      presupuestado = as.numeric(presupuestado)
    )

  pry_contracts <- pry_contracts |>
    mutate(
      contract_id = paste(est_id, personnel_id, occupation, sep = "-") |>
        openssl::sha256()
    )

  # reference: https://www.mef.gov.py/sites/default/files/2025-08/CLASIFICADOR%20PRESUPUESTARIO%20PGN%202026_0.pdf
  pry_payroll <- pry_contracts |>
    filter(
      # only include expenses on salaries (classified as 1)
      stringr::str_sub(objeto_gasto, 1, 1) == "1"
    ) |>
    # collapse salary rows and assign them to gross_salary and allowances
    mutate(
      paycheck_amount = presupuestado, # if devengado is NA, use presupuestado
      base_salary = objeto_gasto == 111,
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
    # only pick the first paygrade associated with each contract
    arrange(
      contract_id,
      ref_date,
      paygrade
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
  
  message(
    "Finished harmonizing data for year ", year, "."
  )
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

# monthly consumer price index --------------------------------------------
# monthly headline CPI (all items) from the IMF CPI dataset, which republishes
# the index of each national source, e.g. the Banco Central del Paraguay (BCP)
# for PRY, base december 2017 = 100 (its annual averages match the WDI CPI in
# govhr::macro_indicators). bcp.gov.py sits behind a cloudflare challenge that
# blocks scripted downloads, hence the IMF.
#
# `country_code` is a vector of ISO3 codes, or NULL for every country.
# national base periods differ across countries, so `cpi` is rebased to
# `base_date` = 100 for every country; the published index and its base are
# kept in `cpi_national` and `cpi_national_base_period`. countries without an
# observation at `base_date` get a missing `cpi`, with a warning.
#
# reference: https://data.imf.org/en/datasets/IMF.STA:CPI
# access date: 10/3/2026
harmonize_cpi <- function(country_code = NULL, base_date = "2021-12-01"){
  # series key: country, CPI, all items (_T), index (IX), monthly (M). countries
  # are joined with "+", and "*" matches all of them
  country_key <- if(is.null(country_code)){
    "*"
  } else {
    paste(country_code, collapse = "+")
  }

  url <- paste0(
    "https://api.imf.org/external/sdmx/3.0/data/dataflow/IMF.STA/CPI/+/",
    country_key, ".CPI._T.IX.M"
  )

  cpi_raw <- httr2::request(url) |>
    httr2::req_headers(
      Accept = "application/vnd.sdmx.data+csv;version=2.0.0"
    ) |>
    httr2::req_timeout(300) |>
    httr2::req_retry(max_tries = 5) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    I() |>
    readr::read_csv(
      col_types = readr::cols(.default = readr::col_character())
    )

  missing_countries <- setdiff(country_code, cpi_raw$COUNTRY)
  if(length(missing_countries) > 0){
    warning(
      "No monthly CPI for ", paste(missing_countries, collapse = ", ")
    )
  }

  # series are padded with empty observations back to 1900
  cpi <- cpi_raw |>
    filter(
      !is.na(OBS_VALUE)
    ) |>
    transmute(
      ref_date = lubridate::ym(TIME_PERIOD),
      country_code = COUNTRY,
      cpi_national = as.numeric(OBS_VALUE),
      cpi_national_base_period = REFERENCE_PERIOD
    )

  base_date <- as.Date(base_date)

  base_cpi <- cpi |>
    filter(
      ref_date == base_date
    ) |>
    select(
      country_code,
      base_cpi = cpi_national
    )

  no_base_countries <- setdiff(cpi$country_code, base_cpi$country_code)
  if(length(no_base_countries) > 0){
    warning(
      "No CPI in ", format(base_date, "%Y-%m"), " for ",
      paste(no_base_countries, collapse = ", "), ": `cpi` is missing"
    )
  }

  cpi |>
    left_join(
      base_cpi,
      by = "country_code"
    ) |>
    mutate(
      cpi = cpi_national / base_cpi * 100,
      .after = country_code
    ) |>
    select(-base_cpi) |>
    arrange(country_code, ref_date)
}
