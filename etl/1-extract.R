# Download the monthly civil servant files (funcionarios_YYYY_M.csv.zip)
# published by Paraguay's Secretaría de la Función Pública (SFP) and save the
# CSV inside each one as data/data-raw/funcionarios_YYYY_M.csv.gz.
#
# https://datos.sfp.gov.py/data/funcionarios/download is an AngularJS page that
# builds its table in the browser, so its HTML contains no links. The table is
# populated from the JSON endpoint /list/data, which lists every file served
# under /data/. We query that endpoint directly.
#
# access date: 9/28/2026

sfp_base_url <- "https://datos.sfp.gov.py"
raw_data_dir <- file.path("data", "data-raw", "hr")

# URLs of every monthly funcionarios CSV zip.
list_funcionarios_urls <- function(base_url = sfp_base_url) {
  files <- httr2::request(base_url) |>
    httr2::req_url_path("list/data") |>
    httr2::req_retry(max_tries = 5) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)

  zips <- grep("^funcionarios_\\d{4}_\\d{1,2}\\.csv\\.zip$", files$data, value = TRUE)
  paste0(base_url, "/data/", zips)
}

# Save the CSV inside funcionarios_YYYY_M.csv.zip as
# <dest_dir>/funcionarios_YYYY_M.csv.gz, re-encoded from Latin-1 to UTF-8.
download_funcionarios_csv <- function(url, dest_dir = raw_data_dir) {
  csv_name <- sub("\\.zip$", "", basename(url))
  dest <- fs::path(dest_dir, paste0(csv_name, ".gz"))

  zip_path <- tempfile(fileext = ".zip")
  on.exit(unlink(zip_path))
  httr2::request(url) |>
    httr2::req_retry(max_tries = 5) |>
    httr2::req_perform(path = zip_path)

  # Read the CSV by name: given a zip path, readr silently reads only the first
  # file. unz() errors if csv_name is not in the archive. All columns are read
  # as character with no NA conversion, so values are written back as published.
  readr::read_csv(
    unz(zip_path, csv_name),
    col_types = readr::cols(.default = readr::col_character()),
    locale = readr::locale(encoding = "latin1"),
    na = character()
  ) |>
    readr::write_csv(dest, na = "")
  dest
}

dir.create(raw_data_dir, recursive = TRUE, showWarnings = FALSE)

funcionarios_urls <- list_funcionarios_urls(sfp_base_url)

funcionarios_urls |>
  purrr::map(
    download_funcionarios_csv,
    dest_dir = raw_data_dir
  )

# budget -----------------------------------------------------------------
presupuestos <- readr::read_csv(
  "https://pyenresultados.rindiendocuentas.gov.py/Documentos/PresupuestoPND_MH.csv"
)

write_csv(
  presupuestos,
  here::here("data", "data-raw", "budget", "pry_budget.csv.gz")
)