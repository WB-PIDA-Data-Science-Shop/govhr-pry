# deflation ---------------------------------------------------------------
# replacement for govhr::deflate_to_real(), with the same arguments and
# output: `col` in constant prices of `base_month`, using the CPI of each
# value's own month. months without CPI are NA, with a warning.
#
# govhr returns the joined values by position, but once duckplyr is attached
# the join runs in duckdb, which does not keep row order, so values get the
# CPI of other rows. here rows are sorted back by their id before returning.
# deflator of each country-month: CPI of `base_month` over CPI of the month
cpi_deflator <- function(base_month){
  govhr::cpi |>
    mutate(
      deflator = cpi[match(as.Date(base_month), ref_date)] / cpi,
      .by = country_code
    ) |>
    select(country_code, ref_date, deflator)
}

# deflates the `wage_cols` of `data` to constant prices of `base_month`, with
# the CPI of `country_code` (an ISO3 code). ref_date is matched exactly to the
# dates of govhr::cpi, which are the first of each month: other dates are NA.
deflate_wages <- function(data, wage_cols, country_code, base_month = "2021-12-01"){
  UseMethod("deflate_wages")
}

# match() looks deflators up by date, so rows keep their order even when
# duckplyr is attached. columns are replaced outside of mutate(), so
# `country_code` cannot be masked by a column of the same name
deflate_wages.data.frame <- function(data, wage_cols, country_code, base_month = "2021-12-01"){
  deflator <- cpi_deflator(base_month) |>
    filter(
      country_code == !!country_code
    )

  row_deflator <- deflator$deflator[match(data$ref_date, deflator$ref_date)]

  no_cpi <- sort(unique(data$ref_date[is.na(row_deflator)]))

  if(length(no_cpi) > 0){
    warning(
      "No CPI for ", length(no_cpi), " ref_date(s), deflated to NA: ",
      paste(utils::head(format(no_cpi), 10), collapse = ", "),
      if(length(no_cpi) > 10) ", ..."
    )
  }

  for(col in wage_cols){
    data[[col]] <- data[[col]] * row_deflator
  }

  data
}

# the deflator is joined in duckdb, so the data stays in the database.
# ref_dates without CPI are NA, without the warning of the data.frame method.
deflate_wages.tbl_dbi <- function(data, wage_cols, country_code, base_month = "2021-12-01"){
  deflator <- cpi_deflator(base_month) |>
    filter(
      country_code == !!country_code
    ) |>
    select(ref_date, deflator)

  # copy = "inline" writes the deflator into the query itself, so no table is
  # created on the connection. a table dropped after the join would break the
  # query, which only runs later, on collect()
  data |>
    left_join(
      deflator,
      by = "ref_date",
      copy = "inline"
    ) |>
    mutate(
      across(all_of(wage_cols), \(col) col * deflator)
    ) |>
    select(-deflator)
}

cut_age <- function(age){
  # by decade from 20-60+
  cut(
    age,
    breaks = c(0, seq(20, 60, by = 10), Inf),
    right = FALSE,
    include.lowest = TRUE,
    labels = c(
      "<20",
      "20-29",
      "30-39",
      "40-49",
      "50-59",
      "60+"
    )
  )
}
