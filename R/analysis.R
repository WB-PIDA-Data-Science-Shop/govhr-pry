compute_headcount <- function(data, group_cols){
  headcount <- data |>
    summarise(
      headcount = n_distinct(personnel_id),
      .by = all_of(
        c(group_cols, "ref_date")
      )
    ) |>
    mutate(
      share_headcount = headcount / sum(headcount, na.rm = TRUE),
      .by = .data[["ref_date"]]
    )
  
  headcount
}

# workforce movement ------------------------------------------------------
# database-friendly replacement for govhr::compute_workforce_movement().
# works on dbplyr lazy tables (tbl(con, ...)), duckplyr frames and plain
# data frames: only joins, distinct and grouped sums are used, so everything
# is pushed down to duckdb and no personnel x date grid is materialized.
#
# definitions follow govhr:
# - hire at t: active at t, not active at the previous period of the calendar
# - fire at t: active at t, not active at the next period of the calendar
# - the calendar is the full sequence of dates between the first and last
#   ref_date (by `freq`), so hires are NA in the first period and fires are
#   NA in the last period, where they cannot be observed
#
# unlike govhr, movements are counted as distinct personnel (not rows), so an
# employee holding two contracts is counted once per group, consistent with
# compute_headcount(). movements are attributed to the group(s) the employee
# belongs to in the period of the event.
build_movement_calendar <- function(data, freq = NULL) {
  # only the distinct dates are pulled into R
  ref_dates <- data |>
    distinct(ref_date) |>
    collect() |>
    pull(ref_date) |>
    as.Date()

  if (is.null(freq)) {
    freq <- govhr::guess_date_frequency(
      list(ref_date = ref_dates)
    )
  }

  full_dates <- seq(
    min(ref_dates),
    max(ref_dates),
    by = freq
  )

  calendar <- tibble::tibble(
    ref_date = full_dates,
    prev_date = dplyr::lag(full_dates),
    next_date = dplyr::lead(full_dates)
  )

  # place the (small) calendar on the same backend as the data
  if (inherits(data, "tbl_lazy")) {
    calendar <- dbplyr::copy_inline(dbplyr::remote_con(data), calendar)
  } else if (inherits(data, "duckplyr_df")) {
    calendar <- duckplyr::as_duckdb_tibble(calendar)
  }

  calendar
}

compute_movement <- function(
  data,
  group_cols = NULL,
  status_col = "employment_status",
  freq = NULL
) {
  active <- data

  if (!is.null(status_col)) {
    active <- active |>
      filter(
        !!rlang::sym(status_col) == "active"
      )
  }

  person_date <- active |>
    select(personnel_id, ref_date) |>
    distinct()

  person_group <- active |>
    select(
      all_of(c("personnel_id", "ref_date", group_cols))
    ) |>
    distinct()

  calendar <- build_movement_calendar(person_date, freq = freq)

  person_date_calendar <- person_date |>
    inner_join(
      calendar,
      by = "ref_date"
    )

  # hired: no active record in the previous period
  hires <- person_date_calendar |>
    filter(
      !is.na(prev_date)
    ) |>
    anti_join(
      person_date,
      by = join_by(personnel_id, prev_date == ref_date)
    ) |>
    select(personnel_id, ref_date) |>
    mutate(
      hire = 1
    )

  # fired: no active record in the next period
  fires <- person_date_calendar |>
    filter(
      !is.na(next_date)
    ) |>
    anti_join(
      person_date,
      by = join_by(personnel_id, next_date == ref_date)
    ) |>
    select(personnel_id, ref_date) |>
    mutate(
      fire = 1
    )

  person_group |>
    inner_join(
      calendar,
      by = "ref_date"
    ) |>
    left_join(
      hires,
      by = c("personnel_id", "ref_date")
    ) |>
    left_join(
      fires,
      by = c("personnel_id", "ref_date")
    ) |>
    summarise(
      headcount = n(),
      hires = sum(hire, na.rm = TRUE),
      separations = sum(fire, na.rm = TRUE),
      .by = all_of(c("ref_date", "prev_date", "next_date", group_cols))
    ) |>
    mutate(
      hires = if_else(is.na(prev_date), NA_real_, hires),
      separations = if_else(is.na(next_date), NA_real_, separations),
      hire_rate = hires / headcount,
      separation_rate = separations / headcount
    ) |>
    select(
      all_of(c("ref_date", group_cols)),
      headcount, hires, separations, hire_rate, separation_rate
    ) |>
    arrange(
      ref_date
    )
}

plot_headcount_trend <- function(data, group_cols, group_labels){
  data |>
    ggplot(
      aes(
        x = ref_date,
        y = headcount,
        color = .data[[group_cols]]
      )
    ) +
    geom_line() +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y"
    ) +
    scale_y_continuous(
      labels = scales::comma_format()
    ) +
    labs(
      x = "Headcount",
      y = "Year",
      color = group_labels
    )
}