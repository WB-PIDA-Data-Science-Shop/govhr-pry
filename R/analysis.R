# the compute_* functions only use functions translated by both dbplyr and
# duckplyr, so they run in duckdb without falling back to dplyr: no if_all(),
# across(), case_when(), paste() or .data[[]] inside verbs.
# column names are injected with !!rlang::sym() instead.

# drop records with a missing value in any of `cols`, as
# filter(if_all(all_of(cols), ~ !is.na(.x))) would
drop_missing <- function(data, cols){
  conditions <- lapply(
    rlang::syms(cols),
    function(col) rlang::expr(!is.na(!!col))
  )

  data |>
    filter(!!!conditions)
}

compute_headcount <- function(data, group_cols = NULL){
  if("ref_date" %in% group_cols){
    stop("`ref_date` should not be included in `group_cols`")
  }

  headcount <- data |>
    summarise(
      headcount = n_distinct(personnel_id),
      .by = all_of(
        c(group_cols, "ref_date")
      )
    ) |>
    mutate(
      share_headcount = headcount / sum(headcount, na.rm = TRUE),
      .by = "ref_date"
    )
  
  headcount
}

compute_wagebill <- function(data, measure_col = "gross_salary_lcu", group_cols = NULL){
  if("ref_date" %in% group_cols){
    stop("`ref_date` should not be included in `group_cols`")
  }

  group_cols_with_date <- c(group_cols, "ref_date")

  wagebill <- data |>
    summarise(
      wagebill = sum(!!rlang::sym(measure_col), na.rm = TRUE),
      .by = all_of(
        group_cols_with_date
      )
    )
    
  # complete the group x date panel, so lag() always refers to the previous
  # period of the calendar, even when a group is missing in some periods
  # cross join through a constant key, since cross_join() is not supported
  # by duckplyr
  if(!is.null(group_cols)){
    wagebill <- wagebill |>
      mutate(
        share_wagebill = wagebill / sum(wagebill, na.rm = TRUE),
        .by = "ref_date"
      )

    # complete implicit missing reference dates
    calendar <- data |>
      distinct(ref_date)

    group_values <- data |>
      select(all_of(group_cols)) |>
      distinct()

    wagebill <- calendar |>
      mutate(key = 1) |>
      inner_join(
        group_values |> mutate(key = 1),
        by = "key"
      ) |>
      select(-key) |>
      # keep missing groups: sql joins do not match NA keys by default
      left_join(
        wagebill,
        by = group_cols_with_date,
        na_matches = "na"
      )
  }
    
  wagebill <- wagebill |>
    mutate(
      wagebill_lag = lag(wagebill, order_by = ref_date),
      wagebill_growth = (wagebill - wagebill_lag) / wagebill_lag,
      .by = all_of(group_cols)
    ) |>
    select(-wagebill_lag)

  wagebill
}

compute_compensation <- function(data, measure_col = "gross_salary_lcu", group_cols = NULL){
  if("ref_date" %in% group_cols){
    stop("`ref_date` should not be included in `group_cols`")
  }

  group_cols_with_date <- c(group_cols, "ref_date")

  compensation <- data |>
    summarise(
      compensation = mean(!!rlang::sym(measure_col), na.rm = TRUE),
      .by = all_of(
        group_cols_with_date
      )
    )

  compensation
}

# distribution ------------------------------------------------------------
# database-friendly replacement for govhr::compute_density().
# works on dbplyr lazy tables, duckplyr frames and plain data frames.
# duckplyr translates neither floor(), %% nor cumsum(), so:
# - bin edges are computed in R from the range of the measure, and records
#   are assigned to their bin with a range join
# - cumulative counts sum, with an inequality self-join, the counts of the
#   bins below. the join grows with groups x bins^2, so keep the number of
#   bins moderate
#
# definitions follow govhr:
# - records with a missing measure are dropped. missing group values are
#   kept as a group of their own
# - bins are [bin, bin + binwidth), with bin a multiple of binwidth. every
#   bin between the lowest and the highest one is reported for every group,
#   with a zero count when empty
# - pct is the share of the group's records in the bin, and cum_pct the
#   share in the bin or below
#
# unlike govhr, several group_cols are allowed and the output is not sorted.
compute_density <- function(
  data,
  measure_col,
  group_cols = NULL,
  binwidth = 1,
  latest_measure = FALSE
) {
  measure <- rlang::sym(measure_col)

  if (latest_measure) {
    latest_date <- data |>
      summarise(
        ref_date = max(ref_date, na.rm = TRUE)
      )

    data <- data |>
      semi_join(
        latest_date,
        by = "ref_date"
      )
  }

  data <- data |>
    drop_missing(measure_col) |>
    select(
      all_of(c(group_cols, measure_col))
    )

  measure_range <- data |>
    summarise(
      n_records = n(),
      lower = min(!!measure, na.rm = TRUE),
      upper = max(!!measure, na.rm = TRUE)
    ) |>
    collect()

  if (measure_range$n_records == 0) {
    stop("`", measure_col, "` has no non-missing values")
  }

  # bin edges, as in govhr. the outer edges are open, so that rounding
  # errors in the range join cannot drop the lowest or highest records
  bin_index <- seq(
    floor(measure_range$lower / binwidth),
    floor(measure_range$upper / binwidth)
  )

  bins <- tibble::tibble(
    key = 1,
    bin_id = seq_along(bin_index),
    bin = bin_index * binwidth,
    bin_lower = c(-Inf, bin[-1]),
    bin_upper = c(bin[-1], Inf)
  )

  binned <- data |>
    inner_join(
      bins |> select(bin_id, bin_lower, bin_upper),
      by = join_by(!!measure >= bin_lower, !!measure < bin_upper),
      copy = TRUE
    ) |>
    summarise(
      count = n(),
      .by = all_of(c(group_cols, "bin_id"))
    )

  # complete the group x bin grid, crossing through a constant key, as in
  # compute_wagebill()
  group_values <- binned |>
    summarise(
      n_bins = n(),
      .by = all_of(group_cols)
    ) |>
    mutate(key = 1)

  grid <- group_values |>
    inner_join(
      bins |> select(key, bin_id, bin),
      by = "key",
      copy = TRUE,
      relationship = "many-to-many"
    ) |>
    select(
      all_of(c(group_cols, "bin_id", "bin"))
    )

  # bins below the group's lowest record have no match, so a zero count
  cumulative <- grid |>
    inner_join(
      binned |> rename(bin_id_below = bin_id, count_below = count),
      by = join_by(!!!rlang::syms(group_cols), bin_id >= bin_id_below),
      na_matches = "na"
    ) |>
    summarise(
      cum_count = sum(count_below, na.rm = TRUE),
      .by = all_of(c(group_cols, "bin_id"))
    )

  grid |>
    left_join(
      binned,
      by = c(group_cols, "bin_id"),
      na_matches = "na"
    ) |>
    left_join(
      cumulative,
      by = c(group_cols, "bin_id"),
      na_matches = "na"
    ) |>
    mutate(
      count = coalesce(count, 0),
      cum_count = coalesce(cum_count, 0)
    ) |>
    mutate(
      total = sum(count, na.rm = TRUE),
      .by = all_of(group_cols)
    ) |>
    mutate(
      pct = count / total,
      cum_pct = cum_count / total
    ) |>
    select(
      all_of(group_cols), bin, count, pct, cum_pct
    )
}

# wagebill growth decomposition -------------------------------------------
# database-friendly replacement for govhr::compute_growth_decomposition().
# works on dbplyr lazy tables, duckplyr frames and plain data frames: the
# group x date panel is completed with joins and lags are window functions,
# so everything is pushed down to duckdb.
#
# definitions follow govhr:
# - records with a missing measure or group value are dropped
# - the group x date panel is completed, so unobserved periods have zero
#   headcount and wagebill and a missing compensation
# - transition_type is "exit" when the group is not observed; otherwise
#   "start" in the first period, "entry" when it was not observed in the
#   previous period and "continuing" when it was
# - continuing groups decompose wagebill growth into employment, compensation
#   and interaction effects. entry_effect is the wagebill at entry, and
#   exit_effect is minus the previous wagebill at exit (zero in later periods
#   of absence)
#
# unlike govhr, the calendar is the set of observed ref_date, as in
# compute_wagebill(), and the output is not sorted.
compute_growth_decomposition <- function(data, measure_col = "gross_salary_lcu", group_cols = NULL){
  if("ref_date" %in% group_cols){
    stop("`ref_date` should not be included in `group_cols`")
  }

  group_cols_with_date <- c(group_cols, "ref_date")
  measure <- rlang::sym(measure_col)

  wagebill <- data |>
    drop_missing(
      c(measure_col, group_cols)
    ) |>
    summarise(
      headcount = n(),
      wagebill = sum(!!measure, na.rm = TRUE),
      compensation = mean(!!measure, na.rm = TRUE),
      .by = all_of(group_cols_with_date)
    )

  # complete the group x date panel, as in compute_wagebill(), so lag()
  # always refers to the previous period of the calendar
  calendar <- data |>
    distinct(ref_date)

  panel <- calendar

  if(!is.null(group_cols)){
    group_values <- wagebill |>
      select(all_of(group_cols)) |>
      distinct()

    panel <- calendar |>
      mutate(key = 1) |>
      inner_join(
        group_values |> mutate(key = 1),
        by = "key"
      ) |>
      select(-key)
  }

  panel |>
    left_join(
      wagebill,
      by = group_cols_with_date
    ) |>
    mutate(
      is_observed = !is.na(headcount),
      headcount = coalesce(headcount, 0),
      wagebill = coalesce(wagebill, 0)
    ) |>
    mutate(
      headcount_lag = lag(headcount, order_by = ref_date),
      compensation_lag = lag(compensation, order_by = ref_date),
      wagebill_lag = lag(wagebill, order_by = ref_date),
      observed_lag = lag(is_observed, order_by = ref_date),
      .by = all_of(group_cols)
    ) |>
    mutate(
      transition_type = if_else(
        !is_observed, "exit",
        if_else(
          is.na(observed_lag), "start",
          if_else(observed_lag, "continuing", "entry")
        )
      )
    ) |>
    mutate(
      # deltas are only defined for continuing groups, NA propagates to the
      # employment, compensation and interaction effects
      delta_headcount = if_else(transition_type == "continuing", headcount - headcount_lag, NA_real_),
      delta_compensation = if_else(transition_type == "continuing", compensation - compensation_lag, NA_real_),
      employment_effect = compensation_lag * delta_headcount,
      compensation_effect = headcount_lag * delta_compensation,
      interaction_effect = delta_headcount * delta_compensation,
      entry_effect = if_else(transition_type == "entry", wagebill, NA_real_),
      exit_effect = if_else(
        transition_type == "exit",
        if_else(coalesce(observed_lag, FALSE), -wagebill_lag, 0),
        NA_real_
      ),
      # entry and exit effects are exclusive, and both are NA at start
      total_effect = if_else(
        transition_type == "continuing",
        employment_effect + compensation_effect + interaction_effect,
        coalesce(entry_effect, exit_effect)
      )
    ) |>
    select(
      all_of(group_cols_with_date),
      transition_type, headcount, headcount_lag, compensation, compensation_lag,
      employment_effect, compensation_effect, interaction_effect, entry_effect,
      delta_compensation, exit_effect, total_effect, wagebill, wagebill_lag,
      is_observed, observed_lag
    )
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
build_calendar <- function(data) {
  calendar <- data |>
    distinct(ref_date) |>
    mutate(
      prev_date = lag(ref_date, order_by = ref_date),
      next_date = lead(ref_date, order_by = ref_date)
    )

  calendar
}

compute_movement <- function(
  data,
  group_cols = NULL,
  status_col = "employment_status",
  freq = NULL
) {
  personnel_active <- data |>
    filter(
      !!rlang::sym(status_col) == "active"
    )

  personnel_unique <- personnel_active |>
    select(personnel_id, ref_date) |>
    distinct()

  personnel_group <- personnel_active |>
    select(
      all_of(c("personnel_id", "ref_date", group_cols))
    ) |>
    distinct()

  calendar <- build_calendar(personnel_unique)

  person_date_calendar <- personnel_unique |>
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
      personnel_unique,
      by = join_by(personnel_id, prev_date == ref_date)
    ) |>
    select(personnel_id, ref_date) |>
    mutate(
      hire = 1
    )

  # fired: no active record in the next period
  separations <- person_date_calendar |>
    filter(
      !is.na(next_date)
    ) |>
    anti_join(
      personnel_unique,
      by = join_by(personnel_id, next_date == ref_date)
    ) |>
    select(personnel_id, ref_date) |>
    mutate(
      fire = 1
    )

  personnel_group |>
    inner_join(
      calendar,
      by = "ref_date"
    ) |>
    left_join(
      hires,
      by = c("personnel_id", "ref_date")
    ) |>
    left_join(
      separations,
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
    )
}

# career transitions ------------------------------------------------------
# database-friendly replacement for govhr::detect_career_transition().
# works on dbplyr lazy tables, duckplyr frames and plain data frames: spells
# are found with lag/lead window functions (gaps-and-islands) instead of
# data.table::rleidv/shift, so everything is pushed down to duckdb.
#
# definitions follow govhr:
# - records with a missing id or group value are dropped
# - an entity with more than one record in the same ref_date has no
#   well-defined position, so all of its records are dropped with a warning
#   (even when those records share the same group)
# - a spell is a run of consecutive observations of the entity in the same
#   group (consecutive in the entity's own history, not in the calendar, so
#   leaving and returning to the same group does not break the spell)
# - each spell is paired with the next one: `from_date` is when the origin
#   spell began, `ref_date` is when the entity is first seen in `to`
# - multiple group_cols are pasted into one label, separated by " | "
#
# unlike govhr, the output is not sorted: ordering is not preserved by lazy
# queries, so arrange() after collect() if needed.
compute_transition <- function(
  data,
  id_col = "contract_id",
  group_cols,
  return_all = FALSE,
  summarize = FALSE
) {
  key_cols <- c(id_col, "ref_date")

  entity_group <- data |>
    drop_missing(
      c(id_col, group_cols)
    ) |>
    select(
      all_of(c(key_cols, group_cols))
    )

  # drop entities holding several records in the same period, even if those
  # records share the same group. checked after the missingness filter, so
  # dropped records are not counted as violations
  key_counts <- entity_group |>
    summarise(
      n_records = n(),
      .by = all_of(key_cols)
    )

  violating_ids <- key_counts |>
    filter(
      n_records > 1
    ) |>
    select(all_of(id_col)) |>
    distinct()

  violations <- key_counts |>
    semi_join(
      violating_ids,
      by = id_col
    ) |>
    summarise(
      n_duplicates = sum(n_records - 1, na.rm = TRUE),
      n_rows = sum(n_records, na.rm = TRUE),
      n_ids = n_distinct(!!rlang::sym(id_col))
    ) |>
    collect()

  if (violations$n_ids > 0) {
    warning(
      sprintf(
        paste(
          "%d record(s) are not uniquely identified by `%s` and `ref_date`;",
          "dropping all %d record(s) for the %d affected `%s` value(s)."
        ),
        as.integer(violations$n_duplicates),
        id_col,
        as.integer(violations$n_rows),
        as.integer(violations$n_ids),
        id_col
      ),
      call. = FALSE
    )

    entity_group <- entity_group |>
      anti_join(
        violating_ids,
        by = id_col
      )
  }

  # if necessary, combine group cols into a single label. paste() cannot be
  # translated, so labels are built in base R for the distinct group values
  # and joined back
  if (length(group_cols) > 1) {
    group_labels <- entity_group |>
      select(all_of(group_cols)) |>
      distinct() |>
      collect()

    group_labels$from <- do.call(
      paste,
      c(group_labels[group_cols], sep = " | ")
    )

    entity_group <- entity_group |>
      left_join(
        group_labels,
        by = group_cols,
        copy = TRUE
      )
  } else {
    entity_group <- entity_group |>
      mutate(
        from = !!rlang::sym(group_cols)
      )
  }

  # a spell starts whenever the group differs from the previous observation
  spells <- entity_group |>
    mutate(
      prev_group = lag(from, order_by = ref_date),
      .by = all_of(id_col)
    ) |>
    filter(
      is.na(prev_group) | prev_group != from
    ) |>
    select(
      all_of(id_col), from, from_date = ref_date
    )

  # date the transition by the destination spell, not the origin one
  transitions <- spells |>
    mutate(
      to = lead(from, order_by = from_date),
      ref_date = lead(from_date, order_by = from_date),
      .by = all_of(id_col)
    ) |>
    select(
      all_of(id_col), from, to, from_date, ref_date
    )

  # if return_all is FALSE, remove terminal spells (non-transitions)
  if (!return_all) {
    transitions <- transitions |>
      filter(
        !is.na(to)
      )
  }

  if(summarize){
    transitions <- transitions |>
      summarise(
        transitions = n(),
        .by = c("from", "to", "ref_date")
      )
  }

  transitions
}

cut_age <- function(age){
  cut(
    age,
    breaks = c(-Inf, 30, 40, 50, 60, Inf),
    labels = c("<30", "30-39", "40-49", "50-59", "60+"),
    right = FALSE
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

plot_transition <- function(data){
  graph_data <- tidygraph::as_tbl_graph(data, directed = TRUE)

  n_nodes <- igraph::gorder(graph_data)
  many_nodes <- n_nodes >= 10
  orange_palette <- grDevices::colorRampPalette(c("#C34729", "#F5C6A0"))(n_nodes)

  graph_data <- graph_data |>
    tidygraph::activate(nodes) |>
    tidygraph::mutate(
      node_id = as.character(dplyr::row_number()),
      node_id = factor(
        .data[["node_id"]],
        levels = as.character(sort(as.integer(.data[["node_id"]])))
      ),
      label = if (many_nodes) .data[["node_id"]] else .data[["name"]],
      degree = tidygraph::centrality_degree(mode = "all")
    )

  point_size <- scales::rescale(
    graph_data |> tidygraph::activate(nodes) |> dplyr::pull(.data[["degree"]]),
    to = if (many_nodes) c(6, 14) else c(20, 30)
  )

  plot <- ggraph::ggraph(graph_data, layout = "stress") +
    ggraph::geom_edge_arc(
      ggplot2::aes(edge_width = .data[["weight"]], edge_alpha = 0.5),
      color = "#4a5568",
      arrow = grid::arrow(length = grid::unit(3, "mm"), type = "closed"),
      end_cap = ggraph::circle(4, "mm"),
      start_cap = ggraph::circle(4, "mm")
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(
        size = point_size,
        color = if (many_nodes) .data[["node_id"]] else "#C34729"
      )
    ) +
    ggraph::geom_node_text(
      ggplot2::aes(label = if (many_nodes) .data[["node_id"]] else .data[["name"]]),
      color = if (many_nodes) "white" else "#2d224e",
      size = if (many_nodes) 3 else 8,
      fontface = "bold"
    ) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(
        x = .data[["x"]],
        y = .data[["y"]],
        size = point_size,
        tooltip = .data[["name"]],
        data_id = .data[["node_id"]]
      ),
      alpha = 0.01
    ) +
    ggraph::scale_edge_width_continuous(range = c(0.2, 3), guide = "none") +
    ggraph::scale_edge_alpha_identity(guide = "none") +
    ggplot2::scale_size_identity(guide = "none") +
    ggplot2::scale_color_manual(values = orange_palette, guide = "none") +
    # the layout pushes nodes to the extremes, so pad the panel and disable
    # clipping to keep the outermost points and labels whole
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.12)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.12)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )

  ggiraph::girafe(
    ggobj = plot,
    width_svg = 10,
    height_svg = 6,
    options = list(
      ggiraph::opts_hover(css = "stroke:#2d224e;stroke-width:2px;"),
      ggiraph::opts_sizing(rescale = TRUE, width = 1)
    )
  )
}

# deflation ---------------------------------------------------------------
# monthly replacement for govhr::deflate_to_real(), which matches the annual
# CPI on the year only, so every month of a year shares one deflator: real
# series jump down every january and drift up within the year.
#
# here each value is matched to the CPI of its own month, from the `cpi` table
# returned by harmonize_cpi() (columns ref_date, country_code and cpi, rebased
# to 100 in its base month), so values are in constant prices of that month,
# december 2021 by default. `country_code` is either a scalar recycled across
# all rows or a column of ISO3 codes. country-months without CPI are NA, with a
# warning.
deflate_to_real_monthly <- function(col, ref_date, country_code, cpi){
  missing_cols <- setdiff(c("ref_date", "country_code", "cpi"), names(cpi))
  if(length(missing_cols) > 0){
    stop(
      "`cpi` should be the output of harmonize_cpi(), missing columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # match on the month, so any day of the month finds its CPI
  input_tbl <- tibble::tibble(
    col = col,
    ref_month = lubridate::floor_date(as.Date(ref_date), "month"),
    country_code = country_code
  )

  cpi_lookup <- cpi |>
    transmute(
      ref_month = lubridate::floor_date(as.Date(ref_date), "month"),
      country_code,
      cpi
    )

  deflated <- input_tbl |>
    left_join(
      cpi_lookup,
      by = c("country_code", "ref_month"),
      relationship = "many-to-one"
    )

  no_cpi <- deflated |>
    filter(
      !is.na(col),
      is.na(cpi)
    ) |>
    distinct(country_code, ref_month)

  if(nrow(no_cpi) > 0){
    warning(
      "No CPI for ", nrow(no_cpi), " country-month(s), deflated to NA: ",
      paste(
        utils::head(paste(no_cpi$country_code, format(no_cpi$ref_month, "%Y-%m")), 10),
        collapse = ", "
      ),
      if(nrow(no_cpi) > 10) ", ..."
    )
  }

  deflated$col * 100 / deflated$cpi
}
