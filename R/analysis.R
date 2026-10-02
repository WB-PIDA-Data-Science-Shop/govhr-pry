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

compute_wagebill <- function(data, group_cols){
  wagebill <- data |>
    summarise(
      wagebill = sum(gross_salary_lcu, na.rm = TRUE),
      .by = all_of(
        c(group_cols, "ref_date")
      )
    ) |>
    mutate(
      share_wagebill = wagebill / sum(wagebill, na.rm = TRUE),
      .by = .data[["ref_date"]]
    )
  
  wagebill
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
      .data[[status_col]] == "active"
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
    filter(
      if_all(
        all_of(c(id_col, group_cols)),
        ~ !is.na(.x)
      )
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
      n_ids = n_distinct(.data[[id_col]])
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

  # if necessary, combine group cols into a single label
  if (length(group_cols) > 1) {
    entity_group <- entity_group |>
      mutate(
        from = paste(!!!rlang::syms(group_cols), sep = " | ")
      )
  } else {
    entity_group <- entity_group |>
      mutate(
        from = .data[[group_cols]]
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
