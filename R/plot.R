plot_area <- function(data, x, y, fill = NULL, xlab = NULL, ylab = NULL) {
  data |>
    ggplot() +
    geom_area(
      aes(
        x = {{ x }},
        y = {{ y }},
        fill = {{ fill }}
      ),
      position = "stack"
    ) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y"
    ) +
    labs(
      x = xlab,
      y = ylab
    ) +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(1, "lines"),
      legend.key.height = unit(1, "lines")
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        title.position = "top",
        ncol = 3,
        override.aes = list(linewidth = 6)
      )
    )
}

plot_transition <- function(
  data,
  weight_col = NULL,
  legend = TRUE,
  interactive = FALSE
) {
  node_colour <- "#C34729"
  legend_node_size <- 8
  max_node_size <- 16
  # strictly proportional sizing would shrink the quietest groups below the
  # width of their number, which is drawn in white and would vanish
  min_node_size <- 5

  weight <- if (is.null(weight_col)) NULL else rlang::sym(weight_col)

  # counting before collecting keeps a lazy table of individual moves from
  # being pulled into memory just to be aggregated
  edges <- data |>
    dplyr::count(
      .data[["from"]],
      .data[["to"]],
      wt = !!weight,
      name = "weight"
    ) |>
    dplyr::collect() |>
    # as_tbl_graph() only creates a `name` column from character ids
    dplyr::mutate(
      dplyr::across(c("from", "to"), as.character)
    )

  graph_data <- tidygraph::as_tbl_graph(edges, directed = TRUE) |>
    tidygraph::activate(nodes) |>
    tidygraph::mutate(
      strength = tidygraph::centrality_degree(
        weights = .data[["weight"]],
        mode = "all"
      )
    ) |>
    # numbering by volume of moves makes node 1 the busiest group, so the
    # legend doubles as a ranking
    tidygraph::arrange(dplyr::desc(.data[["strength"]]), .data[["name"]]) |>
    tidygraph::mutate(
      node_id = factor(dplyr::row_number()),
      # point size sets the diameter, so taking the square root makes node
      # area, not width, proportional to the volume of moves in and out
      size = pmax(
        max_node_size * sqrt(.data[["strength"]] / max(.data[["strength"]])),
        min_node_size
      ),
      # a point of size s is about 0.75 * s mm wide; capping edges just past
      # each node's own radius keeps arrowheads visible on large nodes
      cap = 0.375 * .data[["size"]] + 1
    )

  nodes <- graph_data |>
    tidygraph::activate(nodes) |>
    tidygraph::as_tibble()

  node_ids <- levels(nodes[["node_id"]])
  node_labels <- nodes[["name"]]

  # node size is mapped through an identity scale, which has no legend, so
  # point keys would otherwise fall back to the default size and hide the number
  draw_key_node <- function(data, params, size) {
    data[["size"]] <- legend_node_size
    draw_key_point(data, params, size)
  }

  active_theme <- theme_get()

  plot <- ggraph::ggraph(graph_data, layout = "stress") +
    ggraph::geom_edge_arc(
      aes(
        edge_width = .data[["weight"]],
        start_cap = ggraph::circle(.data[["node1.cap"]], "mm"),
        end_cap = ggraph::circle(.data[["node2.cap"]], "mm")
      ),
      # a slight bend separates the two directions between a pair of nodes
      strength = 0.2,
      colour = "#4a5568",
      edge_alpha = 0.5,
      arrow = grid::arrow(length = grid::unit(3, "mm"), type = "closed")
    ) +
    ggraph::geom_node_point(
      aes(
        size = .data[["size"]],
        colour = .data[["node_id"]]
      ),
      key_glyph = draw_key_node
    ) +
    ggraph::geom_node_text(
      aes(label = .data[["node_id"]]),
      colour = "white",
      size = 3.5,
      fontface = "bold"
    ) +
    # ggiraph replaces a layer's geom with its own, which discards key_glyph
    # and cannot be subclassed, so hover is handled by a near-invisible copy of
    # the nodes drawn on top, where the numbers cannot block it
    ggiraph::geom_point_interactive(
      aes(
        x = .data[["x"]],
        y = .data[["y"]],
        size = .data[["size"]],
        tooltip = .data[["name"]],
        data_id = .data[["node_id"]]
      ),
      alpha = 0.01,
      show.legend = FALSE
    ) +
    ggraph::scale_edge_width_continuous(range = c(0.2, 3), guide = "none") +
    scale_size_identity() +
    # identical breaks, labels and titles make ggplot merge the colour and label
    # legends into one, so each key shows the numbered node beside its name
    scale_colour_manual(
      name = NULL,
      values = rep(node_colour, length(node_ids)),
      breaks = node_ids,
      labels = node_labels
    ) +
    scale_discrete_identity(
      aesthetics = "label",
      name = NULL,
      breaks = node_ids,
      labels = node_labels,
      guide = "legend"
    ) +
    # the layout pushes nodes to the extremes, so pad the panel and disable
    # clipping to keep the outermost nodes whole
    scale_x_continuous(expand = expansion(mult = 0.08)) +
    scale_y_continuous(expand = expansion(mult = 0.08)) +
    coord_cartesian(clip = "off") +
    # the active theme may set axis children such as axis.text.x, which would
    # survive blanking their parents, so start from theme_void() and carry
    # over only its font and colours to match the surrounding figures;
    # theme_void() alone would also leave the background transparent
    theme_void(
      base_size = active_theme[["text"]][["size"]],
      base_family = active_theme[["text"]][["family"]],
      ink = active_theme[["geom"]][["ink"]],
      paper = active_theme[["geom"]][["paper"]]
    ) +
    theme(
      legend.position = if (legend) "bottom" else "none",
      plot.margin = margin(10, 10, 10, 10)
    )

  if (!interactive) {
    return(plot)
  }

  ggiraph::girafe(
    ggobj = plot,
    width_svg = 10,
    height_svg = 6,
    options = list(
      # the overlay's alpha also fades its stroke, so restore full opacity
      ggiraph::opts_hover(
        css = "stroke:#2d224e;stroke-width:2px;stroke-opacity:1;"
      ),
      ggiraph::opts_sizing(rescale = TRUE, width = 1)
    )
  )
}

plot_change <- function(data, x_col, y_col, xlab = NULL, ylab = NULL) {
  data |>
    mutate(
      change_sign = if_else({{ y_col }} >= 0, "positive", "negative")
    ) |>
    ggplot() +
    geom_col(
      aes(
        x = {{ x_col }},
        y = {{ y_col }},
        fill = change_sign
      )
    ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "gray"
    ) +
    theme(
      legend.position = "none"
    ) +
    scale_fill_manual(
      values = c(
        "positive" = "#202d85",
        "negative" = "#de4500"
      )
    ) +
    labs(
      x = xlab,
      y = ylab
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 0.1)
    )
}

plot_movement_joint <- function(movement_data, group_cols = NULL) {
  movement_long <- movement_data |>
    tidyr::pivot_longer(
      c(hire_rate, separation_rate),
      names_to = "movement_type",
      values_to = "value"
    )

  plot <- movement_long |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = ref_date,
        y = value,
        color = movement_type,
        group = movement_type
      )
    ) +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(
      name = "Type of Movement",
      labels = c(
        "hire_rate" = "Hires",
        "separation_rate" = "Separations"
      ),
      values = c(
        "hire_rate" = "#202d85",
        "separation_rate" = "#de4500"
      )
    ) +
    ggplot2::labs(
      x = "Year",
      y = "Movement Rate"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 0.1)
    ) +
    ggplot2::theme(
      legend.position = "bottom"
    )

  if (!is.null(group_cols)) {
    missing_cols <- setdiff(group_cols, names(movement_long))

    if (length(missing_cols) > 0) {
      rlang::abort(
        paste0("`group_cols` not found in data: ", paste(missing_cols, collapse = ", "))
      )
    }

    plot <- plot +
      ggplot2::facet_wrap(
        ggplot2::vars(!!!rlang::syms(group_cols))
      )
  }

  plot
}

plot_movement_net <- function(movement_data, group_cols = NULL) {
  movement_net_rate <- movement_data |>
    mutate(
      net_rate = hire_rate - separation_rate,
      net_rate_sign = if_else(net_rate >= 0, "positive", "negative")
    ) 
    
  plot <- movement_net_rate |>
    ggplot() +
    geom_col(
      aes(
        x = ref_date,
        y = net_rate,
        fill = net_rate_sign
      ),
      color = "white",
      linewidth = 0.1
    ) +
    theme(
      legend.position = "none"
    ) +
    # different colors depending on whether net rate is positive or negative
    scale_fill_manual(
      values = c(
        "positive" = "#202d85",
        "negative" = "#de4500"
      )
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 0.1)
    ) +
    labs(
      x = "Year",
      y = "Net Movement Rate"
    ) 

  if (!is.null(group_cols)) {
    missing_cols <- setdiff(group_cols, names(movement_net_rate))

    if (length(missing_cols) > 0) {
      rlang::abort(
        paste0("`group_cols` not found in data: ", paste(missing_cols, collapse = ", "))
      )
    }

    plot <- plot +
      ggplot2::facet_wrap(
        ggplot2::vars(!!!rlang::syms(group_cols)),
        labeller = ggplot2::labeller(
          occupation_group = \(x) stringr::str_wrap(x, width = 30)
        )
      )
  }

  plot
}
