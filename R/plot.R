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
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1)
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
