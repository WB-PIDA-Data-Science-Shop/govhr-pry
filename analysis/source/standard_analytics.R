# set-up -----------------------------------------------------------------
library(dplyr)
library(duckplyr)
library(ggplot2)
library(govhr)

# download Lato from Google Fonts (if needed) and register it with systemfonts
systemfonts::require_font("Lato")

theme_set(
  theme_classic(
    base_size = 14,
    base_family = "Lato"
  ) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
)

ggsave <- purrr::partial(
  ggplot2::ggsave,
  device = ragg::agg_png,
  path = here::here("analysis", "figs"),
  width = 9,
  height = 6
)

source(
  here::here("R", "analysis.R")
)

set.seed(2026)

# read-in ----------------------------------------------------------------
con <- DBI::dbConnect(
  duckdb::duckdb(),
  here::here("data", "data-clean", "govhr_pry.duckdb")
)

pry_est <- tbl(con, "pry_est")
pry_personnel <- tbl(con, "pry_personnel")
pry_contracts <- tbl(con, "pry_contracts")

pry_est_executive <- pry_est |>
  filter(
    adm1 == "PODER EJECUTIVO"
  )

pry_personnel <- pry_personnel |>
  mutate(
    ref_date = as.Date(ref_date)
  ) |>
  filter(
    ref_date >= as.Date("2016-01-01"),
    ref_date <= as.Date("2025-12-01")
  )

pry_contracts_executive <- pry_contracts |>
  inner_join(
    pry_est_executive,
    by = c("est_id", "ref_date")
  ) |>
  mutate(
    ref_date = as.Date(ref_date),
    across(
      c(base_salary_lcu, allowance_lcu, gross_salary_lcu, whours),
      as.double
    )
  ) |>
  # there seems to be data gaps before 2016
  filter(
    ref_date >= as.Date("2016-01-01"),
    ref_date <= as.Date("2025-12-01")
  )

# workforce analytics ----------------------------------------------------
# headcount
headcount <- pry_contracts_executive |>
  summarise(
    headcount = n_distinct(personnel_id),
    .by = c(ref_date)
  ) |>
  mutate(
    ref_date = as.Date(ref_date)
  ) |>
  collect()

headcount |>
  ggplot(
    aes(
      x = ref_date,
      y = headcount
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
    y = "Year"
  )

ggsave(
  "headcount.png"
)

# composition
# by establishment
headcount_by_est <- pry_contracts_executive |>
  compute_headcount(
    group_cols = c("est_id")
  ) |>
  left_join(
    # only use original establishment name
    pry_est_executive |>
      distinct(est_id, .keep_all = TRUE) |>
      select(est_id, est_name_native),
    by = c("est_id")
  ) |>
  collect() |>
  arrange(
    est_id, ref_date
  )

headcount_by_est |>
  mutate(
    est_name_native = forcats::fct_lump(
      est_name_native,
      n = 6,
      w = headcount,
      other_level = "Other"
    ) |>
      forcats::fct_reorder(
        headcount,
        .fun = sum,
        .desc = TRUE
      )
  ) |>
  summarise(
    headcount = sum(headcount),
    .by = c(ref_date, est_name_native)
  ) |>
  # wrap long establishment names so they fit in the legend
  mutate(
    est_name_native = forcats::fct_relabel(
      est_name_native,
      \(x) stringr::str_wrap(x, width = 30)
    )
  ) |>
  plot_trend(
    group_col = "est_name_native",
    y_col = "headcount",
    y_label = "Headcount"
  ) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1, "lines"),
    legend.key.height = unit(1, "lines")
  ) +
  ggthemes::scale_color_solarized() +
  ggplot2::guides(
    color = ggplot2::guide_legend(
      title = "Establishment",
      title.position = "top",
      ncol = 3,
      override.aes = list(linewidth = 6)
    )
  )

ggplot2::ggsave(
  "headcount_by_est.png",
  path = here::here("analysis", "figs"),
  device = ragg::agg_png,
  width = 12,
  height = 9
)

# by contract type
headcount_by_contract_type <- pry_contracts_executive |>
  compute_headcount(
    group_cols = c("contract_type")
  ) |>
  collect()

headcount_by_contract_type |>
  plot_trend(
    group_col = "contract_type",
    y_col = "headcount",
    y_label = "Headcount"
  ) 

# by age
headcount_by_age <- pry_personnel |>
  inner_join(
    pry_contracts_executive |>
      distinct(personnel_id, ref_date),
    by = c("personnel_id", "ref_date")
  ) |>
  compute_headcount(
    group_cols = c("age")
  ) |>
  collect()

# aggregate by age group
headcount_by_age_group <- headcount_by_age |>
  filter(
    !is.na(age)
  ) |>
  mutate(
    age_group = cut(
      as.double(age),
      breaks = c(0, 30, 40, 50, 60, Inf),
      labels = c("<30", "30-39", "40-49", "50-59", "60+"),
      right = FALSE
    )
  ) |>
  summarise(
    headcount = sum(headcount),
    .by = c(ref_date, age_group)
  ) |>
  mutate(
    share_headcount = headcount / sum(headcount, na.rm = TRUE),
    .by = ref_date
  )

headcount_by_age_group |>
  plot_trend(
    group_col = "age_group",
    y_col = "share_headcount",
    y_label = "Share of Headcount"
  ) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1, "lines"),
    legend.key.height = unit(1, "lines")
  ) +
  scale_color_brewer(palette = "Reds") +
  ggplot2::guides(
    color = ggplot2::guide_legend(
      title = "Age Group",
      title.position = "top",
      ncol = 3,
      override.aes = list(linewidth = 6)
    )
  )

ggsave(
  "headcount_by_age_group.png"
)

# movements
## recruitment
recruitment <- pry_contracts_executive |>
  mutate(
    employment_status = "active"
  ) |>
  govhr::compute_workforce_movement(
    movement_type = "hire",
    measurement_type = "count",
    group_cols = c("ref_date")
  ) |>
  rename(
    hires = indicator
  )

## overall
workforce <- pry_contracts_executive |>
  mutate(
    employment_status = "active"
  ) |>
  compute_movement(
    group_cols = c("ref_date")
  ) |>
  mutate(
    replacement_rate = hires / separations
  ) |>
  collect()

workforce |>
  tidyr::pivot_longer(
    c(hires, separations),
    names_to = "movement_type",
    values_to = "value"
  ) |>
  plot_trend(
    group_col = c("movement_type"),
    y_col = "value",
    y_label = "Number of Movements"
  ) +
  scale_color_manual(
    values = c(
      "hires" = "#202d85",
      "separations" = "#de4500"
    )
  )

ggsave(
  "workforce_movements.png"
)

## by establishment
workforce <- pry_contracts_executive |>
  mutate(
    employment_status = "active"
  ) |>
  compute_movement(
    group_cols = c("ref_date", "est_id")
  ) |>
  mutate(
    replacement_rate = hires / separations
  ) |>
  collect()

# to aid visualization, randomly sample 10 establishments that span the entire
# period
sampled_est_ids <- workforce |>
  group_by(est_id) |>
  filter(
    n_distinct(ref_date) == n_distinct(workforce$ref_date)
  ) |>
  ungroup() |>
  distinct(est_id) |>
  slice_sample(n = 12) |>
  pull(est_id)

workforce |>
  filter(
    est_id %in% sampled_est_ids
  ) |>
  tidyr::pivot_longer(
    c(hire_rate, separation_rate),
    names_to = "movement_type",
    values_to = "value"
  ) |>
  plot_trend(
    group_col = c("movement_type"),
    y_col = "value",
    y_label = "Number of Movements"
  ) +
  scale_color_manual(
    name = "Type of Movement",
    labels = c(
      "hire_rate" = "Hire Rate",
      "separation_rate" = "Separation Rate"
    ),
    values = c(
      "hire_rate" = "#202d85",
      "separation_rate" = "#de4500"
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.1)
  ) +
  theme(
    legend.position = "bottom"
  ) +
  facet_wrap(
    vars(est_id),
    ncol = 3
  )

ggplot2::ggsave(
  "workforce_movements_by_est.png",
  path = here::here("analysis", "figs"),
  device = ragg::agg_png,
  width = 12,
  height = 12
)

## net rate
workforce |>
  filter(
    est_id %in% sampled_est_ids
  ) |>
  mutate(
    net_rate = hire_rate - separation_rate
  ) |>
  plot_trend(
    group_col = c("est_id"),
    y_col = "net_rate",
    y_label = "Net Change in Headcount"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray"
  ) +
  theme(
    legend.position = "none"
  ) +
  # set same color for all lines
  scale_color_manual(
    values = rep("#d99e73", length(sampled_est_ids))
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.1)
  ) +
  facet_wrap(
    vars(est_id)
  )

ggplot2::ggsave(
  "workforce_net_movements_by_est.png",
  path = here::here("analysis", "figs"),
  device = ragg::agg_png,
  width = 12,
  height = 12
)

# wagebill analytics -----------------------------------------------------


# integrated analytics ---------------------------------------------------


# write-out --------------------------------------------------------------
workforce <- headcount |>
  left_join(
    movement,
    by = c("ref_date")
  ) |>
  arrange(
    ref_date
  )

workforce |>
  readr::write_csv(
    here::here("data", "data-clean", "meso", "workforce.csv")
  )


