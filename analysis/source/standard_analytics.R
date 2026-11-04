# set-up -----------------------------------------------------------------
library(dplyr)
library(duckplyr)
library(ggplot2)
library(govhr)

# download Roboto from Google Fonts (if needed) and register it with systemfonts
systemfonts::require_font("Roboto")

theme_set(
  theme_classic(
    base_size = 14,
    base_family = "Roboto"
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

source(
  here::here("R", "harmonize.R")
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

# monthly CPI, december 2021 = 100
pry_cpi <- harmonize_cpi("PRY")

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
  ) |>
  # deflate wages to real ₲ (december 2021 prices) with the CPI of each month
  left_join(
    copy_to(
      con,
      select(pry_cpi, ref_date, cpi),
      "pry_cpi",
      overwrite = TRUE
    ),
    by = "ref_date"
  ) |>
  mutate(
    across(
      c(base_salary_lcu, allowance_lcu, gross_salary_lcu),
      ~ .x * 100 / cpi
    )
  )

months_no_cpi <- pry_contracts_executive |>
  filter(
    is.na(cpi)
  ) |>
  distinct(ref_date) |>
  pull(ref_date)

if(length(months_no_cpi) > 0){
  warning(
    "No CPI for ", length(months_no_cpi), " month(s), wages deflated to NA: ",
    paste(format(sort(months_no_cpi), "%Y-%m"), collapse = ", ")
  )
}

pry_contracts_executive <- pry_contracts_executive |>
  select(-cpi)

pry_est_name <- pry_est_executive |>
      distinct(est_id, .keep_all = TRUE) |>
      select(est_id, est_name_native) 

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
  coord_cartesian(
    ylim = c(0, NA)
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
    age = as.numeric(age),
    age_group = cut_age(age)
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
## overall
movement <- pry_contracts_executive |>
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

movement |>
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
      "hire_rate" = "Hires",
      "separation_rate" = "Separations"
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
  )

ggsave(
  "workforce_movements.png"
)

## by establishment
movement_by_est <- pry_contracts_executive |>
  mutate(
    employment_status = "active"
  ) |>
  compute_movement(
    group_cols = c("est_id")
  ) |>
  mutate(
    replacement_rate = hires / separations
  ) |>
  collect()

# to aid visualization, randomly sample 10 establishments that span the entire
# period
sampled_est_ids <- movement_by_est |>
  group_by(est_id) |>
  filter(
    n_distinct(ref_date) == n_distinct(movement_by_est$ref_date)
  ) |>
  ungroup() |>
  distinct(est_id) |>
  slice_sample(n = 9) |>
  pull(est_id)

movement_by_est |>
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
movement_by_est |>
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

# transition
## establishment
transitions_est <- pry_contracts_executive |>
  compute_transition(
    id_col = "personnel_id",
    group_cols = "est_id",
    summarize = TRUE
  )
  
transitions_est <- transitions_est |>
  left_join(
    pry_est_name,
    by = c("from" = "est_id")
  ) |>
  left_join(
    pry_est_name,
    by = c("to" = "est_id"),
    suffix = c("_from", "_to")
  ) |>
  select(
    from = est_name_native_from,
    to = est_name_native_to,
    transitions
  ) |>
  summarise(
    weight = sum(transitions, na.rm = TRUE),
    .by = c("from", "to")
  ) |>
  collect()

transitions_est |>
  plot_transition()
