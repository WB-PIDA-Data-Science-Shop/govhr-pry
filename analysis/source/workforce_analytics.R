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
  path = here::here("analysis", "figs", "executive", "workforce"),
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
  )

pry_est_name <- pry_est_executive |>
  distinct(est_id, .keep_all = TRUE) |>
  select(est_id, est_name_native)

pry_budget <- tbl(con, "pry_budget")

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
    est_id,
    ref_date
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
  ggplot() +
  geom_area(
    aes(
      x = ref_date,
      y = headcount,
      fill = est_name_native
    ),
    position = "stack"
  ) +
  labs(
    x = "Year",
    y = "Headcount"
  ) +
  theme(
    legend.position = "bottom"
  ) +
  ggthemes::scale_fill_solarized() +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
      title = "Establishment",
      title.position = "top",
      ncol = 3,
      override.aes = list(linewidth = 6)
    )
  )

ggplot2::ggsave(
  "headcount_by_est.png",
  path = here::here("analysis", "figs", "executive", "workforce"),
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
    pry_contracts_judicial |>
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
    age = as.numeric(age)
  ) |>
  classify_age_group() |>
  summarise(
    headcount = sum(headcount),
    .by = c(ref_date, age_group)
  ) |>
  mutate(
    share_headcount = headcount / sum(headcount, na.rm = TRUE),
    .by = ref_date
  )

headcount_by_age_group |>
  plot_area(
    x = ref_date,
    y = share_headcount,
    fill = age_group,
    xlab = "Year",
    ylab = "Share of Headcount"
  ) +
  scale_fill_manual(
    name = "Age Group",
    values = c(
      "<30" = "#202d85",
      "30-39" = "#de4500",
      "40-49" = "#f2a541",
      "50-59" = "#47b679",
      "60+" = "#8c8c8c"
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
  mutate(
    value = if_else(
      movement_type == "hire_rate",
      value,
      -value
    )
  ) |>
  plot_trend(
    group_col = c("movement_type"),
    y_col = "value",
    y_label = "Movement Rate"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray"
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

## by establishment: only retain ministry of health and education
movement_by_est <- pry_contracts_executive |>
  mutate(
    employment_status = "active"
  ) |>
  filter(
    est_id == "12-8-1" |
    est_id == "12-7-1"
  ) |>
  compute_movement(
    group_cols = c("est_id")
  ) |>
  collect()

# join with establishment names
movement_by_est <- movement_by_est |>
  left_join(
    pry_est |> 
      distinct(est_id, .keep_all = TRUE) |> 
      select(est_id, est_name_native) |>
      collect(),
    by = c("est_id")
  )

# only retain ministry of health and education
movement_by_est <- movement_by_est |>
  inner_join(
    headcount_by_est |>
      summarise(
        headcount = sum(headcount),
        .by = c(est_id)
      ) |>
      slice_max(
        order_by = headcount,
        n = 6
      ) |>
      select(est_id),
    by = c("est_id")
  )

movement_by_est |>
  tidyr::pivot_longer(
    c(hire_rate, separation_rate),
    names_to = "movement_type",
    values_to = "value"
  ) |>
  mutate(
    value = if_else(
      movement_type == "hire_rate",
      value,
      -value
    )
  ) |>
  plot_trend(
    group_col = c("movement_type"),
    y_col = "value",
    y_label = "Movement Rate"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray"
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
  ) +
  facet_wrap(
    vars(est_name_native),
    ncol = 2,
    labeller = ggplot2::labeller(
      est_name_native = \(x) stringr::str_wrap(x, width = 30)
    )
  )

ggplot2::ggsave(
  "workforce_movements_by_est.png",
  path = here::here("analysis", "figs", "executive", "workforce"),
  device = ragg::agg_png,
  width = 12,
  height = 12
)

## net rate
movement_by_est |>
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
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.1)
  ) +
  facet_wrap(
    vars(est_name_native),
    labeller = ggplot2::labeller(
      est_name_native = \(x) stringr::str_wrap(x, width = 30)
    )
  )

ggplot2::ggsave(
  "workforce_net_movements_by_est.png",
  path = here::here("analysis", "figs", "executive", "workforce"),
  device = ragg::agg_png,
  width = 12,
  height = 12
)

# movement by gender
movement_by_gender <- pry_contracts_executive |>
  inner_join(
    pry_personnel |>
      select(personnel_id, gender),
    by = c("personnel_id")
  ) |>
  mutate(
    employment_status = "active"
  ) |>
  compute_movement(
    group_cols = c("gender")
  ) |>
  collect()

movement_by_gender |>
  tidyr::pivot_longer(
    c(hire_rate, separation_rate),
    names_to = "movement_type",
    values_to = "value"
  ) |>
  mutate(
    value = if_else(
      movement_type == "hire_rate",
      value,
      -value
    )
  ) |>
  plot_trend(
    group_col = c("movement_type"),
    y_col = "value",
    y_label = "Movement Rate"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray"
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
  ) +
  facet_wrap(
    vars(gender)
  )

ggplot2::ggsave(
  "movement_by_gender.png",
  path = here::here("analysis", "figs", "executive", "workforce"),
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

ggplot2::ggsave(
  "workforce_transitions_by_est.png",
  path = here::here("analysis", "figs", "workforce"),
  device = ragg::agg_png,
  width = 12,
  height = 12
)

## ministry (adm2)
transitions_ministry <- pry_contracts_executive |>
  compute_transition(
    id_col = "personnel_id",
    group_cols = "adm2",
    summarize = TRUE
  ) |>
  summarise(
    weight = sum(transitions, na.rm = TRUE),
    .by = c("from", "to")
  ) |>
  collect()

transitions_ministry |>
  plot_transition()

ggplot2::ggsave(
  "workforce_transitions_by_ministry.png",
  path = here::here("analysis", "figs", "workforce"),
  device = ragg::agg_png,
  width = 12,
  height = 12
)

# absorption capacity ----------------------------------------------------
budget_by_est <- pry_budget |>
  summarise(
    budget = sum(budget_lcu, na.rm = TRUE),
    .by = c(est_id, ref_date)
  ) |>
  collect()

budget_est <- pry_budget |>
  distinct(est_id, .keep_all = TRUE) |>
  select(est_id, est_name_native) |>
  collect()

headcount_by_est <- pry_contracts_executive |>
  compute_headcount(
    group_cols = c("est_id")
  ) |>
  left_join(
    pry_est |>
      select(ref_date, est_id, adm2),
    by = c("est_id", "ref_date")
  ) |>
  collect() |>
  mutate(
        # extract two first numbers separated by hyphen
        est_id = stringr::str_extract(est_id, "^\\d+-\\d+")
      ) |>
  summarise(
    headcount = sum(headcount, na.rm = TRUE),
    .by = c(est_id, ref_date)
  )

absorption_capacity_by_est <- headcount_by_est |>
  inner_join(
    budget_by_est,
    by = c("est_id", "ref_date")
  ) |>
  left_join(
    budget_est,
    by = c("est_id")
  ) |>
  mutate(
    absorption_capacity = budget / headcount
  )

largest_absorption_capacity_est <- absorption_capacity_by_est |>
  summarise(
    avg_absorption_capacity = mean(absorption_capacity, na.rm = TRUE),
    .by = c(est_id)
  ) |>
  slice_max(
    order_by = avg_absorption_capacity,
    n = 6
  ) |>
  select(est_id)

absorption_capacity_by_est |>
  inner_join(largest_absorption_capacity_est, by = "est_id") |>
  ggplot(
    aes(
      x = ref_date,
      y = absorption_capacity,
      group = est_id
    )
  ) +
  geom_col() +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "₲", big.mark = ".", decimal.mark = ",")
  ) +
  coord_cartesian(
    ylim = c(0, NA)
  ) +
  facet_wrap(
    vars(est_name_native),
    ncol = 3,
    labeller = ggplot2::labeller(
      est_name_native = \(x) stringr::str_wrap(x, width = 30)
    )
  ) +
  labs(
    x = "Year",
    y = "Absorption Capacity (Budget per Headcount)"
  )

ggsave(
  "absorption_capacity_by_est.png"
)
