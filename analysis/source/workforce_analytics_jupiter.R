# set-up -----------------------------------------------------------------
library(dplyr)
library(duckplyr)
library(ggplot2)
library(govhr)

# download Lato from Google Fonts (if needed) and register it with systemfonts
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
  path = here::here("analysis", "figs", "jupiter", "workforce"),
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

pry_est_judicial <- pry_est |>
  filter(
    adm1 == "PODER JUDICIAL" &
      adm2 == "CORTE SUPREMA DE JUSTICIA"
  )

pry_personnel <- pry_personnel |>
  mutate(
    ref_date = as.Date(ref_date)
  ) |>
  filter(
    ref_date >= as.Date("2016-01-01"),
    ref_date <= as.Date("2025-12-01")
  )

pry_contracts_judicial <- pry_contracts |>
  inner_join(
    pry_est_judicial,
    by = c("est_id", "ref_date")
  ) |>
  mutate(
    ref_date = as.Date(ref_date),
    whours = as.double(whours)
  ) |>
  # there seems to be data gaps before 2016
  filter(
    ref_date >= as.Date("2016-01-01"),
    ref_date <= as.Date("2025-12-01")
  )

pry_est_name <- pry_est_judicial |>
  distinct(est_id, .keep_all = TRUE) |>
  select(
    est_id, est_name_native
  )

# workforce analytics ----------------------------------------------------
# headcount
headcount <- pry_contracts_judicial |>
  govhr::compute_headcount()

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
    ylim = c(1.2e4, 1.5e4)
  ) +
  labs(
    x = "Headcount",
    y = "Year"
  )

ggsave(
  "headcount.png"
)

# composition
#  by occupation
headcount_by_occupation <- pry_contracts_judicial |>
  compute_headcount(
    group_cols = c("occupation")
  ) |>
  collect()

# miembro de tribunal
miembro_de_tribunal <- headcount_by_occupation |>
  filter(
    stringr::str_detect(occupation, "MIEMBRO"),
    stringr::str_detect(occupation, "TRIB")
  ) |>
  distinct(occupation)

# juez de primera instancia
juez_de_primera_instancia <- headcount_by_occupation |>
  filter(
    stringr::str_detect(occupation, "JUEZ"),
    stringr::str_detect(occupation, "PRIMERA|1"),
    stringr::str_detect(occupation, "INST")
  ) |>
  distinct(occupation)

# juez de paz
juez_de_paz <- headcount_by_occupation |>
  filter(
    stringr::str_detect(occupation, "JUEZ"),
    stringr::str_detect(occupation, "PAZ")
  ) |>
  distinct(occupation)

# administration superior
administracion_superior <- headcount_by_occupation |>
  filter(
    stringr::str_detect(occupation, "JUEZ")
  ) |>
  distinct(occupation)

# ejecutivo superior
ejecutivo_superior <- headcount_by_occupation |>
  filter(
    stringr::str_detect(occupation, "CONTADOR GENERAL") |
    stringr::str_detect(occupation, "CONTADOR GRAL.") |
    stringr::str_detect(occupation, "COORDINADOR") |
    stringr::str_detect(occupation, "SECRETARIO JUD") |
    stringr::str_detect(occupation, "SECRETARIO GENERAL") |
    stringr::str_detect(occupation, "SUPERVISOR GENERAL") |
    stringr::str_detect(occupation, "DIRECTOR")
  ) |>
  distinct(occupation)

# simplify occupation
headcount_by_occupation_simplified <- headcount_by_occupation |>
  mutate(
    occupation = forcats::fct_lump_n(
      occupation,
      w = headcount,
      n = 10
    )
  ) |>
  summarise(
    headcount = sum(headcount),
    share_headcount = sum(share_headcount),
    .by = c(occupation, ref_date)
  )

headcount_by_occupation_simplified |>
  filter(
    occupation != "Other"
  ) |>
  ggplot() +
  geom_line(
    aes(ref_date, headcount)
  ) +
  facet_wrap(
    vars(occupation),
    nrow = 3
  )

# by contract type
headcount_by_contract_type <- pry_contracts_judicial |>
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
  ggplot() +
  geom_area(
    aes(
      x = ref_date,
      y = share_headcount,
      fill = age_group
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
    x = "Year",
    y = "Share of Headcount"
  ) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1, "lines"),
    legend.key.height = unit(1, "lines")
  ) +
  scale_fill_brewer(palette = "Reds") +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
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
movement <- pry_contracts_judicial |>
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

## by establishment
movement_by_est <- pry_contracts_judicial |>
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

# to aid visualization, sample the 9 largest establishments
sampled_est_ids <- headcount_by_est |>
  summarise(
    headcount = sum(headcount),
    .by = c(est_id)
  ) |>
  slice_max(
    order_by = headcount,
    n = 4
  ) |>
  pull(est_id)

movement_by_est |>
  filter(
    est_id %in% sampled_est_ids
  ) |>
  # join establishment names for better visualization
  left_join(
    pry_est_name |> collect(),
    by = c("est_id")
  ) |>
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
    scales = "free_y",
    labeller = ggplot2::labeller(
      est_name_native = \(x) stringr::str_wrap(x, width = 30)
    )
  )

ggplot2::ggsave(
  "workforce_movements_by_est.png",
  path = here::here("analysis", "figs", "workforce"),
  device = ragg::agg_png,
  width = 12,
  height = 12
)

## net rate
movement_by_est |>
  filter(
    est_id %in% sampled_est_ids
  ) |>
  left_join(
    pry_est_name |> collect(),
    by = c("est_id")
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
    vars(est_name_native),
    scales = "free_y",
    labeller = ggplot2::labeller(
      est_name_native = \(x) stringr::str_wrap(x, width = 30)
    )
  )

ggplot2::ggsave(
  "workforce_net_movements_by_est.png",
  path = here::here("analysis", "figs", "workforce"),
  device = ragg::agg_png,
  width = 12,
  height = 12
)

# transition
## establishment
transitions_est <- pry_contracts_judicial |>
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
transitions_ministry <- pry_contracts_judicial |>
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
