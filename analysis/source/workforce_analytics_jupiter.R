# set-up -----------------------------------------------------------------
library(dplyr)
library(duckplyr)
library(ggplot2)
library(ggalluvial)
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

source(
  here::here("R", "occupation.R")
)

source(
  here::here("R", "plot.R")
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
  ) |>
  classify_occupation_group()

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
headcount_by_occupation_group <- pry_contracts_judicial |>
  compute_headcount(
    group_cols = c("occupation_group")
  ) |>
  collect()

headcount_by_occupation_group |>
  filter(
    occupation_group != "Otros"
  ) |>
  ggplot() +
  geom_line(
    aes(ref_date, headcount, color = occupation_group)
  ) +
  labs(
    x = "Year",
    y = "Headcount"
  ) +
  theme(
    legend.position = "bottom"
  ) +
  scale_color_manual(
    name = "Occupation Group",
    values = c(
      "Miembro de Tribunal" = "#202d85",
      "Juez de Primera Instancia" = "#de4500",
      "Juez de Paz" = "#f2a541"
    )
  ) +
  guides(
    color = guide_legend(
      title = "Occupation Group",
      nrow = 2,
      byrow = TRUE
    )
  )

ggsave(
  "headcount_by_occupation_group.png"
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

# by gender
headcount_by_gender <- pry_personnel |>
  inner_join(
    pry_contracts_judicial |>
      distinct(personnel_id, ref_date),
    by = c("personnel_id", "ref_date")
  ) |>
  compute_headcount(
    group_cols = c("gender")
  ) |>
  collect()

headcount_by_gender |>
  plot_area(
    x = ref_date,
    y = share_headcount,
    fill = gender,
    xlab = "Year",
    ylab = "Share of Headcount"
  ) +
  scale_fill_manual(
    name = "Gender",
    labels = c(
      "FEMENINO" = "Women",
      "MASCULINO" = "Men"
    ),
    values = c(
      "MASCULINO" = "#202d85",
      "FEMENINO" = "#de4500"
    )
  )

ggsave(
  "headcount_by_gender.png"
)

# headcount by gender x occupation_group
headcount_by_gender_occupation <- pry_personnel |>
  inner_join(
    pry_contracts_judicial |>
      distinct(personnel_id, ref_date, occupation_group),
    by = c("personnel_id", "ref_date")
  ) |>
  compute_headcount(
    group_cols = c("gender", "occupation_group")
  ) |>
  collect() |>
  mutate(
    share_headcount = headcount/sum(headcount, na.rm = TRUE),
    .by = c(ref_date, occupation_group)
  )

headcount_by_gender_occupation |>
  plot_area(
    x = ref_date,
    y = share_headcount,
    fill = gender,
    xlab = "Year",
    ylab = "Share of Headcount"
  ) +
  scale_fill_manual(
    name = "Gender",
    labels = c(
      "FEMENINO" = "Women",
      "MASCULINO" = "Men"
    ),
    values = c(
      "MASCULINO" = "#202d85",
      "FEMENINO" = "#de4500"
    )
  ) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    color = "gray"
  ) +
  facet_wrap(
    vars(occupation_group),
    ncol = 2,
    labeller = ggplot2::labeller(
      occupation_group = \(x) stringr::str_wrap(x, width = 30)
    )
  )

ggplot2::ggsave(
  "headcount_by_gender_occupation_group.png",
  path = here::here("analysis", "figs", "jupiter", "workforce"),
  device = ragg::agg_png,
  width = 12,
  height = 8
)

# movements --------------------------------------------------------------
## overall
movement <- pry_contracts_judicial |>
  mutate(
    employment_status = "active"
  ) |>
  compute_movement() |>
  collect()

movement |>
  plot_movement_joint()

ggsave(
  "movements.png"
)

## net rate
movement |>
  plot_movement_net()

ggsave(
  "movements_net.png"
)

## by occupation
movement_by_occupation <- pry_contracts_judicial |>
  left_join(
    pry_personnel |> select(personnel_id, ref_date, employment_status),
    by = c("personnel_id", "ref_date")
  ) |>
  compute_movement(
    group_cols = c("occupation_group")
  ) |>
  collect()

movement_by_occupation |>
  plot_movement_joint(
    group_cols = c("occupation_group")
  ) 

ggplot2::ggsave(
  "movements_by_occupation_group.png",
  path = here::here("analysis", "figs", "jupiter", "workforce"),
  device = ragg::agg_png,
  width = 12,
  height = 12
)

## net rate
movement_by_occupation |>
  plot_movement_net(
    group_cols = c("occupation_group")
  )

ggplot2::ggsave(
  "net_movements_by_occupation_group.png",
  path = here::here("analysis", "figs", "jupiter", "workforce"),
  device = ragg::agg_png,
  width = 12,
  height = 12
)

## by age group
movement_by_age_group <- pry_contracts_judicial |>
  left_join(
    pry_personnel |> select(personnel_id, ref_date, employment_status, age),
    by = c("personnel_id", "ref_date")
  ) |>
  filter(
    !is.na(age)
  ) |>
  classify_age_group() |>
  compute_movement(
    group_cols = c("age_group")
  ) |>
  collect()

movement_by_age_group |>
  plot_movement_joint(
    group_cols = c("age_group")
  )

ggplot2::ggsave(
  "movements_by_age_group.png",
  path = here::here("analysis", "figs", "jupiter", "workforce"),
  device = ragg::agg_png,
  width = 12,
  height = 8
)

## net rate
movement_by_age_group |>
  plot_movement_net(
    group_cols = c("age_group")
  )

ggplot2::ggsave(
  "net_movements_by_age_group.png",
  path = here::here("analysis", "figs", "jupiter", "workforce"),
  device = ragg::agg_png,
  width = 12,
  height = 8
)

## movement decomposition
movement_decomposition_by_age_group <- movement_by_age_group |>
  arrange(ref_date) |>
  mutate(
    net_change = headcount - lag(headcount, order_by = ref_date),
    net_aging = net_change - (hires - separations),
    .by = age_group
  ) |>
  filter(
    !is.na(net_change)
  )

movement_decomposition_by_age_group |>
  mutate(
    separations = -separations
  ) |>
  tidyr::pivot_longer(
    c(hires, separations, net_aging),
    names_to = "component",
    values_to = "value"
  ) |>
  ggplot(
    aes(x = ref_date)
  ) +
  geom_col(
    aes(
      y = value,
      fill = component
    ),
    color = "white",
    linewidth = 0.1,
    position = "stack"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray"
  ) +
  scale_fill_manual(
    name = "Component",
    labels = c(
      "hires" = "Hires",
      "separations" = "Separations",
      "net_aging" = "Transitions through aging"
    ),
    values = c(
      "hires" = "#202d85",
      "separations" = "#de4500",
      "net_aging" = "#f2a541"
    )
  ) +
  scale_color_manual(
    name = NULL,
    labels = c("net_change" = "Net change in headcount"),
    values = c("net_change" = "black")
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  labs(
    x = "Year",
    y = "Change in Headcount"
  ) +
  theme(
    legend.position = "bottom"
  ) +
  facet_wrap(
    vars(age_group),
    ncol = 2
  )

ggplot2::ggsave(
  "movement_decomposition_by_age_group.png",
  path = here::here("analysis", "figs", "jupiter", "workforce"),
  device = ragg::agg_png,
  width = 12,
  height = 8
)

## by gender
# movement by gender
movement_by_gender <- pry_contracts_judicial |>
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
  plot_movement_joint(
    group_cols = c("gender")
  )

ggplot2::ggsave(
  "movement_by_gender.png",
  path = here::here("analysis", "figs", "jupiter", "workforce"),
  device = ragg::agg_png,
  width = 12,
  height = 9
)

# transition -------------------------------------------------------------
# occupation
transitions_occupation_group <- pry_contracts_judicial |>
  compute_transition(
    id_col = "personnel_id",
    group_col = "occupation_group"
  ) |>
  collect()

transitions_occupation_group |>
  plot_transition(
    weight_col = "transitions"
  )

ggplot2::ggsave(
  "transitions_by_occupation_group.png",
  path = here::here("analysis", "figs", "jupiter", "workforce"),
  device = ragg::agg_png,
  width = 12,
  height = 12
)

## count transitions in and out of each group
transitions_in <- transitions_occupation_group |>
  # simplify ref_date by quarter to reduce number of x-axis ticks
  mutate(
    ref_date = lubridate::floor_date(ref_date, unit = "quarter")
  ) |>
  summarise(
    transitions = sum(transitions),
    .by = c(from, ref_date)
  ) |>
  rename(
    occupation_group = from
  )
  
transitions_out <- transitions_occupation_group |>
  mutate(
    ref_date = lubridate::floor_date(ref_date, unit = "quarter")
  ) |>
  summarise(
    transitions = -sum(transitions),
    .by = c(to, ref_date)
  ) |>
  rename(
    occupation_group = to
  )

transitions_in_and_out <- transitions_in |>
  inner_join(
    transitions_out,
    by = c("occupation_group", "ref_date"),
    suffix = c("_in", "_out")
  )

transitions_in_and_out |>
  ggplot(
    aes(
      x = ref_date
    )
  ) +
  geom_col(
    aes(
      y = transitions_in,
      fill = "Transitions In"
    )
  ) +
  geom_col(
    aes(
      y = transitions_out,
      fill = "Transitions Out"
    )
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray"
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Transitions In" = "#202d85",
      "Transitions Out" = "#de4500"
    )
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  labs(
    x = "Year",
    y = "Transitions"
  ) +
  theme(
    legend.position = "bottom"
  ) +
  facet_wrap(
    vars(occupation_group),
    ncol = 2,
    labeller = ggplot2::labeller(
      occupation_group = \(x) stringr::str_wrap(x, width = 30)
    )
  )

ggsave(
  "transitions_in_and_out_by_occupation_group.png"
)

## alluvial
transitions_occupation |>
  summarise(
    transitions = sum(transitions),
    .by = c(from, to)
  ) |>
  ggplot(
    aes(
      axis1 = from,
      axis2 = to,
      y = transitions
    )
  ) +
  ggalluvial::geom_alluvium(
    aes(fill = from)
  ) +
  ggalluvial::geom_stratum() +
  # stat = "stratum" only resolves when ggalluvial is attached, and the script
  # namespaces it instead
  geom_text(
    stat = ggalluvial::StatStratum,
    aes(label = after_stat(stratum))
  ) +
  scale_x_discrete(
    limits = c("Original occupation", "Next occupation")
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  scale_fill_manual(
    name = "Original occupation",
    values = c(
      "juez de paz" = "#202d85",
      "juez de primera instancia" = "#de4500",
      "miembro de tribunal" = "#f2a541",
      "otros" = "#6c757d"
    )
  ) +
  labs(
    y = "Transitions",
    fill = "Original occupation"
  )

ggsave(
  "transitions_by_occupation_alluvial.png"
)

## disaggregated by gender
transitions_occupation_gender <- pry_contracts_judicial |>
  detect_transition(
    id_col = "personnel_id",
    group_col = "occupation_group"
  ) |>
  inner_join(
    pry_personnel |> select(personnel_id, ref_date, gender),
    by = c("personnel_id", "ref_date")
  ) |>
  collect()

transitions_occupation_gender |>
  summarise(
    transitions = n(),
    .by = c(from, to, gender)
  ) |>
  mutate(
    transition_rate = transitions / sum(transitions, na.rm = TRUE),
    .by = gender
  ) |>
  ggplot(
    aes(
      axis1 = from,
      axis2 = to,
      y = transition_rate
    )
  ) +
  ggalluvial::geom_alluvium(
    aes(fill = from)
  ) +
  ggalluvial::geom_stratum(
    width = 1/2
  ) +
  geom_text(
    aes(
      label = after_stat(
        stringr::str_wrap(stratum, width = 20)
      )
    ),
    stat = "stratum"
  ) +
  scale_x_discrete(
    limits = c("Original occupation", "Next occupation")
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.1)
  ) +
  scale_fill_manual(
    name = "Original occupation",
    values = c(
      "juez de paz" = "#202d85",
      "juez de primera instancia" = "#de4500",
      "miembro de tribunal" = "#f2a541",
      "otros" = "#6c757d"
    )
  ) +
  labs(
    y = "Transitions"
  ) +
  theme(
    legend.position = "none"
  ) +
  facet_wrap(
    vars(gender),
    ncol = 1
  )

ggplot2::ggsave(
  "transitions_by_occupation_gender_alluvial.png",
  path = here::here("analysis", "figs", "jupiter", "workforce"),
  device = ragg::agg_png,
  width = 12,
  height = 10
)

# age group
transitions_age_group <- pry_contracts_judicial |>
  left_join(
    pry_personnel |> select(personnel_id, ref_date, age),
    by = c("personnel_id", "ref_date")
  ) |>
  filter(
    !is.na(age)
  ) |>
  classify_age_group() |>
  compute_transition(
    id_col = "personnel_id",
    group_col = "age_group"
  ) |>
  collect()

transitions_age_group |>
  mutate(
    from_to = paste(from, to, sep = " -> ")
  ) |>
  ggplot(
    aes(
      ref_date,
      transitions,
      color = from_to
    )
  ) +
  geom_line() +
  geom_point() +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_color_manual(
    name = "Transition",
    values = c(
      "<30 -> 30-39" = "#202d85",
      "30-39 -> 40-49" = "#de4500",
      "40-49 -> 50-59" = "#f2a541",
      "50-59 -> 60+" = "#6c757d"
    )
  ) +
  facet_wrap(
    vars(from_to),
    ncol = 2
  )

ggplot2::ggsave(
  "transitions_by_age_group.png",
  path = here::here("analysis", "figs", "jupiter", "workforce"),
  device = ragg::agg_png,
  width = 12,
  height = 8
)
