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

# wagebill analytics -----------------------------------------------------
wagebill <- pry_contracts_executive |>
  mutate(
    employment_status = "active"
  ) |>
  compute_wagebill(
    measure_col = "gross_salary_lcu"
  ) |>
  collect() |>
  arrange(
    ref_date
  ) |>
  mutate(
    wagebill_growth = wagebill / lag(wagebill) - 1
  )

wagebill |>
  plot_trend(
    group_col = c("ref_date"),
    y_col = "wagebill",
    y_label = "Wagebill (real ₲, Dec 2021)"
  ) +
  coord_cartesian(
    ylim = c(0, NA)
  )

ggsave(
  "wagebill.png"
)

# by type of salary
wagebill_salary <- pry_contracts_executive |>
  mutate(
    employment_status = "active"
  ) |>
  compute_wagebill(
    measure_col = "base_salary_lcu"
  ) |>
  collect() |>
  select(
    ref_date,
    wagebill_base_salary = wagebill,
    growth_base_salary = wagebill_growth
  ) |>
  inner_join(
    pry_contracts_executive |>
      mutate(
        employment_status = "active"
      ) |>
      compute_wagebill(
        measure_col = "allowance_lcu"
      ) |>
      collect() |>
      select(
        ref_date,
        wagebill_allowance = wagebill,
        growth_allowance = wagebill_growth
      ),
    by = "ref_date"
  )

wagebill_salary |>
  tidyr::pivot_longer(
    c(wagebill_base_salary, wagebill_allowance),
    names_to = "component",
    values_to = "wagebill"
  ) |>
  mutate(
    component = recode(
      component,
      "wagebill_base_salary" = "Base Salary",
      "wagebill_allowance" = "Allowances"
    )
  ) |>
  mutate(
    component = forcats::fct_relevel(
      component,
      "Base Salary",
      "Allowances"
    )
  ) |>
  plot_trend(
    group_col = c("component"),
    y_col = "wagebill",
    y_label = "Wagebill (real ₲, Dec 2021)"
  ) +
  scale_color_manual(
    name = "Salary Component",
    values = c(
      "Base Salary" = "#202d85",
      "Allowances" = "#de4500"
    )
  ) +
  facet_wrap(
    vars(component)
  ) +
  theme(
    legend.position = "none"
  ) +
  coord_cartesian(
    ylim = c(0, NA)
  )

ggsave(
  "wagebill_by_salary_component.png"
)

# by establishment
wagebill_est <- pry_contracts_executive |>
  mutate(
    employment_status = "active"
  ) |>
  compute_wagebill(
    group_cols = c("est_id")
  ) |>
  left_join(
    pry_est_name,
    by = c("est_id")
  ) |>
  collect()

wagebill_est |>
  filter(
    !is.na(wagebill)
  ) |>
  mutate(
    est_name_native = forcats::fct_lump(
      est_name_native,
      n = 6,
      w = wagebill,
      other_level = "Other"
    ) |>
      forcats::fct_reorder(
        wagebill,
        .fun = sum,
        .desc = TRUE
      )
  ) |>
  summarise(
    wagebill = sum(wagebill),
    .by = c(ref_date, est_name_native)
  ) |>
  plot_trend(
    group_col = "est_name_native",
    y_col = "wagebill",
    y_label = "Wagebill (real ₲, Dec 2021)"
  ) +
  theme(
    legend.position = "bottom"
  ) +
  ggthemes::scale_color_solarized() +
  ggplot2::guides(
    color = ggplot2::guide_legend(
      title = "Establishment",
      title.position = "top",
      ncol = 2,
      override.aes = list(linewidth = 6)
    )
  )

ggsave(
  "wagebill_by_est.png"
)

# growth
wagebill |>
  plot_trend(
    group_col = c("ref_date"),
    y_col = "wagebill_growth",
    y_label = "Wagebill Growth Rate"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.1)
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray"
  )

ggsave(
  "wagebill_growth.png"
)

# growth by salary component
wagebill_salary |>
  tidyr::pivot_longer(
    c(growth_base_salary, growth_allowance),
    names_to = "component",
    values_to = "growth_rate"
  ) |>
  mutate(
    component = recode(
      component,
      "growth_base_salary" = "Base Salary",
      "growth_allowance" = "Allowances"
    )
  ) |>
  mutate(
    component = forcats::fct_relevel(
      component,
      "Base Salary",
      "Allowances"
    )
  ) |>
  plot_trend(
    group_col = c("component"),
    y_col = "growth_rate",
    y_label = "Wagebill Growth Rate"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.1)
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray"
  ) +
  scale_color_manual(
    name = "Salary Component",
    values = c(
      "Base Salary" = "#202d85",
      "Allowances" = "#de4500"
    )
  ) +
  facet_wrap(
    vars(component)
  ) +
  theme(
    legend.position = "none"
  )

## decomposition
wagebill_growth_decomposition <- pry_contracts_executive |>
  compute_growth_decomposition(
    measure_col = "base_salary_lcu"
  ) |>
  collect()

wagebill_growth_decomposition |>
  tidyr::pivot_longer(
    c(employment_effect, compensation_effect, interaction_effect),
    names_to = "effect_type",
    values_to = "effect_value"
  ) |>
  ggplot(
    aes(
      x = ref_date,
      y = effect_value,
      fill = effect_type,
      group = effect_type
    )
  ) +
  geom_col(
    position = "stack"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray"
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale()
    )
  ) +
  scale_fill_manual(
    name = "Effect Type",
    labels = c(
      "employment_effect" = "Employment Effect",
      "compensation_effect" = "Compensation Effect",
      "interaction_effect" = "Interaction Effect"
    ),
    values = c(
      "employment_effect" = "#202d85",
      "compensation_effect" = "#de4500",
      "interaction_effect" = "#f5a623"
    )
  ) +
  labs(
    x = "Time",
    y = "Magnitude of Effect"
  )

# compensation
## overall
pry_compensation <- pry_contracts_executive |>
  compute_compensation(
    measure_col = "gross_salary_lcu"
  ) |>
  collect()

pry_compensation |>
  plot_trend(
    group_col = c("ref_date"),
    y_col = "compensation",
    y_label = "Average compensation"
  )

## by salary component
pry_compensation_by_component <- pry_contracts_executive |>
  compute_compensation(
    measure_col = "base_salary_lcu"
  ) |>
  select(
    ref_date,
    compensation_base_salary = compensation
  ) |>
  left_join(
    pry_contracts_executive |>
      compute_compensation(
        measure_col = "allowance_lcu"
      ) |>
      select(
        ref_date,
        compensation_allowance = compensation
      ),
    by = "ref_date"
  ) |>
  collect()

pry_compensation_by_component |>
  tidyr::pivot_longer(
    c(compensation_base_salary, compensation_allowance),
    names_to = "component",
    values_to = "compensation"
  ) |>
  mutate(
    component = recode(
      component,
      "compensation_base_salary" = "Base Salary",
      "compensation_allowance" = "Allowances"
    )
  ) |>
  mutate(
    component = forcats::fct_relevel(
      component,
      "Base Salary",
      "Allowances"
    )
  ) |>
  plot_trend(
    group_col = c("component"),
    y_col = "compensation",
    y_label = "Average compensation"
  ) +
  scale_color_manual(
    name = "Salary Component",
    values = c(
      "Base Salary" = "#202d85",
      "Allowances" = "#de4500"
    )
  ) +
  facet_wrap(
    vars(component)
  ) +
  theme(
    legend.position = "none"
  )

ggsave(
  "compensation_by_salary_component.png"
)

## by contract type
pry_compensation_by_contract <- pry_contracts_executive |>
  filter(
    !is.na(contract_type)
  ) |>
  compute_compensation(
    measure_col = "gross_salary_lcu",
    group_cols = c("contract_type")
  ) |>
  collect()

pry_compensation_by_contract |>
  mutate(
    contract_type = forcats::fct_relevel(
      contract_type,
      "permanent",
      "fixed-term"
    ) |>
      recode(
        "permanent" = "Permanent",
        "fixed-term" = "Fixed-Term"
      )
  ) |>
  plot_trend(
    group_col = c("contract_type"),
    y_col = "compensation",
    y_label = "Average compensation (real ₲, Dec 2021)"
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 1,
    color = "#f5a623"
  ) +
  scale_color_manual(
    name = "Type of Contract",
    values = c(
      "Fixed-Term" = "#de4500",
      "Permanent" = "#202d85"
    )
  ) +
  facet_wrap(
    vars(contract_type)
  ) +
  theme(
    legend.position = "none"
  )

ggsave(
  "compensation_by_contract_type.png"
)

## by gender
pry_compensation_by_gender <- pry_contracts_executive |>
  left_join(
    pry_personnel |>
      select(
        personnel_id,
        ref_date,
        gender
      ),
    by = c("personnel_id", "ref_date")
  ) |>
  filter(
    !is.na(gender)
  ) |>
  compute_compensation(
    measure_col = "gross_salary_lcu",
    group_cols = c("gender")
  ) |>
  collect()

pry_compensation_by_gender |>
  mutate(
    gender = forcats::fct_relevel(
      gender,
      "FEMENINO",
      "MASCULINO"
    )
  ) |>
  plot_trend(
    group_col = c("gender"),
    y_col = "compensation",
    y_label = "Average compensation (real ₲, Dec 2021)"
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 1,
    color = "#f5a623"
  ) +
  scale_color_manual(
    name = "Gender",
    values = c(
      "FEMENINO" = "#de4500",
      "MASCULINO" = "#202d85"
    )
  ) +
  facet_wrap(
    vars(gender)
  ) +
  theme(
    legend.position = "none"
  )

ggsave(
  "compensation_by_gender.png"
)

# compensation distribution
## overall
compensation_distribution <- pry_contracts_executive |>
 compute_density(
    measure_col = "gross_salary_lcu",
    binwidth = 1e6
  ) |>
  collect() 

compensation_distribution |>
  plot_histogram(
    group_col = c("ref_date")
  ) +
  scale_x_continuous(
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale()
    )
  ) +
  coord_cartesian(
    xlim = c(0, 2.5e7)
  )

ggsave(
  "compensation_distribution.png"
)

## by gender
compensation_distribution_by_gender <- pry_contracts_executive |>
  left_join(
    pry_personnel |>
      select(
        personnel_id,
        ref_date,
        gender
      ),
    by = c("personnel_id", "ref_date")
  ) |>
  filter(
    !is.na(gender)
  ) |>
  compute_density(
    measure_col = "gross_salary_lcu",
    group_cols = c("gender"),
    binwidth = 1e6
  ) |>
  collect()

compensation_distribution_by_gender |>
  plot_histogram(
    group_col = c("gender")
  ) +
  geom_col(
    aes(
      fill = gender
    )
  ) +
  scale_x_continuous(
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale()
    )
  ) +
  scale_fill_manual(
    name = "Gender",
    values = c(
      "FEMENINO" = "#de4500",
      "MASCULINO" = "#202d85"
    )
  ) +
  coord_cartesian(
    xlim = c(0, 2.5e7)
  )

ggsave(
  "compensation_distribution_by_gender.png"
)

# wage composition
## base salary vs. allowances
wagebill_base_salary <- pry_contracts_executive |>
  mutate(
    employment_status = "active"
  ) |>
  compute_wagebill(
    measure_col = "base_salary_lcu"
  ) |>
  collect() |>
  select(
    ref_date,
    base_salary = wagebill
  )

wagebill_allowance <- pry_contracts_executive |>
  mutate(
    employment_status = "active"
  ) |>
  compute_wagebill(
    measure_col = "allowance_lcu"
  ) |>
  collect() |>
  select(
    ref_date,
    allowance = wagebill
  )

wagebill_by_component <- wagebill_base_salary |>
  left_join(
    wagebill_allowance,
    by = c("ref_date")
  ) |>
  mutate(
    total_wagebill = base_salary + allowance,
    share_base_salary = base_salary / total_wagebill,
    share_allowance = allowance / total_wagebill
  ) |>
  tidyr::pivot_longer(
    c(share_base_salary, share_allowance),
    names_to = "component",
    values_to = "share_wagebill"
  ) 

wagebill_by_component |>
  ggplot(
    aes(
      x = ref_date,
      y = share_wagebill,
      fill = component,
      group = component
    )
  ) +
  geom_area(
    position = "stack"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.1)
  ) +
  theme(
    legend.position = "bottom"
  ) +
  scale_fill_manual(
    name = "Wagebill Component",
    labels = c(
      "share_base_salary" = "Base Salary",
      "share_allowance" = "Allowances"
    ),
    values = c(
      "share_base_salary" = "#202d85",
      "share_allowance" = "#de4500"
    )
  )

ggsave(
  "wagebill_share_by_component.png"
)
