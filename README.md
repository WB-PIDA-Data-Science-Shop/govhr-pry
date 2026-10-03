# govhr-pry

This repository cleans, harmonizes and analyzes Paraguay's public-sector HR microdata. 

The data pipeline turns the monthly civil-servant payroll files published by the Secretaría de la Función Pública (SFP) into the standard [`govhr`](https://github.com/WB-PIDA-Data-Science-Shop/govhr) data modules (establishments, personnel, contracts).

It then produces workforce and wage-bill analytics for the central government (Poder Ejecutivo).

## Repository structure

```
govhr-pry/
├── etl/
│   ├── 1-extract.R          # download every monthly funcionarios_YYYY_M.csv from datos.sfp.gov.py
│   └── 2-harmonize.R        # build the DuckDB database, one year at a time, and set primary keys
├── R/
│   ├── harmonize.R          # harmonize_hr(): raw SFP file -> govhr modules; harmonize_cpi(): monthly CPI from the IMF
│   ├── parse_whours.R       # parse_whours(): free-text schedules (carga_horaria) -> weekly working hours
│   └── analysis.R           # DuckDB-friendly versions of govhr indicators (headcount, wage bill, movements, ...)
├── analysis/
│   ├── source/
│   │   ├── workforce_analytics.R   # headcount, composition, hires/separations, transitions
│   │   ├── wagebill_analytics.R    # wage bill, growth decomposition, compensation
│   │   └── standard_analytics.R    # earlier combined script (workforce + CPI deflation)
│   └── figs/                # PNG output of the analysis scripts
├── references/              # SFP data dictionary and MEF budget classifier (PGN 2026)
├── data/                    # symlink to the shared OneDrive folder (git-ignored)
│   ├── data-raw/            # funcionarios_YYYY_M.csv.gz, as published (UTF-8)
│   └── data-clean/          # govhr_pry.duckdb
└── renv.lock                # pinned R package versions
```

## Running the pipeline

Run from the project root. `data/` must point to the shared data folder.

```r
renv::restore()              # install pinned packages, including govhr
source("etl/1-extract.R")    # ~140 monthly files
source("etl/2-harmonize.R")  # rebuilds data/data-clean/govhr_pry.duckdb from scratch
source("analysis/source/workforce_analytics.R")
source("analysis/source/wagebill_analytics.R")
```

Indicators in `R/analysis.R` are written so they run inside DuckDB (via dbplyr/duckplyr) rather than in memory. Each person-month is spread over several payroll lines, so the full panel is too large to load into R.

## Data

**Source.** SFP open data, *Funcionarios* dataset ([datos.sfp.gov.py](https://datos.sfp.gov.py/data/funcionarios/download)). There is one file per month, and each row is one payroll line: a person × position × object of expenditure. A typical month has about 1.6M rows. The column definitions are in [references/diccionario_funcionarios.pdf](references/diccionario_funcionarios.pdf).

**Time frame.** The raw data is monthly, from January 2015 to August 2026 (140 months, no gaps). The analysis scripts restrict to **January 2016 to December 2025**, because the 2015 data appears to have gaps.

**Coverage.** The database includes all branches and levels of government in the SFP files (`adm1`). The analysis keeps only `adm1 == "PODER EJECUTIVO"`.

**Money.** All amounts are in nominal guaraníes (`_lcu`) and are taken from the budgeted amount (`presupuestado`). `harmonize_cpi()` downloads the monthly headline CPI (BCP series via the IMF, rebased to Dec 2021 = 100) for deflating to real terms.

### Modules (`data/data-clean/govhr_pry.duckdb`)

All tables are monthly panels keyed by `ref_date` (first day of the month).

#### `pry_est`: establishments (key: `est_id`, `ref_date`)

| Variable | Description | Source column |
|---|---|---|
| `est_id` | `adm1_code-adm2_code-adm3_code` | |
| `adm1_code`, `adm1` | Level (e.g. Poder Ejecutivo, Poder Legislativo) | `nivel`, `descripcion_nivel` |
| `adm2_code`, `adm2` | Entity (e.g. a ministry) | `entidad`, `descripcion_entidad` |
| `adm3_code`, `adm3` | Organism / executing unit (OEE) | `oee`, `descripcion_oee` |
| `est_name_native` | Establishment name (= `adm3`) | |
| `country_code`, `country_name` | `PRY`, `Paraguay` | |

#### `pry_personnel`: people (key: `personnel_id`, `ref_date`)

| Variable | Description | Source column |
|---|---|---|
| `personnel_id` | National ID number | `documento` |
| `birth_date`, `age` | Date of birth; age in years at `ref_date` | `fecha_nacimiento` |
| `gender` | `FEMENINO` / `MASCULINO` | `sexo` |
| `first_employment_date` | Year of entry into the public service (earliest value reported for the person) | `anho_ingreso` |
| `employment_status` | Always `active` (only people on payroll appear) | |

#### `pry_contracts`: positions and pay (key: `contract_id`, `ref_date`)

A contract is one person in one occupation at one establishment.

| Variable | Description | Source column |
|---|---|---|
| `contract_id` | SHA-256 of `est_id` + `personnel_id` + `occupation` | |
| `est_id`, `personnel_id` | Links to `pry_est` and `pry_personnel` | |
| `contract_type` | `permanent` (PERMANENTE) or `fixed-term` (CONTRATADO, COMISIONADO) | `estado` |
| `paygrade` | Budget category; `NA` if *sin categoría* | `categoria` |
| `occupation` | Position title, normalized (upper case, no accents) | `cargo` |
| `base_salary_lcu` | Base salary: object of expenditure 111 | `presupuestado` |
| `allowance_lcu` | All other personnel expenditure objects (1xx): bonuses, allowances, etc. | `presupuestado` |
| `gross_salary_lcu` | `base_salary_lcu` + `allowance_lcu` | `presupuestado` |
| `whours` | Weekly working hours parsed from free text (`NA` if unparseable or outside 0–168) | `carga_horaria` |

Only objects of expenditure in group 1 (*servicios personales*) count as pay. See [references/CLASIFICADOR _PRESUPUESTARIO_PGN2026.pdf](references/CLASIFICADOR%20_PRESUPUESTARIO_PGN2026.pdf).

## Analysis

The analysis scripts follow the standard `govhr` diagnostic of public-sector employment and pay. Each produces the figures in [analysis/figs/](analysis/figs/).

**Workforce** ([workforce_analytics.R](analysis/source/workforce_analytics.R))
- Headcount over time: total, by establishment, by contract type, and by age group.
- Workforce movements: monthly hire and separation rates, overall and for the nine largest establishments, plus net rates.
- Career transitions: a network of moves between establishments.

**Wage bill and compensation** ([wagebill_analytics.R](analysis/source/wagebill_analytics.R))
- Wage bill level and growth: total, by establishment, and split into base salary and allowances.
- Composition of the wage bill: share paid as base salary vs. allowances.
- Growth decomposition of the wage bill into employment, compensation and interaction effects.
- Average compensation: by salary component, contract type and gender.
- Compensation distribution: overall and by gender.
