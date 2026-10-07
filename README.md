# govhr-pry

This repository cleans, harmonizes and analyzes Paraguay's payroll data. It comprises three main components:

1. An ETL pipeline that extracts the monthly payroll files, harmonizes them and loads them into a database with the standard [`govhr`](https://github.com/WB-PIDA-Data-Science-Shop/govhr) data modules.
2. An analytical pipeline that computes workforce and wage bill indicators.
3. A set of applications that visualize the indicators and allow users to explore the data (to-do).

## Repository structure

The repository is organized as follows:

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
│   │   ├── wagebill_analytics.R    # wage bill, growth decomposition, wages
│   │   └── standard_analytics.R    # earlier combined script (workforce + CPI deflation)
│   └── figs/                # PNG output of the analysis scripts
├── references/              # SFP data dictionary and MEF budget classifier (PGN 2026)
├── data/                    # symlink to the shared OneDrive folder (git-ignored)
│   ├── data-raw/            # funcionarios_YYYY_M.csv.gz, as published (UTF-8)
│   └── data-clean/          # govhr_pry.duckdb
└── renv.lock                # pinned R package versions
```

## Running the pipeline

The repository is fully reproducible from your local machine. The steps below reproduce components 1 and 2 outlined above:

```r
# install.packages("renv") # if not already installed

renv::restore()              # install pinned packages, including govhr

# component 1. ETL pipeline
source("etl/1-extract.R")    # ~140 monthly files
source("etl/2-harmonize.R")  # rebuilds data/data-clean/govhr_pry.duckdb from scratch

# component 2. Analytical pipeline
source("analysis/source/workforce_analytics.R")
source("analysis/source/wagebill_analytics.R")

# component 3. Applications (to-do)
```

## Data

**Source.** All data analyzed in this repository is publicly available in the SFP open data, *Funcionarios* dataset ([datos.sfp.gov.py](https://datos.sfp.gov.py/data/funcionarios/download)). There is one file per month, and each row is one payroll line: a person × position × object of expenditure. A typical month has about 1.6M rows. The column definitions are in [references/diccionario_funcionarios.pdf](references/diccionario_funcionarios.pdf).

**Time frame.** The raw data is monthly, from January 2015 to August 2026 (140 months).

**Coverage.** The database includes all branches and levels of government in the SFP files (`adm1`).

**Paychecks.** All paycheck amounts are in nominal guaraníes.

### Modules

All harmonized modules are monthly panels indexed by `ref_date` (first day of the month).

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

The analysis adopts the standard `govhr` assessment of public sector workforce and wage bill. Each produces the figures in [analysis/figs/](analysis/figs/).

**Workforce** ([workforce_analytics.R](analysis/source/workforce_analytics.R))
- Headcount: overall and by group.
- Movements: external (hires and separations) and internal (transitions).
- Lifecycle: public service expectancy and trajectory.

**Wage bills** ([wagebill_analytics.R](analysis/source/wagebill_analytics.R))
- Wage bill: overall and by group.
- Wage bill growth: overall and by group, decomposition.
- Wage: overall and by group.
- Wage growth: overall and by group.
- Wage structure: composition (base salary vs. allowances) and distribution (histogram, compression ratio).
