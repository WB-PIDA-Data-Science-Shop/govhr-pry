classify_occupation_group <- function(data) {
  # classifying distinct occupations and joining back keeps the regex work
  # proportional to the number of unique titles rather than the number of rows
  occupation_group <- data |>
    distinct(occupation) |>
    mutate(
      occupation_group = case_when(
        stringr::str_detect(occupation, "MIEMBRO") &
          stringr::str_detect(occupation, "TRIB") ~ "Miembro de Tribunal",
        stringr::str_detect(occupation, "JUEZ") &
          stringr::str_detect(occupation, "PRIMERA|1") &
          stringr::str_detect(occupation, "INST") ~ "Juez de Primera Instancia",
        stringr::str_detect(occupation, "JUEZ") &
          stringr::str_detect(occupation, "PAZ") ~ "Juez de Paz",
        .default = "Otros"
      )
    )

  data |>
    left_join(occupation_group, by = "occupation") |>
    # SQL joins never match NULL keys, so missing occupations need a fallback
    mutate(occupation_group = coalesce(occupation_group, "Otros"))
}
