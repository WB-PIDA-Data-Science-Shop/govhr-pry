# parse working hours --------------------------------------------------------
# convert free-text schedules (carga_horaria) into weekly working hours, e.g.
#   "07:00 A 12:00"                                      -> 25 (monday to friday by default)
#   "LUNES A JUEVES 07:00 A 15:00 VIERNES 07:00 A 13:00" -> 38
#   "S Y D 06:00 A 18:00"                                -> 24
#   "(GUARDIA) 18:00 A 06:00 C/ 72 HS", "24 X 48"        -> 28, 56 (shift cycles)
#   "5 HORAS CATEDRA SEMANALES", "6 HORAS DIARIAS"       -> 5, 30
# schedules that cannot be converted ("M", "TURNO ROTATIVO", "HORAS CATEDRA") are NA

day_token <- paste0(
  "\\b(?:",
  "LUNES|LUN|LU|L|MARTES|MAR|MA|M|MIERCOLES|MI\\S{0,3}RCOLES|MIER|MIE|MI|",
  "JUEVES|JUE|JU|J|VIERNES|VIER|VIE|VI|V|SABADOS?|SAB|SA|S|DOMINGOS?|DOM|DO|D",
  ")\\b\\.?"
)
day_span <- paste0("(", day_token, ")\\s?(?:A|AL|-|HASTA)\\s?(", day_token, ")")

# day of the week (1 = monday) of a day token
day_index <- function(token) {
  first <- ifelse(startsWith(token, "MI"), "X", substr(token, 1, 1))
  match(first, c("L", "M", "X", "J", "V", "S", "D"))
}

# number of days of the week mentioned in each text:
# spans ("LUNES A VIERNES" = 5) plus single days ("LUNES Y MIERCOLES" = 2)
count_days <- function(text) {
  n <- length(text)
  if (n == 0) return(integer())
  spans <- stringr::str_match_all(text, day_span)
  span_id <- rep(seq_len(n), purrr::map_int(spans, nrow))
  spans <- do.call(rbind, spans)
  span_days <- (day_index(spans[, 3]) - day_index(spans[, 2])) %% 7 + 1
  n_span <- tabulate(rep(span_id, span_days), nbins = n)

  singles <- stringr::str_extract_all(stringr::str_remove_all(text, day_span), day_token)
  single_id <- rep(seq_len(n), lengths(singles))
  single_day <- day_index(unlist(singles))
  n_single <- tabulate(single_id[!duplicated(cbind(single_id, single_day))], nbins = n)

  n_span + n_single
}

# fill NAs with the previous (then next) non-NA value within each group
fill_by <- function(x, group) {
  pos <- seq_along(x)
  prev <- cummax(ifelse(is.na(x), 0L, pos))
  fill <- is.na(x) & prev > 0
  fill[fill] <- group[fill] == group[prev[fill]]
  x[fill] <- x[prev[fill]]
  nxt <- rev(cummin(rev(ifelse(is.na(x), length(x) + 1L, pos))))
  fill <- is.na(x) & nxt <= length(x)
  fill[fill] <- group[fill] == group[nxt[fill]]
  x[fill] <- x[nxt[fill]]
  x
}

parse_whours <- function(x) {
  if (length(x) == 0) return(numeric())
  s <- x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_upper() |>
    stringr::str_squish() |>
    dplyr::coalesce("") |>
    # 07: 00 -> 07:00; drop seconds: 07:00:00 -> 07:00
    stringr::str_replace_all("(\\d):\\s(\\d)", "\\1:\\2") |>
    stringr::str_replace_all("(\\d{1,2}:\\d{1,2}):\\d{2}", "\\1") |>
    # 07-00 A 13-00 -> 07:00 A 13:00
    stringr::str_replace_all("\\b(\\d{1,2})-(\\d{2})\\s?A\\s?(\\d{1,2})-(\\d{2})\\b", "\\1:\\2 A \\3:\\4") |>
    # 0700 -> 07:00
    stringr::str_replace_all("(?<![\\d:.,])(\\d{2})(\\d{2})(?![\\d:.,])", "\\1:\\2") |>
    # 07.00 / 07 00 -> 07:00
    stringr::str_replace_all("(?<![\\d:.,])(\\d{1,2})[. ](\\d{2})(?![\\d:.,])", "\\1:\\2") |>
    # drop hour suffixes after clock times: 07:00HS, 15:00 HORAS, 7 HS A 13 HS
    stringr::str_replace_all("(\\d:\\d{1,2})\\s?(?:HORAS|HRS|HS|H)\\b\\.?", "\\1") |>
    stringr::str_replace_all("\\b(\\d{1,2})\\s?(?:HRS|HS|H)\\b\\.?(?=\\s?(?:A|-|AL|HASTA)\\b)", "\\1") |>
    stringr::str_replace_all("(?<=\\b(?:A|AL|HASTA)\\s)(\\d{1,2})\\s?(?:HRS|HS|H)\\b\\.?", "\\1") |>
    # spelled-out numbers used in shift cycles
    stringr::str_replace_all(c("\\bDOS\\b" = "2", "\\bTRES\\b" = "3", "\\bCUATRO\\b" = "4"))

  # 1. explicit hours: "5 HORAS CATEDRA SEMANALES", "REF 6 HORAS DIARIAS", "16 HS MES"
  n_hours <- "(\\d+(?:[.,]\\d+)?)\\s?(?:HORAS?|HRS?|HS)\\.?\\s?(?:CATEDRAS?\\s?|RELOJ\\s?)?"
  as_num <- \(v) as.numeric(stringr::str_replace(v, ",", "."))
  weekly <- stringr::str_match(s, paste0(n_hours, "(?:SEMANAL(?:ES)?|POR SEMANA|X SEMANA|A LA SEMANA)"))[, 2]
  daily <- stringr::str_match(s, paste0(n_hours, "(?:DIARIAS?|POR DIA|X DIA|AL DIA)"))[, 2]
  monthly <- stringr::str_match(s, paste0(n_hours, "(?:MES|MENSUAL(?:ES)?|AL MES|POR MES|X MES)\\b"))[, 2]

  # 2. shift cycles: "24 X 48" (24h on, 48h off)
  on_off <- stringr::str_match(s, "\\b(\\d{1,2})\\s?X\\s?(\\d{2,3})\\b")
  on_off <- as_num(on_off[, 2]) * 168 / (as_num(on_off[, 2]) + as_num(on_off[, 3]))
  # "C/ 72 HS", "CADA 72 HORAS", "CADA 3 NOCHES", "DIAS ALTERNOS": one shift every n hours
  every <- stringr::str_match(s, "\\b(?:CADA|C/?)\\s?(\\d{1,3})\\s?(HORAS|HS|H|NOCHES?|DIAS?)\\b")
  every <- as_num(every[, 2]) * dplyr::if_else(stringr::str_starts(every[, 3], "H"), 1, 24)
  every[stringr::str_detect(s, "ALTERNO|DIA POR MEDIO|DIA DE POR MEDIO")] <- 48

  # 3. time ranges: "07:00 A 15:00", "7 A 13", "07:00-13:00", "DE 07:00 HASTA 13:00"
  range_regex <- paste0(
    "(?<![\\d:])(\\d{1,2})(?::(\\d{1,2}))?\\s?(?:A|-|AL|HASTA)\\s?(?:LAS\\s)?",
    "(\\d{1,2})(?::(\\d{1,2}))?(?![\\d:])"
  )
  matches <- stringr::str_match_all(s, range_regex)
  locs <- stringr::str_locate_all(s, range_regex)
  n_ranges <- purrr::map_int(matches, nrow)
  matches <- do.call(rbind, matches)
  locs <- do.call(rbind, locs)

  id <- rep(seq_along(s), n_ranges)
  text <- s[id]
  first <- id != dplyr::lag(id, default = 0L)
  last <- id != dplyr::lead(id, default = 0L)
  start_m <- dplyr::coalesce(as.numeric(matches[, 3]), 0)
  end_m <- dplyr::coalesce(as.numeric(matches[, 5]), 0)
  start <- as.numeric(matches[, 2]) + start_m / 60
  end <- as.numeric(matches[, 4]) + end_m / 60
  # overnight shifts wrap around, e.g. 18:00 A 06:00
  dur <- (end - start) %% 24
  valid <- start <= 24 & end <= 24 & start_m < 60 & end_m < 60 & dur > 0

  # days attached to each range: those written before it ("LUNES A VIERNES 07:00 A 13:00")
  # or, if the schedule starts with a time, after it ("07:00 A 13:00 LUN A VIE")
  days_before <- count_days(stringr::str_sub(
    text, dplyr::if_else(first, 1L, dplyr::lag(locs[, 2]) + 1L), locs[, 1] - 1L
  ))
  days_after <- count_days(stringr::str_sub(
    text, locs[, 2] + 1L, dplyr::if_else(last, -1L, dplyr::lead(locs[, 1]) - 1L)
  ))
  days <- dplyr::if_else(days_before[first][match(id, unique(id))] > 0, days_before, days_after)
  # ranges with no days of their own share those of the previous range (split shifts)
  days <- fill_by(dplyr::na_if(days, 0L), id)
  max_days <- as.vector(tapply(dplyr::coalesce(days, 0L), id, max))

  # without days, weekday schedule: monday to friday
  hours <- as.vector(tapply(dplyr::if_else(valid, dur * dplyr::coalesce(days, 5L), 0), id, sum))
  daily_hours <- as.vector(tapply(dplyr::if_else(valid, dur, 0), id, sum))
  ids <- unique(id)
  rotating <- stringr::str_detect(s[ids], "GUARDIA|ROTATIV")

  from_ranges <- rep(NA_real_, length(s))
  from_ranges[ids] <- dplyr::case_when(
    daily_hours == 0 ~ NA_real_,
    # one shift every n hours
    !is.na(every[ids]) ~ daily_hours * 168 / every[ids],
    # guards and rotations without specific days are not worked every day
    rotating & max_days %in% c(0, 7) ~ NA_real_,
    .default = hours
  )

  out <- dplyr::coalesce(as_num(weekly), 5 * as_num(daily), as_num(monthly) * 12 / 52, on_off, from_ranges)
  # govhr quality rule: weekly hours between 0 and 168
  dplyr::if_else(out > 0 & out <= 168, out, NA_real_)
}
