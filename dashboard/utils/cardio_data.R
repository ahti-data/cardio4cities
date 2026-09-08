#' Cardio4Cities data loading and pure helper functions.
#'
#' Loads the pre-aggregated, already-suppressed output of the CBS Remote
#' Access pipeline (`code/06_make_desritpives.R` in the project bundle) --
#' committed as `data/cardio4cities_outcomes.csv`, one long-format row per
#' (breakdown sheet, category, year, outcome, metric type). Every count here
#' is a whole-population registry count that has already been through the
#' pipeline's own CBS output rules (`drop_if_below_x()` in
#' `code/00_inputs.R`: any cell with fewer than 10 people is dropped, and
#' every denominator is rounded to the nearest 10) -- this file never reads
#' anything from CBS Remote Access directly, only this already-cleared
#' export.
#'
#' Kept in `utils/` (not inlined in `app.R`) per CLAUDE.md's "Add reusable
#' logic to utils/, not inline in app.R", and so its pure functions (no
#' Shiny reactivity) are unit-testable in `tests/testthat/test-cardio_data.R`.

#' Default path to the committed outcomes export.
C4C_OUTCOMES_FILE <- file.path("data", "cardio4cities_outcomes.csv")

#' Read and type the outcomes export.
#' @param path Path to the CSV (see [C4C_OUTCOMES_FILE]).
#' @return A tibble with columns `breakdown`, `groep`, `year`, `n_totaal`,
#'   `variable`, `value`, `name`, `type`.
c4c_load_outcomes <- function(path = C4C_OUTCOMES_FILE) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character")
  df$year <- as.integer(df$year)
  df$n_totaal <- as.numeric(df$n_totaal)
  df$value <- as.numeric(df$value)
  tibble::as_tibble(df)
}

#' Whether an outcome's raw name is binary (`heeft_...`, exported only as a
#' user count) or continuous (`n_...`, exported as both a user count and a
#' summed count) -- see `make_aggregated_data()` in `code/00_inputs.R`.
#' @param name Raw outcome column name.
#' @return `"binary"` or `"continuous"`.
c4c_outcome_kind <- function(name) {
  ifelse(grepl("^heeft_", name), "binary", "continuous")
}

#' Human-readable label for one outcome, from [C4C_OUTCOME_META]. Falls back
#' to the raw name (never errors) for a name not yet in the metadata table.
#' @param name Raw outcome column name.
c4c_outcome_label <- function(name, meta = C4C_OUTCOME_META) {
  vapply(name, function(n) {
    hit <- meta[[n]]
    if (is.null(hit)) n else hit$label
  }, character(1), USE.NAMES = FALSE)
}

#' Grouped choices for an indicator `selectInput`: a named list (one
#' `<optgroup>` per [C4C_OUTCOME_GROUPS] entry, in that order) of named
#' character vectors (`label = raw_name`), sorted by label within each
#' group.
#' @return Named list suitable for `shiny::selectInput(choices = ...)`.
c4c_outcome_choices <- function(meta = C4C_OUTCOME_META, groups = C4C_OUTCOME_GROUPS) {
  names_by_group <- split(names(meta), vapply(meta, function(m) m$group, character(1)))
  stats::setNames(
    lapply(names(groups), function(g) {
      nms <- names_by_group[[g]]
      if (is.null(nms)) return(character(0))
      labels <- c4c_outcome_label(nms, meta)
      stats::setNames(nms, labels)[order(labels)]
    }),
    unname(groups)
  )
}

#' Metric choices available for an outcome's kind, as a named vector for
#' `shiny::selectInput(choices = ...)`. The first entry is the sensible
#' default.
#' @param kind `"binary"` or `"continuous"` (see [c4c_outcome_kind()]).
c4c_available_metrics <- function(kind) {
  if (identical(kind, "continuous")) {
    c("Aantal per 1.000 inwoners" = "rate_per_1000", "Totaal aantal (opnames)" = "absolute")
  } else {
    c("Aandeel van de bevolking (%)" = "percentage", "Aantal personen" = "absolute")
  }
}

#' Which `type` value (see `c4c_filter_outcome()`'s `outcome_type`) an
#' outcome's kind is stored under. A continuous outcome (`n_...`) is always
#' read from `"sum_totaal_groep"` here, whichever metric
#' ([c4c_available_metrics()]'s `"rate_per_1000"` or `"absolute"`) the user
#' picked -- both describe the *summed* value (a rate per resident, or the
#' raw total), never the `"n_totaal_gebruikers"` people-count type a
#' continuous outcome also has on file. A binary outcome only ever has
#' `"n_totaal_gebruikers"`.
#' @param kind `"binary"` or `"continuous"` (see [c4c_outcome_kind()]).
c4c_outcome_type <- function(kind) {
  if (identical(kind, "continuous")) "sum_totaal_groep" else "n_totaal_gebruikers"
}

#' Axis/title fragment for one metric.
#' @param metric One of `"percentage"`, `"rate_per_1000"`, `"absolute"`.
c4c_metric_axis_label <- function(metric) {
  switch(metric,
    percentage = "% van de bevolking",
    rate_per_1000 = "per 1.000 inwoners",
    absolute = "Aantal",
    metric
  )
}

#' Filter the outcomes table down to one breakdown/outcome/metric-type slice.
#' @param df Result of [c4c_load_outcomes()].
#' @param breakdown_id One of `data$breakdown`'s values (e.g. `"yearly_total"`,
#'   `"stadsdeel"`) -- see [C4C_BREAKDOWNS].
#' @param outcome_name Raw outcome column name (a name of [C4C_OUTCOME_META]).
#' @param outcome_type `"n_totaal_gebruikers"` or `"sum_totaal_groep"`.
#' @param years Optional integer vector to additionally restrict `year` to.
c4c_filter_outcome <- function(df, breakdown_id, outcome_name, outcome_type, years = NULL) {
  out <- dplyr::filter(
    df,
    .data$breakdown == breakdown_id,
    .data$name == outcome_name,
    .data$type == outcome_type
  )
  if (!is.null(years)) {
    out <- dplyr::filter(out, .data$year %in% years)
  }
  out
}

#' Add a `waarde` column holding the selected metric, computed from the raw
#' `value`/`n_totaal` columns.
#' @param df A slice from [c4c_filter_outcome()].
#' @param metric One of `"percentage"`, `"rate_per_1000"`, `"absolute"`.
c4c_add_metric <- function(df, metric) {
  dplyr::mutate(df, waarde = dplyr::case_when(
    metric == "percentage" ~ .data$value / .data$n_totaal * 100,
    metric == "rate_per_1000" ~ .data$value / .data$n_totaal * 1000,
    metric == "absolute" ~ as.numeric(.data$value),
    TRUE ~ NA_real_
  ))
}

#' Relabel a category column through the shared Dictionary
#' (`utils/dictionary.R`), preserving factor level order when `x` is already
#' an ordered factor -- same approach as `chart_data_downloads_server()`'s
#' internal `relabel_column()` (`utils/chart_downloads.R`), duplicated here
#' (rather than reused) because that one is private to the module's closure.
#' @param x Character or factor vector.
#' @param scope Dictionary scope (breakdown id).
c4c_relabel_groep <- function(x, scope) {
  identity_fallback <- function(v) v
  relabeled <- dictionary_relabel(x, scope = scope, fallback = identity_fallback)
  if (is.factor(x)) {
    relabeled <- factor(relabeled, levels = dictionary_relabel(levels(x), scope = scope, fallback = identity_fallback))
  }
  relabeled
}

#' Order a breakdown's raw `groep` values into a factor using
#' [C4C_BREAKDOWNS]'s fixed `levels` (e.g. age groups ascending), keeping
#' only levels actually present. Returns `x` unchanged (as character) when
#' the breakdown has no fixed order (e.g. `wijk_25` -- the "Naar gebied" tab
#' ranks those by value instead).
#' @param x Character vector of raw `groep` values.
#' @param breakdown_id One of `names(C4C_BREAKDOWNS)`.
c4c_order_groep <- function(x, breakdown_id, breakdowns = C4C_BREAKDOWNS) {
  fixed_levels <- breakdowns[[breakdown_id]]$levels
  if (is.null(fixed_levels)) return(as.character(x))
  present <- fixed_levels[fixed_levels %in% x]
  factor(x, levels = present)
}

#' One outcome's raw `value` for a single (year, breakdown) slice -- used to
#' build the "Overzicht"/"Zorgpad" KPI cards. `NA` (never an error) when the
#' combination has no row, e.g. because the cell was suppressed upstream or
#' the outcome isn't yet defined for that year.
#' @param df Result of [c4c_load_outcomes()].
#' @param year Single year.
#' @param outcome_name Raw outcome column name.
#' @param outcome_type `"n_totaal_gebruikers"` (default) or `"sum_totaal_groep"`.
#' @param breakdown_id Defaults to `"yearly_total"` (Amsterdam-wide).
c4c_value_for <- function(df, year, outcome_name, outcome_type = "n_totaal_gebruikers", breakdown_id = "yearly_total") {
  # `.env$year` (not a bare `year`) is required here: dplyr's data mask
  # resolves a bare symbol against the data's own columns *first* -- since
  # this function's `year` parameter shares its name with `df`'s `year`
  # column, an unqualified `.data$year == year` would silently compare the
  # column to itself (always TRUE) instead of to this argument.
  hit <- dplyr::filter(
    df,
    .data$breakdown == breakdown_id,
    .data$year == .env$year,
    .data$name == outcome_name,
    .data$type == outcome_type
  )
  if (nrow(hit) == 0) return(NA_real_)
  hit$value[[1]]
}

#' The Amsterdam-wide resident population for one year (the shared `n_totaal`
#' denominator, identical across every outcome row for that year in the
#' `"yearly_total"` breakdown). `NA` if the year isn't present at all.
#' @param df Result of [c4c_load_outcomes()].
#' @param year Single year.
c4c_population_for <- function(df, year) {
  # See c4c_value_for()'s comment on why .env$year (not a bare year) matters here.
  hit <- dplyr::filter(df, .data$breakdown == "yearly_total", .data$year == .env$year)
  if (nrow(hit) == 0) return(NA_real_)
  hit$n_totaal[[1]]
}

#' KPI summary for the "Overzicht" tab: Amsterdam-wide population, share
#' using risk-factor medication, and share with a major CVD event
#' (hospital admission or death), for one year.
#' @param df Result of [c4c_load_outcomes()].
#' @param year Single year.
#' @return A named list of scalars (some may be `NA`).
c4c_kpi_overzicht <- function(df, year) {
  population <- c4c_population_for(df, year)
  medicatie_n <- c4c_value_for(df, year, "heeft_risicofactor_medicatie")
  event_n <- c4c_value_for(df, year, "heeft_hartinfarct_of_acute_beroerte_lbz_of_do")
  list(
    year = year,
    population = population,
    medicatie_n = medicatie_n,
    medicatie_pct = medicatie_n / population * 100,
    event_n = event_n,
    event_pct = event_n / population * 100
  )
}

#' KPI summary for the "Zorgpad" tab: of everyone with a first major CVD
#' event in `year`, how many already used risk-factor medication beforehand
#' vs. not -- see [C4C_ZORGPAD_EVENT_NAMES] and
#' `code/04_make_incidence_outcomes.R`'s "4 outcome groups" section.
#' @param df Result of [c4c_load_outcomes()].
#' @param year Single year.
#' @return A named list of scalars (some may be `NA`).
c4c_kpi_zorgpad <- function(df, year) {
  met_n <- c4c_value_for(df, year, "heeft_eerste_event_met_medicatie")
  zonder_n <- c4c_value_for(df, year, "heeft_eerste_event_geen_medicatie")
  totaal_n <- met_n + zonder_n
  list(
    year = year,
    eerste_event_met_medicatie_n = met_n,
    eerste_event_zonder_medicatie_n = zonder_n,
    eerste_event_totaal_n = totaal_n,
    eerste_event_zonder_medicatie_pct = zonder_n / totaal_n * 100
  )
}

#' A categorical colour palette of length `n`. Uses `base` (the AHTI brand
#' palette, `ahti_branding$scale_discrete`) directly when it already has
#' enough colours, and interpolates additional ones otherwise -- some
#' breakdowns have more categories than the 5-colour brand palette provides
#' (e.g. `herkomst7`'s 8), and `ggplot2::scale_colour_manual()`/
#' `scale_fill_manual()` errors ("Insufficient values") rather than
#' recycling when handed fewer colours than factor levels.
#' @param n Number of distinct colours needed.
#' @param base Character vector of hex colours to start from.
c4c_palette <- function(n, base) {
  if (n <= length(base)) return(base[seq_len(n)])
  grDevices::colorRampPalette(base)(n)
}

#' Data behind the "Zorgpad" tab's flagship chart: for every year, the
#' number of people with a first major CVD event who had (`"met"`) or had
#' not (`"zonder"`) already used risk-factor medication beforehand.
#' @param df Result of [c4c_load_outcomes()].
#' @return Tibble with columns `year`, `name`, `groep_label`, `waarde`.
c4c_zorgpad_data <- function(df) {
  out <- dplyr::filter(
    df,
    .data$breakdown == "yearly_total",
    .data$name %in% C4C_ZORGPAD_EVENT_NAMES,
    .data$type == "n_totaal_gebruikers"
  )
  out <- dplyr::mutate(out, groep_label = c4c_outcome_label(.data$name), waarde = .data$value)
  out$groep_label <- factor(
    out$groep_label,
    levels = c4c_outcome_label(rev(C4C_ZORGPAD_EVENT_NAMES))
  )
  out
}
