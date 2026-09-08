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

#' Default path to the committed stadsdeel boundary polygons (see
#' [c4c_load_geo_stadsdeel()]).
C4C_GEO_STADSDEEL_FILE <- file.path("data", "geo_stadsdeel.csv")

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

#' Read the stadsdeel boundary polygons used by the "Naar gebied" tab's map
#' view: one row per polygon vertex (`stadsdeel`, `order`, `long`, `lat`),
#' already simplified and in plain WGS84 lon/lat. Boundaries come from the
#' Who's On First open gazetteer's Amsterdam "borough" records
#' (github.com/whosonfirst-data/whosonfirst-data-admin-nl, current as of
#' this file's creation), matched to our own `stadsdeel` names and
#' simplified (Douglas-Peucker, tolerance 0.0002 degrees -- under 0.2% area
#' error) to keep the file small. Deliberately
#' plain `ggplot2::geom_polygon()`-ready data, not an `sf` object: this avoids
#' adding the `sf` package (and its GDAL/GEOS/PROJ system dependencies) to
#' the dashboard's deploy requirements for a single static map.
#'
#' Only 8 of Amsterdam's stadsdelen have a boundary here -- "Weesp" (merged
#' into Amsterdam in 2022) isn't yet in the open boundary source this file
#' was built from, and "Onbekend" has no location by definition. Both still
#' appear normally in the "Naar gebied" bar chart; [c4c_geo_join_stadsdeel()]
#' simply drops them from the map.
#' @param path Path to the CSV (see [C4C_GEO_STADSDEEL_FILE]).
#' @return A tibble with columns `stadsdeel`, `order`, `long`, `lat`.
c4c_load_geo_stadsdeel <- function(path = C4C_GEO_STADSDEEL_FILE) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  df$order <- as.integer(df$order)
  df$long <- as.numeric(df$long)
  df$lat <- as.numeric(df$lat)
  tibble::as_tibble(df)
}

#' Join stadsdeel boundary polygons to one outcome/year/metric slice, for a
#' choropleth map. Preserves each polygon's vertex order (required for
#' `ggplot2::geom_polygon()` to draw a correct shape, and not guaranteed by
#' a join). Areas with no boundary (see [c4c_load_geo_stadsdeel()]) are
#' silently dropped -- an inner join, since a map can only show what it has
#' a shape for.
#' @param geo_df Result of [c4c_load_geo_stadsdeel()].
#' @param outcome_df A single (year, metric) slice with `groep` (raw
#'   stadsdeel name) and `waarde` columns, e.g. [c4c_add_metric()]'s output
#'   filtered to one year for `breakdown_id` `"stadsdeel"`.
#' @return Tibble of polygon vertices with the matching `waarde` attached,
#'   ordered for plotting.
c4c_geo_join_stadsdeel <- function(geo_df, outcome_df) {
  out <- dplyr::inner_join(geo_df, outcome_df, by = c("stadsdeel" = "groep"))
  dplyr::arrange(out, .data$stadsdeel, .data$order)
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
#' default. When `name` is given and is incidence-eligible (see
#' [c4c_is_incidence_eligible()]), an extra `"incidence"` choice is appended.
#' @param kind `"binary"` or `"continuous"` (see [c4c_outcome_kind()]).
#' @param name Optional raw outcome column name.
c4c_available_metrics <- function(kind, name = NULL) {
  base <- if (identical(kind, "continuous")) {
    c("Aantal per 1.000 inwoners" = "rate_per_1000", "Totaal aantal (opnames)" = "absolute")
  } else {
    c("Aandeel van de bevolking (%)" = "percentage", "Aantal personen" = "absolute")
  }
  if (!is.null(name) && c4c_is_incidence_eligible(name)) {
    base <- c(base, "Incidentie: aandeel van de risicogroep (%)" = "incidence")
  }
  base
}

#' The `"heeft_geen_eerdere_..."` outcome name that gives an "eerste_jaar"
#' incidence outcome's at-risk denominator, e.g.
#' `"heeft_eerste_jaar_hi_event"` -> `"heeft_geen_eerdere_hi_event"`. `NA`
#' when `name` isn't an `"heeft_eerste_jaar_..."` outcome, or has no
#' matching denominator in `meta` (e.g. the "Zorgpad" cross-tab outcomes,
#' which aren't simple yes/no conditions with their own "not yet" state).
#' @param name Raw outcome column name (may be a vector).
#' @param meta Outcome metadata table (see [C4C_OUTCOME_META]).
c4c_incidence_denominator_name <- function(name, meta = C4C_OUTCOME_META) {
  denom <- sub("^heeft_eerste_jaar_", "heeft_geen_eerdere_", name)
  ifelse(grepl("^heeft_eerste_jaar_", name) & denom %in% names(meta), denom, NA_character_)
}

#' Whether an outcome has a valid incidence denominator -- see
#' [c4c_incidence_denominator_name()].
#' @param name Raw outcome column name.
#' @param meta Outcome metadata table (see [C4C_OUTCOME_META]).
c4c_is_incidence_eligible <- function(name, meta = C4C_OUTCOME_META) {
  !is.na(c4c_incidence_denominator_name(name, meta))
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
#' @param metric One of `"percentage"`, `"rate_per_1000"`, `"absolute"`,
#'   `"incidence"`.
c4c_metric_axis_label <- function(metric) {
  switch(metric,
    percentage = "% van de bevolking",
    rate_per_1000 = "per 1.000 inwoners",
    absolute = "Aantal",
    incidence = "% van de risicogroep",
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

#' Add a `waarde` column holding the incidence % = numerator / at-risk
#' population * 100, where the at-risk population is the matching
#' `"heeft_geen_eerdere_..."` outcome's own person-count for the same
#' year/breakdown/`groep` -- see [c4c_incidence_denominator_name()]. Unlike
#' [c4c_add_metric()]'s `"percentage"` (which divides by the whole
#' population, `n_totaal`), this divides by the smaller population that had
#' not yet had the event/started the medication, i.e. genuinely at risk of
#' a first occurrence that year.
#' @param df A single-outcome slice from [c4c_filter_outcome()] (one `name`,
#'   `type` `"n_totaal_gebruikers"`).
#' @param full_df The full result of [c4c_load_outcomes()], to look up the
#'   denominator outcome's rows.
#' @param breakdown_id The breakdown `df` was filtered to (e.g.
#'   `"yearly_total"`, `"stadsdeel"`).
c4c_add_incidence <- function(df, full_df, breakdown_id) {
  denom_name <- unique(c4c_incidence_denominator_name(df$name))
  stopifnot(
    "df must contain exactly one incidence-eligible outcome name" =
      length(denom_name) == 1 && !is.na(denom_name)
  )
  denom <- dplyr::filter(
    full_df,
    .data$breakdown == breakdown_id,
    .data$name == denom_name,
    .data$type == "n_totaal_gebruikers"
  )
  denom <- dplyr::select(denom, "year", "groep", at_risk = "value")
  out <- dplyr::left_join(df, denom, by = c("year", "groep"))
  dplyr::mutate(out, waarde = .data$value / .data$at_risk * 100)
}

#' Dispatch to [c4c_add_metric()] or [c4c_add_incidence()] based on
#' `metric` -- shared by every tab that lets the user pick a metric, since
#' `"incidence"` needs the full outcomes table and the breakdown id that
#' the other metrics don't.
#' @param df A single-outcome slice from [c4c_filter_outcome()].
#' @param metric One of `"percentage"`, `"rate_per_1000"`, `"absolute"`,
#'   `"incidence"`.
#' @param full_df Full result of [c4c_load_outcomes()] -- required only
#'   when `metric` is `"incidence"`.
#' @param breakdown_id The breakdown `df` was filtered to -- required only
#'   when `metric` is `"incidence"`.
c4c_apply_metric <- function(df, metric, full_df = NULL, breakdown_id = NULL) {
  if (identical(metric, "incidence")) {
    c4c_add_incidence(df, full_df, breakdown_id)
  } else {
    c4c_add_metric(df, metric)
  }
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

#' Data behind the "Zorgpad" tab's event x medication cross-tab chart: for
#' every year, the number of people in each selected group -- see
#' [C4C_ZORGPAD_ALL_NAMES]. Also derives `heeft_event`/`heeft_medicatie`
#' factor columns so the chart can facet by one and fill/dodge by the
#' other, since the two "event" groups (a few hundred people) and the two
#' "no event" groups (most of the population) sit on wildly different
#' scales -- faceting with a free y-axis keeps both readable.
#' @param df Result of [c4c_load_outcomes()].
#' @param names Character vector of outcome names to include (a subset of
#'   [C4C_ZORGPAD_ALL_NAMES]) -- lets the "Zorgpad" tab's group picker show
#'   only the selected groups. Defaults to all 4.
#' @param years Optional integer vector to restrict `year` to.
#' @return Tibble with columns `year`, `name`, `groep_label`, `heeft_event`,
#'   `heeft_medicatie`, `waarde`.
c4c_zorgpad_data <- function(df, names = C4C_ZORGPAD_ALL_NAMES, years = NULL) {
  out <- dplyr::filter(
    df,
    .data$breakdown == "yearly_total",
    .data$name %in% names,
    .data$type == "n_totaal_gebruikers"
  )
  if (!is.null(years)) {
    out <- dplyr::filter(out, .data$year %in% years)
  }
  out <- dplyr::mutate(
    out,
    groep_label = c4c_outcome_label(.data$name),
    waarde = .data$value,
    heeft_event = ifelse(
      grepl("^heeft_eerste_event", .data$name), "Wel gebeurtenis", "Geen gebeurtenis"
    ),
    heeft_medicatie = ifelse(
      grepl("met_medicatie$", .data$name), "Met eerdere medicatie", "Zonder eerdere medicatie"
    )
  )
  out$groep_label <- factor(out$groep_label, levels = c4c_outcome_label(C4C_ZORGPAD_ALL_NAMES))
  out$heeft_event <- factor(out$heeft_event, levels = c("Wel gebeurtenis", "Geen gebeurtenis"))
  out$heeft_medicatie <- factor(
    out$heeft_medicatie, levels = c("Zonder eerdere medicatie", "Met eerdere medicatie")
  )
  out
}
