library(testthat)

test_that("c4c_outcome_kind reads the heeft_/n_ naming convention", {
  expect_equal(c4c_outcome_kind("heeft_risicofactor_medicatie"), "binary")
  expect_equal(c4c_outcome_kind("n_hartinfarct_lbz"), "continuous")
  expect_equal(
    c4c_outcome_kind(c("heeft_x", "n_y")),
    c("binary", "continuous")
  )
})

test_that("c4c_outcome_type maps kind to the right export type", {
  expect_equal(c4c_outcome_type("binary"), "n_totaal_gebruikers")
  expect_equal(c4c_outcome_type("continuous"), "sum_totaal_groep")
})

test_that("c4c_outcome_label looks up the metadata table and falls back to the raw name", {
  expect_equal(c4c_outcome_label("heeft_risicofactor_medicatie"), "Eén van de risicofactor-medicijnen")
  expect_equal(c4c_outcome_label("onbekende_naam"), "onbekende_naam")
  expect_equal(
    c4c_outcome_label(c("heeft_risicofactor_medicatie", "onbekende_naam")),
    c("Eén van de risicofactor-medicijnen", "onbekende_naam")
  )
})

test_that("c4c_outcome_choices covers every outcome exactly once, grouped and sorted", {
  choices <- c4c_outcome_choices()

  expect_equal(names(choices), unname(C4C_OUTCOME_GROUPS))

  all_values <- unlist(choices, use.names = FALSE)
  expect_equal(sort(all_values), sort(names(C4C_OUTCOME_META)))
  expect_equal(length(all_values), length(unique(all_values)))

  medicatie_group <- choices[["Medicijngebruik risicofactoren"]]
  expect_equal(unname(medicatie_group), unname(medicatie_group[order(names(medicatie_group))]))
})

test_that("c4c_available_metrics offers the right choices per kind, default first", {
  binary_metrics <- c4c_available_metrics("binary")
  expect_equal(unname(binary_metrics[1]), "percentage")
  expect_true("absolute" %in% binary_metrics)
  # Prevalence per 1.000 is offered alongside percentage for binary outcomes.
  expect_true("rate_per_1000" %in% binary_metrics)

  cont_metrics <- c4c_available_metrics("continuous")
  expect_equal(unname(cont_metrics[1]), "rate_per_1000")
  expect_true("absolute" %in% cont_metrics)
})

test_that("c4c_available_metrics adds an incidence choice only for incidence-eligible outcomes", {
  with_incidence <- c4c_available_metrics("binary", "heeft_eerste_jaar_hi_event")
  expect_true("incidence" %in% with_incidence)

  without_incidence <- c4c_available_metrics("binary", "heeft_risicofactor_medicatie")
  expect_false("incidence" %in% without_incidence)

  # No name given at all -- same as before, no incidence choice.
  expect_false("incidence" %in% c4c_available_metrics("binary"))
})

test_that("c4c_incidence_denominator_name / c4c_is_incidence_eligible identify eerste_jaar outcomes", {
  expect_equal(
    c4c_incidence_denominator_name("heeft_eerste_jaar_hi_event"),
    "heeft_geen_eerdere_hi_event"
  )
  expect_true(c4c_is_incidence_eligible("heeft_eerste_jaar_hi_event"))

  # Not an "eerste_jaar" outcome at all.
  expect_true(is.na(c4c_incidence_denominator_name("heeft_risicofactor_medicatie")))
  expect_false(c4c_is_incidence_eligible("heeft_risicofactor_medicatie"))

  # The Zorgpad cross-tab outcomes have no matching "geen_eerdere" counterpart.
  expect_false(c4c_is_incidence_eligible("heeft_eerste_event_geen_medicatie"))

  # Vectorized.
  expect_equal(
    c4c_incidence_denominator_name(c("heeft_eerste_jaar_hi_event", "heeft_risicofactor_medicatie")),
    c("heeft_geen_eerdere_hi_event", NA_character_)
  )
})

test_that("c4c_metric_axis_label has a label for every known metric", {
  expect_equal(c4c_metric_axis_label("percentage"), "% van de bevolking")
  expect_equal(c4c_metric_axis_label("rate_per_1000"), "per 1.000 inwoners")
  expect_equal(c4c_metric_axis_label("absolute"), "Aantal")
  expect_equal(c4c_metric_axis_label("incidence"), "% van de risicogroep")
})

make_test_outcomes <- function() {
  tibble::tribble(
    ~breakdown,       ~groep,     ~year, ~n_totaal, ~variable,                                     ~value, ~name,                                          ~type,
    "yearly_total",   "",         2022,  900000,    "heeft_risicofactor_medicatie_n_totaal_gebruikers", 140000, "heeft_risicofactor_medicatie",                "n_totaal_gebruikers",
    "yearly_total",   "",         2023,  915280,    "heeft_risicofactor_medicatie_n_totaal_gebruikers", 143760, "heeft_risicofactor_medicatie",                "n_totaal_gebruikers",
    "yearly_total",   "",         2023,  915280,    "heeft_hartinfarct_of_acute_beroerte_lbz_of_do_n_totaal_gebruikers", 2160, "heeft_hartinfarct_of_acute_beroerte_lbz_of_do", "n_totaal_gebruikers",
    "yearly_total",   "",         2023,  915280,    "heeft_eerste_event_geen_medicatie_n_totaal_gebruikers", 440, "heeft_eerste_event_geen_medicatie",           "n_totaal_gebruikers",
    "yearly_total",   "",         2023,  915280,    "heeft_eerste_event_met_medicatie_n_totaal_gebruikers", 1340, "heeft_eerste_event_met_medicatie",            "n_totaal_gebruikers",
    "yearly_total",   "",         2023,  915280,    "n_hartinfarct_or_acute_beroerte_lbz_sum_totaal_groep", 2350, "n_hartinfarct_or_acute_beroerte_lbz",         "sum_totaal_groep",
    "geslacht",       "Mannen",   2023,  450000,    "heeft_hypertensie_n_totaal_gebruikers",           60000, "heeft_hypertensie",                            "n_totaal_gebruikers",
    "geslacht",       "Vrouwen",  2023,  465280,    "heeft_hypertensie_n_totaal_gebruikers",           70000, "heeft_hypertensie",                            "n_totaal_gebruikers",
    "yearly_total",   "",         2023,  915280,    "heeft_geen_eerste_event_geen_medicatie_n_totaal_gebruikers", 750000, "heeft_geen_eerste_event_geen_medicatie", "n_totaal_gebruikers",
    "yearly_total",   "",         2023,  915280,    "heeft_geen_eerste_event_met_medicatie_n_totaal_gebruikers", 163700, "heeft_geen_eerste_event_met_medicatie", "n_totaal_gebruikers",
    "yearly_total",   "",         2022,  900000,    "heeft_eerste_event_geen_medicatie_n_totaal_gebruikers", 400, "heeft_eerste_event_geen_medicatie",           "n_totaal_gebruikers",
    "yearly_total",   "",         2022,  900000,    "heeft_eerste_event_met_medicatie_n_totaal_gebruikers", 1300, "heeft_eerste_event_met_medicatie",            "n_totaal_gebruikers",
    "yearly_total",   "",         2023,  915280,    "heeft_eerste_jaar_hi_event_n_totaal_gebruikers",  90, "heeft_eerste_jaar_hi_event",                  "n_totaal_gebruikers",
    "yearly_total",   "",         2023,  915280,    "heeft_geen_eerdere_hi_event_n_totaal_gebruikers", 910000, "heeft_geen_eerdere_hi_event",                 "n_totaal_gebruikers",
    "geslacht",       "Mannen",   2023,  450000,    "heeft_eerste_event_geen_medicatie_n_totaal_gebruikers", 40, "heeft_eerste_event_geen_medicatie",         "n_totaal_gebruikers",
    "geslacht",       "Vrouwen",  2023,  465280,    "heeft_eerste_event_geen_medicatie_n_totaal_gebruikers", 20, "heeft_eerste_event_geen_medicatie",         "n_totaal_gebruikers",
    "geslacht",       "Mannen",   2023,  450000,    "heeft_eerste_event_met_medicatie_n_totaal_gebruikers", 100, "heeft_eerste_event_met_medicatie",           "n_totaal_gebruikers",
    "geslacht",       "Vrouwen",  2023,  465280,    "heeft_eerste_event_met_medicatie_n_totaal_gebruikers", 60, "heeft_eerste_event_met_medicatie",            "n_totaal_gebruikers",
    "geslacht",       "Mannen",   2023,  450000,    "heeft_eerste_jaar_major_cvd_event_n_totaal_gebruikers", 140, "heeft_eerste_jaar_major_cvd_event",         "n_totaal_gebruikers",
    "geslacht",       "Vrouwen",  2023,  465280,    "heeft_eerste_jaar_major_cvd_event_n_totaal_gebruikers", 80, "heeft_eerste_jaar_major_cvd_event",          "n_totaal_gebruikers",
    "geslacht",       "Mannen",   2023,  450000,    "heeft_geen_eerdere_major_cvd_event_n_totaal_gebruikers", 447000, "heeft_geen_eerdere_major_cvd_event",     "n_totaal_gebruikers",
    "geslacht",       "Vrouwen",  2023,  465280,    "heeft_geen_eerdere_major_cvd_event_n_totaal_gebruikers", 463000, "heeft_geen_eerdere_major_cvd_event",     "n_totaal_gebruikers"
  )
}

test_that("c4c_filter_outcome selects only the requested breakdown/name/type slice", {
  df <- make_test_outcomes()
  out <- c4c_filter_outcome(df, "geslacht", "heeft_hypertensie", "n_totaal_gebruikers")
  expect_equal(nrow(out), 2)
  expect_setequal(out$groep, c("Mannen", "Vrouwen"))

  out_year <- c4c_filter_outcome(df, "yearly_total", "heeft_risicofactor_medicatie", "n_totaal_gebruikers", years = 2023)
  expect_equal(nrow(out_year), 1)
  expect_equal(out_year$value, 143760)
})

test_that("c4c_add_metric computes percentage, rate_per_1000 and absolute correctly", {
  df <- tibble::tibble(value = 200, n_totaal = 1000)
  expect_equal(c4c_add_metric(df, "percentage")$waarde, 20)
  expect_equal(c4c_add_metric(df, "rate_per_1000")$waarde, 200)
  expect_equal(c4c_add_metric(df, "absolute")$waarde, 200)
})

test_that("c4c_order_groep applies the fixed level order and drops missing levels", {
  ordered <- c4c_order_groep(c("Vrouwen", "Mannen"), "geslacht")
  expect_true(is.factor(ordered))
  expect_equal(levels(ordered), c("Mannen", "Vrouwen"))

  # A breakdown with no fixed order (wijk_25) is returned unchanged, as character.
  unordered <- c4c_order_groep(c("Osdorp", "Centrum-Oost"), "wijk_25")
  expect_false(is.factor(unordered))
  expect_equal(unordered, c("Osdorp", "Centrum-Oost"))
})

with_dictionary_path <- function(code) {
  path <- tempfile("dictionary_", fileext = ".json")
  old <- Sys.getenv("SHINY_DICTIONARY_PATH", unset = NA)
  Sys.setenv(SHINY_DICTIONARY_PATH = path)
  on.exit({
    if (is.na(old)) Sys.unsetenv("SHINY_DICTIONARY_PATH") else Sys.setenv(SHINY_DICTIONARY_PATH = old)
    unlink(path)
  }, add = TRUE)
  force(code)
}

# Mirrors the (raw_key, scope, pretty_label) entries dictionary_seed.R seeds
# for real, just a small subset -- kept local to this test file (rather than
# sourcing data/metadata/dictionary_seed.R and relying on the real,
# growing seed list) so these tests exercise the *mechanism*
# (c4c_relabel_groep()'s dictionary lookup + fallback + factor-order
# handling) and stay stable regardless of that list's exact contents.
with_test_seed_entries <- function(code) {
  old <- dictionary_seed_entries
  dictionary_seed_entries <<- function() {
    list(
      list(raw_key = "Nieuw-West", scope = "stadsdeel", pretty_label = "Nieuw-West"),
      list(raw_key = "0-25%", scope = "seswoa_cat", pretty_label = "0-25%"),
      list(raw_key = "80-older", scope = "age_cat", pretty_label = "80+ jaar"),
      list(raw_key = "tot_120", scope = "inkomen_klasse", pretty_label = "Tot 120% sociaal minimum"),
      list(raw_key = "400+", scope = "inkomen_klasse", pretty_label = "Meer dan 400% sociaal minimum")
    )
  }
  on.exit(dictionary_seed_entries <<- old, add = TRUE)
  force(code)
}

test_that("c4c_relabel_groep pins hyphenated category values instead of mangling them", {
  with_dictionary_path({
    with_test_seed_entries({
      expect_equal(c4c_relabel_groep("Nieuw-West", "stadsdeel"), "Nieuw-West")
      expect_equal(c4c_relabel_groep("0-25%", "seswoa_cat"), "0-25%")
      expect_equal(c4c_relabel_groep("80-older", "age_cat"), "80+ jaar")
    })
  })
})

test_that("c4c_relabel_groep preserves factor level order while relabeling", {
  with_dictionary_path({
    with_test_seed_entries({
      x <- factor(c("tot_120", "400+"), levels = c("tot_120", "400+"))
      out <- c4c_relabel_groep(x, "inkomen_klasse")
      expect_equal(as.character(out), c("Tot 120% sociaal minimum", "Meer dan 400% sociaal minimum"))
      expect_equal(levels(out), c("Tot 120% sociaal minimum", "Meer dan 400% sociaal minimum"))
    })
  })
})

test_that("c4c_zorgpad_data returns all 4 cross-tab groups by default, labeled and factored", {
  df <- make_test_outcomes()
  out <- c4c_zorgpad_data(df, years = 2023)
  expect_equal(nrow(out), 4)
  expect_true(is.factor(out$groep_label))
  expect_true(is.factor(out$heeft_event))
  expect_true(is.factor(out$heeft_medicatie))

  # Event/medicatie flags line up correctly per group.
  by_name <- stats::setNames(seq_len(nrow(out)), out$name)
  expect_equal(as.character(out$heeft_event[by_name["heeft_eerste_event_geen_medicatie"]]), "Wel gebeurtenis")
  expect_equal(as.character(out$heeft_medicatie[by_name["heeft_eerste_event_geen_medicatie"]]), "Zonder eerdere medicatie")
  expect_equal(as.character(out$heeft_event[by_name["heeft_geen_eerste_event_met_medicatie"]]), "Geen gebeurtenis")
  expect_equal(as.character(out$heeft_medicatie[by_name["heeft_geen_eerste_event_met_medicatie"]]), "Met eerdere medicatie")

  expect_equal(sum(out$waarde), 440 + 1340 + 750000 + 163700)
})

test_that("c4c_zorgpad_data respects a names subset and a years filter", {
  df <- make_test_outcomes()
  out <- c4c_zorgpad_data(df, names = C4C_ZORGPAD_EVENT_NAMES)
  expect_setequal(out$name, C4C_ZORGPAD_EVENT_NAMES)
  expect_setequal(out$year, c(2022, 2023))

  out_2023 <- c4c_zorgpad_data(df, names = C4C_ZORGPAD_EVENT_NAMES, years = 2023)
  expect_equal(out_2023$year, rep(2023, nrow(out_2023)))
})

test_that("c4c_zorgpad_data defaults to yearly_total with a constant 'Amsterdam' breakdown_label and a plain facet_key", {
  df <- make_test_outcomes()
  out <- c4c_zorgpad_data(df, years = 2023)
  expect_true(all(as.character(out$breakdown_label) == "Amsterdam"))
  expect_equal(out$facet_key, as.character(out$heeft_event))
})

test_that("c4c_zorgpad_data supports a non-default breakdown_id, adding breakdown_label and a combined facet_key", {
  df <- make_test_outcomes()
  out <- c4c_zorgpad_data(df, names = C4C_ZORGPAD_EVENT_NAMES, breakdown_id = "geslacht", years = 2023)
  expect_equal(nrow(out), 4)
  expect_setequal(out$groep, c("Mannen", "Vrouwen"))
  expect_setequal(as.character(out$breakdown_label), c("Mannen", "Vrouwen"))
  expect_true(all(grepl("\\|", out$facet_key)))
  expect_equal(
    out$facet_key[out$groep == "Mannen" & out$name == "heeft_eerste_event_geen_medicatie"],
    "Wel gebeurtenis | Mannen"
  )
})

test_that("c4c_breakdown_label returns 'Amsterdam' for yearly_total and relabels/orders otherwise", {
  expect_equal(c4c_breakdown_label(c("", ""), "yearly_total"), c("Amsterdam", "Amsterdam"))

  out <- c4c_breakdown_label(c("Vrouwen", "Mannen"), "geslacht")
  expect_equal(as.character(out), c("Vrouwen", "Mannen"))
  expect_equal(levels(out), c("Mannen", "Vrouwen"))
})

test_that("c4c_add_incidence divides by the at-risk (geen_eerdere) population, not n_totaal", {
  df <- make_test_outcomes()
  numerator <- c4c_filter_outcome(df, "yearly_total", "heeft_eerste_jaar_hi_event", "n_totaal_gebruikers")
  out <- c4c_add_incidence(numerator, df, "yearly_total")
  expect_equal(out$waarde, 90 / 910000 * 100)
})

test_that("c4c_apply_metric dispatches to c4c_add_incidence only for the incidence metric", {
  df <- make_test_outcomes()
  numerator <- c4c_filter_outcome(df, "yearly_total", "heeft_eerste_jaar_hi_event", "n_totaal_gebruikers")

  incidence_out <- c4c_apply_metric(numerator, "incidence", full_df = df, breakdown_id = "yearly_total")
  expect_equal(incidence_out$waarde, 90 / 910000 * 100)

  pct_out <- c4c_apply_metric(numerator, "percentage")
  expect_equal(pct_out$waarde, 90 / 915280 * 100)
})

test_that("C4C_ZORGPAD_INCIDENCE_NAME's incidence works per breakdown, feeding the Zorgpad tab's rate chart", {
  df <- make_test_outcomes()
  numerator <- c4c_filter_outcome(df, "geslacht", C4C_ZORGPAD_INCIDENCE_NAME, "n_totaal_gebruikers")
  out <- c4c_apply_metric(numerator, "incidence", full_df = df, breakdown_id = "geslacht")
  expect_setequal(out$groep, c("Mannen", "Vrouwen"))
  expect_equal(out$waarde[out$groep == "Mannen"], 140 / 447000 * 100)
  expect_equal(out$waarde[out$groep == "Vrouwen"], 80 / 463000 * 100)
})

test_that("c4c_palette reuses the base palette when it already has enough colours", {
  base <- c("#111111", "#222222", "#333333")
  expect_equal(c4c_palette(2, base), base[1:2])
  expect_equal(c4c_palette(3, base), base)
})

test_that("c4c_palette interpolates extra colours instead of erroring when n exceeds the base palette", {
  base <- c("#111111", "#222222", "#333333")
  out <- c4c_palette(8, base)
  expect_length(out, 8)
  expect_length(unique(out), 8)
})

test_that("c4c_load_geo_stadsdeel reads and types the committed polygon CSV", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c(
    "stadsdeel,order,long,lat",
    "Centrum,0,4.9,52.37",
    "Centrum,1,4.91,52.37",
    "Centrum,2,4.91,52.38"
  ), tmp)
  on.exit(unlink(tmp))

  out <- c4c_load_geo_stadsdeel(tmp)
  expect_true(is.integer(out$order))
  expect_true(is.numeric(out$long))
  expect_true(is.numeric(out$lat))
  expect_equal(nrow(out), 3)
})

test_that("c4c_geo_join_stadsdeel attaches waarde and keeps polygon vertex order", {
  geo <- tibble::tribble(
    ~stadsdeel, ~order, ~long, ~lat,
    "Centrum",  2,      4.91,  52.38,
    "Centrum",  0,      4.90,  52.37,
    "Zuid",     0,      4.88,  52.35,
    "Centrum",  1,      4.91,  52.37
  )
  outcome <- tibble::tribble(
    ~groep,    ~waarde,
    "Centrum", 12.5,
    "Zuid",    8.0,
    "Weesp",   3.0
  )

  out <- c4c_geo_join_stadsdeel(geo, outcome)

  # Weesp has no polygon at all, so it's dropped regardless of join type
  # (it isn't a row in geo_df to begin with); Centrum and Zuid both have a
  # polygon and a matching outcome row, so both survive.
  expect_setequal(unique(out$stadsdeel), c("Centrum", "Zuid"))

  # Vertex order within each stadsdeel must be restored (0, 1, 2), even
  # though the input geo data frame was shuffled.
  centrum <- out[out$stadsdeel == "Centrum", ]
  expect_equal(centrum$order, c(0, 1, 2))
  expect_equal(centrum$waarde, c(12.5, 12.5, 12.5))
})

test_that("c4c_geo_join_stadsdeel keeps an area with a polygon but no outcome row, waarde NA", {
  geo <- tibble::tribble(
    ~stadsdeel, ~order, ~long, ~lat,
    "Centrum",  0,      4.90,  52.37,
    "Centrum",  1,      4.91,  52.37,
    "Zuid",     0,      4.88,  52.35
  )
  # Zuid is suppressed this year -- no row in outcome_df at all.
  outcome <- tibble::tribble(
    ~groep,    ~waarde,
    "Centrum", 12.5
  )

  out <- c4c_geo_join_stadsdeel(geo, outcome)

  # Both areas still appear (Zuid has a shape, even without a value).
  expect_setequal(unique(out$stadsdeel), c("Centrum", "Zuid"))
  expect_true(all(is.na(out$waarde[out$stadsdeel == "Zuid"])))
  expect_equal(out$waarde[out$stadsdeel == "Centrum"], c(12.5, 12.5))
})

test_that("c4c_load_geo_wijk reads and types the committed polygon CSV, including part", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c(
    "wijk,part,order,long,lat",
    "Jordaan,1,0,4.88,52.37",
    "Jordaan,1,1,4.89,52.37",
    "IJburg-West,1,0,5.00,52.35",
    "IJburg-West,2,0,5.02,52.36"
  ), tmp)
  on.exit(unlink(tmp))

  out <- c4c_load_geo_wijk(tmp)
  expect_true(is.integer(out$part))
  expect_true(is.integer(out$order))
  expect_true(is.numeric(out$long))
  expect_true(is.numeric(out$lat))
  expect_equal(nrow(out), 4)
})

test_that("c4c_geo_join_wijk attaches waarde and keeps vertex order within each polygon piece", {
  geo <- tibble::tribble(
    ~wijk,          ~part, ~order, ~long, ~lat,
    "IJburg-West",  1,     1,      5.01,  52.35,
    "IJburg-West",  1,     0,      5.00,  52.35,
    "IJburg-West",  2,     0,      5.02,  52.36,
    "Jordaan",      1,     0,      4.88,  52.37
  )
  outcome <- tibble::tribble(
    ~groep,         ~waarde,
    "IJburg-West",  12.5,
    "Jordaan",      8.0,
    "Onbekend",     3.0
  )

  out <- c4c_geo_join_wijk(geo, outcome)

  # Onbekend has no polygon at all, so it's dropped.
  expect_setequal(unique(out$wijk), c("IJburg-West", "Jordaan"))

  # Vertex order within each (wijk, part) piece is restored.
  piece1 <- out[out$wijk == "IJburg-West" & out$part == 1, ]
  expect_equal(piece1$order, c(0, 1))
  expect_equal(piece1$waarde, c(12.5, 12.5))

  # Both pieces of the multi-part wijk survive the join, distinctly.
  expect_equal(nrow(out[out$wijk == "IJburg-West", ]), 3)
})

test_that("c4c_geo_join_wijk keeps a wijk with a polygon but no outcome row, waarde NA", {
  geo <- tibble::tribble(
    ~wijk,     ~part, ~order, ~long, ~lat,
    "Jordaan", 1,     0,      4.88,  52.37,
    "Jordaan", 1,     1,      4.89,  52.37,
    "Zuidas",  1,     0,      4.89,  52.34
  )
  # Zuidas is suppressed this year -- no row in outcome_df at all.
  outcome <- tibble::tribble(
    ~groep,    ~waarde,
    "Jordaan", 8.0
  )

  out <- c4c_geo_join_wijk(geo, outcome)

  expect_setequal(unique(out$wijk), c("Jordaan", "Zuidas"))
  expect_true(all(is.na(out$waarde[out$wijk == "Zuidas"])))
})

test_that("c4c_load_geo_wijk25 reads and types the committed polygon CSV, including part", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c(
    "wijk_25,part,order,long,lat",
    "Centrum-West,1,0,4.88,52.37",
    "Centrum-West,1,1,4.89,52.37",
    "Centrum-Oost,1,0,4.90,52.37"
  ), tmp)
  on.exit(unlink(tmp))

  out <- c4c_load_geo_wijk25(tmp)
  expect_true(is.integer(out$part))
  expect_true(is.integer(out$order))
  expect_true(is.numeric(out$long))
  expect_true(is.numeric(out$lat))
  expect_equal(nrow(out), 3)
})

test_that("c4c_geo_join_wijk25 attaches waarde, keeps vertex order, and keeps an area with no outcome row as NA", {
  geo <- tibble::tribble(
    ~wijk_25,       ~part, ~order, ~long, ~lat,
    "Centrum-West", 1,     1,      4.91,  52.38,
    "Centrum-West", 1,     0,      4.90,  52.37,
    "Osdorp",       1,     0,      4.80,  52.36,
    "Centrum-West", 1,     2,      4.91,  52.37
  )
  outcome <- tibble::tribble(
    ~groep,         ~waarde,
    "Centrum-West", 12.5
    # Osdorp suppressed this year -- no row at all.
  )

  out <- c4c_geo_join_wijk25(geo, outcome)

  expect_setequal(unique(out$wijk_25), c("Centrum-West", "Osdorp"))
  cw <- out[out$wijk_25 == "Centrum-West", ]
  expect_equal(cw$order, c(0, 1, 2))
  expect_equal(cw$waarde, c(12.5, 12.5, 12.5))
  expect_true(all(is.na(out$waarde[out$wijk_25 == "Osdorp"])))
})

test_that("c4c_ring_centroid computes the centroid and area of a closed polygon ring", {
  # A 2x2 square: (0,0)-(2,0)-(2,2)-(0,2)-(0,0), closed.
  out <- c4c_ring_centroid(c(0, 2, 2, 0, 0), c(0, 0, 2, 2, 0))
  expect_equal(out$long, 1)
  expect_equal(out$lat, 1)
  expect_equal(out$area, 4)

  # Vertex winding order shouldn't flip the sign of the reported area.
  out_reversed <- c4c_ring_centroid(rev(c(0, 2, 2, 0, 0)), rev(c(0, 0, 2, 2, 0)))
  expect_equal(out_reversed$area, 4)
})

test_that("c4c_geo_label_points returns one centroid per area, single-piece", {
  geo <- tibble::tribble(
    ~stadsdeel, ~order, ~long, ~lat,
    "Centrum",  0,      0,     0,
    "Centrum",  1,      2,     0,
    "Centrum",  2,      2,     2,
    "Centrum",  3,      0,     2,
    "Centrum",  4,      0,     0,
    "Zuid",     0,      10,    10,
    "Zuid",     1,      12,    10,
    "Zuid",     2,      12,    12,
    "Zuid",     3,      10,    12,
    "Zuid",     4,      10,    10
  )
  out <- c4c_geo_label_points(geo, "stadsdeel")
  expect_setequal(out$stadsdeel, c("Centrum", "Zuid"))
  expect_equal(out$long[out$stadsdeel == "Centrum"], 1)
  expect_equal(out$lat[out$stadsdeel == "Centrum"], 1)
  expect_equal(out$long[out$stadsdeel == "Zuid"], 11)
})

test_that("c4c_geo_label_points picks the largest piece's centroid for a multi-part area", {
  geo <- tibble::tribble(
    ~wijk,   ~part, ~order, ~long, ~lat,
    # Big square piece (area 4), centroid (1, 1).
    "IJburg-West", 1, 0, 0, 0,
    "IJburg-West", 1, 1, 2, 0,
    "IJburg-West", 1, 2, 2, 2,
    "IJburg-West", 1, 3, 0, 2,
    "IJburg-West", 1, 4, 0, 0,
    # Tiny, far-away square piece (area 0.01), centroid (100, 100).
    "IJburg-West", 2, 0, 100,    100,
    "IJburg-West", 2, 1, 100.1,  100,
    "IJburg-West", 2, 2, 100.1,  100.1,
    "IJburg-West", 2, 3, 100,    100.1,
    "IJburg-West", 2, 4, 100,    100
  )
  out <- c4c_geo_label_points(geo, "wijk", part_col = "part")
  expect_equal(nrow(out), 1)
  expect_equal(out$long, 1)
  expect_equal(out$lat, 1)
})

test_that("c4c_point_in_ring detects points inside, outside, and on the edge of a square", {
  # Closed 2x2 square: (0,0)-(2,0)-(2,2)-(0,2)-(0,0).
  ring_long <- c(0, 2, 2, 0, 0)
  ring_lat <- c(0, 0, 2, 2, 0)

  expect_true(c4c_point_in_ring(1, 1, ring_long, ring_lat))
  expect_false(c4c_point_in_ring(5, 5, ring_long, ring_lat))
  expect_false(c4c_point_in_ring(-1, 1, ring_long, ring_lat))
})

test_that("c4c_area_at_point finds the matching single-piece area and returns its waarde", {
  geo <- tibble::tribble(
    ~stadsdeel, ~order, ~long, ~lat, ~waarde,
    "Centrum",  0,      0,     0,    12.5,
    "Centrum",  1,      2,     0,    12.5,
    "Centrum",  2,      2,     2,    12.5,
    "Centrum",  3,      0,     2,    12.5,
    "Centrum",  4,      0,     0,    12.5,
    "Zuid",     0,      10,    10,   8.0,
    "Zuid",     1,      12,    10,   8.0,
    "Zuid",     2,      12,    12,   8.0,
    "Zuid",     3,      10,    12,   8.0,
    "Zuid",     4,      10,    10,   8.0
  )

  hit <- c4c_area_at_point(geo, 1, 1, "stadsdeel")
  expect_equal(hit$name, "Centrum")
  expect_equal(hit$waarde, 12.5)

  miss <- c4c_area_at_point(geo, 50, 50, "stadsdeel")
  expect_null(miss)
})

test_that("c4c_area_at_point finds a point inside either piece of a multi-part area", {
  geo <- tibble::tribble(
    ~wijk,          ~part, ~order, ~long, ~lat,  ~waarde,
    "IJburg-West",  1,     0,      0,     0,     20,
    "IJburg-West",  1,     1,      2,     0,     20,
    "IJburg-West",  1,     2,      2,     2,     20,
    "IJburg-West",  1,     3,      0,     2,     20,
    "IJburg-West",  1,     4,      0,     0,     20,
    "IJburg-West",  2,     0,      100,   100,   20,
    "IJburg-West",  2,     1,      101,   100,   20,
    "IJburg-West",  2,     2,      101,   101,   20,
    "IJburg-West",  2,     3,      100,   101,   20,
    "IJburg-West",  2,     4,      100,   100,   20
  )

  hit_main <- c4c_area_at_point(geo, 1, 1, "wijk", part_col = "part")
  expect_equal(hit_main$name, "IJburg-West")

  hit_island <- c4c_area_at_point(geo, 100.5, 100.5, "wijk", part_col = "part")
  expect_equal(hit_island$name, "IJburg-West")
})

test_that("c4c_load_outcomes reads and types the committed CSV correctly", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c(
    "breakdown,groep,year,n_totaal,variable,value,name,type",
    "yearly_total,,2023,915280,heeft_risicofactor_medicatie_n_totaal_gebruikers,143760,heeft_risicofactor_medicatie,n_totaal_gebruikers"
  ), tmp)
  on.exit(unlink(tmp))

  out <- c4c_load_outcomes(tmp)
  expect_s3_class(out, "tbl_df")
  expect_true(is.integer(out$year))
  expect_true(is.numeric(out$n_totaal))
  expect_true(is.numeric(out$value))
  expect_equal(out$year, 2023L)
  expect_equal(out$value, 143760)
})
