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

  cont_metrics <- c4c_available_metrics("continuous")
  expect_equal(unname(cont_metrics[1]), "rate_per_1000")
  expect_true("absolute" %in% cont_metrics)
})

test_that("c4c_metric_axis_label has a label for every known metric", {
  expect_equal(c4c_metric_axis_label("percentage"), "% van de bevolking")
  expect_equal(c4c_metric_axis_label("rate_per_1000"), "per 1.000 inwoners")
  expect_equal(c4c_metric_axis_label("absolute"), "Aantal")
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
    "geslacht",       "Vrouwen",  2023,  465280,    "heeft_hypertensie_n_totaal_gebruikers",           70000, "heeft_hypertensie",                            "n_totaal_gebruikers"
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

test_that("c4c_value_for and c4c_population_for return NA (not an error) for a missing combination", {
  df <- make_test_outcomes()
  expect_equal(c4c_value_for(df, 2023, "heeft_risicofactor_medicatie"), 143760)
  expect_true(is.na(c4c_value_for(df, 2099, "heeft_risicofactor_medicatie")))
  expect_true(is.na(c4c_value_for(df, 2023, "heeft_onbekend")))
  expect_equal(c4c_population_for(df, 2023), 915280)
  expect_true(is.na(c4c_population_for(df, 1999)))
})

test_that("c4c_kpi_overzicht computes shares from the yearly_total breakdown", {
  df <- make_test_outcomes()
  kpi <- c4c_kpi_overzicht(df, 2023)
  expect_equal(kpi$population, 915280)
  expect_equal(kpi$medicatie_n, 143760)
  expect_equal(round(kpi$medicatie_pct, 2), round(143760 / 915280 * 100, 2))
  expect_equal(kpi$event_n, 2160)
})

test_that("c4c_kpi_zorgpad computes the share of first events without prior medication", {
  df <- make_test_outcomes()
  kpi <- c4c_kpi_zorgpad(df, 2023)
  expect_equal(kpi$eerste_event_met_medicatie_n, 1340)
  expect_equal(kpi$eerste_event_zonder_medicatie_n, 440)
  expect_equal(kpi$eerste_event_totaal_n, 1780)
  expect_equal(round(kpi$eerste_event_zonder_medicatie_pct, 2), round(440 / 1780 * 100, 2))
})

test_that("c4c_zorgpad_data returns only the two first-event outcomes, labeled and factored", {
  df <- make_test_outcomes()
  out <- c4c_zorgpad_data(df)
  expect_equal(nrow(out), 2)
  expect_true(is.factor(out$groep_label))
  expect_setequal(
    as.character(out$groep_label),
    c("Eerste gebeurtenis, mét eerdere medicatie", "Eerste gebeurtenis, zonder eerdere medicatie")
  )
  expect_equal(sum(out$waarde), 1780)
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
