#' Cardio4Cities dictionary seed.
#'
#' Overrides `dictionary_seed_entries()` (default defined in
#' `utils/dictionary.R`, before this file is sourced) with pretty labels for
#' this dashboard's own raw category values -- see CLAUDE.md's
#' `dictionary_seed_entries()` override convention. `app.R` sources this file
#' with a plain top-level `source()` (`local = FALSE`, same as every
#' `utils/*.R` file), so the redefinition below lands in the global
#' environment `dictionary_list()` actually reads from -- no `<<-` needed
#' here (that's only required for an override written *inline* inside
#' `app.R` itself).
#'
#' Every entry is scoped to its breakdown id (`utils/dictionary.R`'s
#' `scope` -- see `C4C_BREAKDOWNS` in `data/metadata/outcome_metadata.R`),
#' matching the `category_scope` this dashboard's charts pass to
#' `chart_data_downloads_server()`/`dictionary_relabel()`.
#'
#' Most raw values here are already the exact Dutch labels the CBS
#' Remote Access pipeline (and its Amsterdam stadsdelen/wijken crosswalk)
#' produced, so most entries are a deliberate pass-through -- without them,
#' `dictionary_default_prettify()`'s fallback (which turns every `-`/`_`
#' into a space before title-casing) would silently mangle values that
#' contain a real hyphen, e.g. turning the borough "Nieuw-West" into
#' "Nieuw West", or the range "0-25%" into "0 25%". A handful of raw codes
#' (`inkomen_klasse`, `age_cat`'s "80-older") get a genuinely nicer label
#' instead of a plain pass-through.
dictionary_seed_entries <- function() {
  entry <- function(raw_key, scope, pretty_label) {
    list(raw_key = raw_key, scope = scope, pretty_label = pretty_label)
  }

  pass_through <- function(values, scope) {
    lapply(values, function(v) entry(v, scope, v))
  }

  c(
    pass_through(c("Mannen", "Vrouwen"), "geslacht"),

    list(
      entry("0-49", "age_cat", "0-49 jaar"),
      entry("50-59", "age_cat", "50-59 jaar"),
      entry("60-69", "age_cat", "60-69 jaar"),
      entry("70-79", "age_cat", "70-79 jaar"),
      entry("80-older", "age_cat", "80+ jaar")
    ),

    pass_through(c("Nederlandse herkomst", "Kind van migrant", "Migrant"), "herkomst3"),

    pass_through(c(
      "Nederland", "Europa (exclusief Nederland)", "Turkije", "Marokko",
      "Suriname", "Nederlands-Caribisch gebied", "Indonesië",
      "Overig Afrika, Azië, Amerika en Oceanië"
    ), "herkomst7"),

    pass_through(c("0-25%", "25-50%", "50-75%", "75-100%", "Onbekend"), "seswoa_cat"),

    pass_through(c(
      "Eenpersoonshuishouden", "Eenouderhuishouden", "Paar met kinderen",
      "Paar zonder kinderen", "Overig meerpersoonshuishouden",
      "Institutioneel huishouden", "Onbekend"
    ), "huishsamstsocec"),

    list(
      entry("tot_120", "inkomen_klasse", "Tot 120% sociaal minimum"),
      entry("120_280", "inkomen_klasse", "120–280% sociaal minimum"),
      entry("280_400", "inkomen_klasse", "280–400% sociaal minimum"),
      entry("400+", "inkomen_klasse", "Meer dan 400% sociaal minimum"),
      entry("student", "inkomen_klasse", "Student"),
      entry("Onbekend_institutioneel", "inkomen_klasse", "Onbekend (institutioneel huishouden)")
    ),

    pass_through(c(
      "Centrum", "West", "Nieuw-West", "Zuid", "Oost", "Noord",
      "Zuidoost", "Westpoort", "Weesp", "Onbekend"
    ), "stadsdeel"),

    pass_through(c(
      "Bijlmer-Centrum", "Bijlmer-Oost", "Bijlmer-West", "Bos en Lommer",
      "Buitenveldert, Zuidas", "Centrum-Oost", "Centrum-West",
      "De Aker, Sloten, Nieuw-Sloten", "De Pijp, Rivierenbuurt", "Gaasperdam",
      "Geuzenveld, Slotermeer", "IJburg, Zeeburgereiland",
      "Indische Buurt, Oostelijk Havengebied", "Noord-Oost", "Noord-West",
      "Onbekend", "Osdorp", "Oud-Noord", "Oud-Oost", "Oud-West, De Baarsjes",
      "Oud-Zuid", "Sloterdijk Nieuw-West", "Slotervaart", "Watergraafsmeer",
      "Weesp, Driemond", "Westerpark"
    ), "wijk_25")
  )
}
