#' Cardio4Cities outcome metadata.
#'
#' Maps every raw outcome column produced by the CBS Remote Access pipeline
#' (see the project's `code/04_make_incidence_outcomes.R` and
#' `code/06_make_desritpives.R`) to a human-readable Dutch label and one of 9
#' thematic groups, used to build the grouped indicator picker
#' (`c4c_outcome_choices()` in `utils/cardio_data.R`) shared by the
#' "Overzicht", "Zorgpad" and "Naar achtergrond"/"Naar gebied" tabs in `app.R`.
#'
#' The pipeline's own naming convention (see `make_aggregated_data()` in
#' `code/00_inputs.R`) is: a binary outcome's raw column name starts with
#' `heeft_` and is exported only as `<naam>_n_totaal_gebruikers` (count of
#' people with the outcome); a continuous outcome starts with `n_` and is
#' exported as both `<naam>_n_totaal_gebruikers` (count of people with a
#' non-zero value) and `<naam>_sum_totaal_groep` (summed value, e.g. total
#' number of admissions). `c4c_outcome_kind()` in `utils/cardio_data.R` reads
#' this prefix directly rather than duplicating it here.

C4C_OUTCOME_GROUPS <- c(
  medicatie              = "Medicijngebruik risicofactoren",
  medicatie_nieuw        = "Nieuwe medicatiestarters",
  medicatie_geen_eerder  = "Nog geen medicatie gebruikt",
  event                  = "Hartinfarct of acute beroerte (opname of overlijden)",
  overlijden             = "Overleden aan hartinfarct of acute beroerte",
  event_eerste           = "Eerste hartinfarct of acute beroerte",
  event_geen_eerder      = "Nog geen eerdere hartinfarct of acute beroerte",
  zorgpad                = "Zorgpad: gebeurtenis vs. medicatiegebruik",
  opnames                = "Ziekenhuisopnames (aantallen)"
)

C4C_OUTCOME_META <- list(
  heeft_diabetes_type_2                     = list(group = "medicatie",             label = "Diabetes type 2 (medicatie)"),
  heeft_hoog_cholesterol                    = list(group = "medicatie",             label = "Hoog cholesterol (medicatie)"),
  heeft_hypertensie                         = list(group = "medicatie",             label = "Hypertensie (medicatie)"),
  heeft_risicofactor_medicatie              = list(group = "medicatie",             label = "Eén van de risicofactor-medicijnen"),

  heeft_eerste_jaar_diabetes_type_2         = list(group = "medicatie_nieuw",       label = "Startte met diabetesmedicatie"),
  heeft_eerste_jaar_hoog_cholesterol        = list(group = "medicatie_nieuw",       label = "Startte met cholesterolmedicatie"),
  heeft_eerste_jaar_hypertensie             = list(group = "medicatie_nieuw",       label = "Startte met bloeddrukmedicatie"),
  heeft_eerste_jaar_risicofactor_medicatie  = list(group = "medicatie_nieuw",       label = "Startte met risicofactor-medicatie"),

  heeft_geen_eerdere_diabetes_type_2        = list(group = "medicatie_geen_eerder", label = "Nog geen diabetesmedicatie gebruikt"),
  heeft_geen_eerdere_hoog_cholesterol       = list(group = "medicatie_geen_eerder", label = "Nog geen cholesterolmedicatie gebruikt"),
  heeft_geen_eerdere_hypertensie            = list(group = "medicatie_geen_eerder", label = "Nog geen bloeddrukmedicatie gebruikt"),
  heeft_geen_eerdere_risicofactor_medicatie = list(group = "medicatie_geen_eerder", label = "Nog geen risicofactor-medicatie gebruikt"),

  heeft_hartinfarct_lbz_of_do                   = list(group = "event", label = "Hartinfarct (opname of overlijden)"),
  heeft_acute_beroerte_lbz_of_do                = list(group = "event", label = "Acute beroerte (opname of overlijden)"),
  heeft_hartinfarct_of_acute_beroerte_lbz_of_do = list(group = "event", label = "Hartinfarct of acute beroerte (opname of overlijden)"),

  heeft_do_hartinfarct                    = list(group = "overlijden", label = "Overleden aan hartinfarct"),
  heeft_do_acute_beroerte                 = list(group = "overlijden", label = "Overleden aan acute beroerte"),
  heeft_do_hartinfarct_of_acute_beroerte  = list(group = "overlijden", label = "Overleden aan hartinfarct of acute beroerte"),

  heeft_eerste_jaar_hi_event             = list(group = "event_eerste", label = "Eerste hartinfarct"),
  heeft_eerste_jaar_acute_beroerte_event = list(group = "event_eerste", label = "Eerste acute beroerte"),
  heeft_eerste_jaar_major_cvd_event      = list(group = "event_eerste", label = "Eerste hartinfarct of acute beroerte"),

  heeft_geen_eerdere_hi_event             = list(group = "event_geen_eerder", label = "Nog geen hartinfarct gehad"),
  heeft_geen_eerdere_acute_beroerte_event = list(group = "event_geen_eerder", label = "Nog geen acute beroerte gehad"),
  heeft_geen_eerdere_major_cvd_event      = list(group = "event_geen_eerder", label = "Nog geen hartinfarct of acute beroerte gehad"),

  heeft_eerste_event_geen_medicatie      = list(group = "zorgpad", label = "Eerste gebeurtenis, zonder eerdere medicatie"),
  heeft_eerste_event_met_medicatie       = list(group = "zorgpad", label = "Eerste gebeurtenis, mét eerdere medicatie"),
  heeft_geen_eerste_event_geen_medicatie = list(group = "zorgpad", label = "Geen gebeurtenis dit jaar, ook geen medicatie"),
  heeft_geen_eerste_event_met_medicatie  = list(group = "zorgpad", label = "Geen gebeurtenis dit jaar, wel medicatie"),

  n_hartinfarct_lbz                    = list(group = "opnames", label = "Ziekenhuisopnames voor hartinfarct"),
  n_acute_beroerte_lbz                 = list(group = "opnames", label = "Ziekenhuisopnames voor acute beroerte"),
  n_hartinfarct_or_acute_beroerte_lbz  = list(group = "opnames", label = "Ziekenhuisopnames voor hartinfarct of acute beroerte")
)

#' The two "eerste gebeurtenis" outcomes behind the "Zorgpad" tab's flagship
#' chart: everyone with a first major CVD event in a given year, split by
#' whether they had already used risk-factor medication beforehand. See
#' `code/04_make_incidence_outcomes.R`'s "4 outcome groups" section for the
#' underlying logic.
C4C_ZORGPAD_EVENT_NAMES <- c(
  "heeft_eerste_event_met_medicatie",
  "heeft_eerste_event_geen_medicatie"
)

#' All 4 outcomes behind the "Zorgpad" tab's full event x medication
#' cross-tab: every Amsterdammer that year, split by whether they had a
#' first major CVD event *and* whether they had already used risk-factor
#' medication beforehand -- see [C4C_ZORGPAD_EVENT_NAMES] (the "event"
#' half) and `code/04_make_incidence_outcomes.R`'s "4 outcome groups"
#' section.
C4C_ZORGPAD_ALL_NAMES <- c(
  "heeft_eerste_event_geen_medicatie",
  "heeft_eerste_event_met_medicatie",
  "heeft_geen_eerste_event_geen_medicatie",
  "heeft_geen_eerste_event_met_medicatie"
)

#' The outcome behind the "Zorgpad" tab's separate incidence-percentage
#' chart: everyone with a first major CVD event (heart attack or acute
#' stroke) that year, divided by its matching `"heeft_geen_eerdere_..."`
#' at-risk population (see [c4c_incidence_denominator_name()] in
#' `utils/cardio_data.R`) -- i.e. what share of Amsterdammers still at risk
#' had a first event that year. Distinct from [C4C_ZORGPAD_ALL_NAMES]'s
#' 4-group cross-tab, which splits the same "first event" population by
#' *medication* history instead of computing a rate.
C4C_ZORGPAD_INCIDENCE_NAME <- "heeft_eerste_jaar_major_cvd_event"

#' Demographic and geographic breakdowns available in the dashboard (matches
#' the `breakdown` column of `data/cardio4cities_outcomes.csv`, itself one
#' sheet name from the pipeline's `code/06_make_desritpives.R` export). Raw
#' CBS-code breakdowns without a name crosswalk available in this bundle
#' ("wijk", "buurt") are intentionally left out of this dashboard's data file
#' -- "stadsdeel" (8 boroughs) and "wijk_25" (25 named areas) already give a
#' readable geographic split.
#'
#' `levels`, where set, fixes a natural display order (used for bar/line
#' charts in the "Naar achtergrond" tab); `NULL` means "no natural order" --
#' the "Naar gebied" tab instead ranks by value.
C4C_BREAKDOWNS <- list(
  geslacht = list(
    label = "Geslacht", kind = "demografisch",
    levels = c("Mannen", "Vrouwen")
  ),
  age_cat = list(
    label = "Leeftijdsgroep", kind = "demografisch",
    levels = c("0-49", "50-59", "60-69", "70-79", "80-older")
  ),
  herkomst3 = list(
    label = "Migratieachtergrond (3 groepen)", kind = "demografisch",
    levels = c("Nederlandse herkomst", "Kind van migrant", "Migrant")
  ),
  herkomst7 = list(
    label = "Migratieachtergrond (naar regio)", kind = "demografisch",
    levels = c(
      "Nederland", "Europa (exclusief Nederland)", "Turkije", "Marokko",
      "Suriname", "Nederlands-Caribisch gebied", "Indonesië",
      "Overig Afrika, Azië, Amerika en Oceanië"
    )
  ),
  seswoa_cat = list(
    label = "Sociaaleconomische positie (SESWOA)", kind = "demografisch",
    levels = c("0-25%", "25-50%", "50-75%", "75-100%", "Onbekend")
  ),
  huishsamstsocec = list(
    label = "Huishoudsamenstelling", kind = "demografisch",
    levels = c(
      "Eenpersoonshuishouden", "Eenouderhuishouden", "Paar met kinderen",
      "Paar zonder kinderen", "Overig meerpersoonshuishouden",
      "Institutioneel huishouden", "Onbekend"
    )
  ),
  inkomen_klasse = list(
    label = "Inkomensklasse", kind = "demografisch",
    levels = c("tot_120", "120_280", "280_400", "400+", "student", "Onbekend_institutioneel")
  ),
  stadsdeel = list(
    label = "Stadsdeel", kind = "geografisch",
    levels = c("Centrum", "West", "Nieuw-West", "Zuid", "Oost", "Noord", "Zuidoost", "Westpoort", "Weesp", "Onbekend")
  ),
  wijk_25 = list(
    label = "Gebied (buurtcombinatie)", kind = "geografisch",
    levels = NULL
  )
)
