# Cardio4Cities dashboard
#
# Explores the aggregated, already CBS-cleared output of the Cardio4Cities
# pipeline (see the project's own `code/00_inputs.R` through
# `code/06_make_desritpives.R`): Amsterdam residents' use of risk-factor
# medication (diabetes, cholesterol, blood pressure) and first-time/major
# cardiovascular events (heart attack, acute stroke -- hospital admission or
# death), 2006-2024. Every number here is a whole-population registry count
# that has already been through the pipeline's own CBS output rules (a cell
# covering fewer than 10 people is dropped, every denominator rounded to the
# nearest 10) -- see `utils/cardio_data.R` for the loader and pure helper
# functions, and `data/metadata/outcome_metadata.R` for what each raw
# outcome column means.

source("data/metadata/brand_colors.R")
source("data/metadata/outcome_metadata.R")
source("utils/format_thinkcell_download.R")
source("utils/slide_download.R")
source("utils/template_admin.R")
source("utils/favorites.R")
source("utils/export_history.R")
source("utils/chart_downloads.R")
source("utils/dictionary.R")
source("data/metadata/dictionary_seed.R")
source("utils/dictionary_admin.R")
source("utils/tab_theme.R")
source("utils/cardio_data.R")

library(shiny)
library(dplyr)
library(ggplot2)

DASHBOARD_TITLE <- "Cardio4Cities — hart- en vaatziekten in Amsterdam"

# Loaded once at app start -- this is static, committed reference data (the
# pipeline's own export), not something that changes while the app runs, so
# every session shares the same in-memory copy rather than re-reading the
# CSV per session.
OUTCOMES_DATA <- c4c_load_outcomes()
OUTCOMES_SOURCE_FILE <- basename(C4C_OUTCOMES_FILE)
OUTCOMES_SOURCE_MTIME <- tc_format_source_mtime(C4C_OUTCOMES_FILE)

GEO_STADSDEEL <- c4c_load_geo_stadsdeel()
GEO_WIJK <- c4c_load_geo_wijk()
GEO_WIJK25 <- c4c_load_geo_wijk25()

# "Naar gebied" niveaus that offer a choropleth map, in addition to the bar
# chart every geografisch niveau always gets -- see gebied_map_data().
GEBIED_MAP_LEVELS <- c("stadsdeel", "wijk", "wijk_25")
GEBIED_MAP_LEVELS_JS <- paste(
  sprintf("input.gebied_niveau == '%s'", GEBIED_MAP_LEVELS), collapse = " || "
)

# Never print/format a number in scientific notation anywhere in this app
# (tables included) -- matches the pipeline's own `options(scipen = 999)`
# in `code/00_inputs.R`.
options(scipen = 999)

DEFAULT_INDICATOR <- "heeft_hartinfarct_of_acute_beroerte_lbz_of_do"

DEMOGRAFISCHE_BREAKDOWNS <- Filter(function(b) identical(b$kind, "demografisch"), C4C_BREAKDOWNS)
GEOGRAFISCHE_BREAKDOWNS <- Filter(function(b) identical(b$kind, "geografisch"), C4C_BREAKDOWNS)

#' Named vector of breakdown ids for a `selectInput`, named by their
#' `C4C_BREAKDOWNS` label.
breakdown_select_choices <- function(breakdowns) {
  stats::setNames(names(breakdowns), vapply(breakdowns, function(b) b$label, character(1)))
}

#' A plain HTML5 color-picker input (`<input type="color">`), wired to Shiny
#' with a `Shiny.setInputValue()` call on every `input` event (fires live
#' while dragging the swatch, not just once the picker closes). Deliberately
#' not the `colourpicker` package -- it isn't installed, and this dashboard's
#' deploy target has no general internet access to fetch a new CRAN package.
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label.
#' @param value Initial hex color (e.g. `"#EE3124"`); mirror this same
#'   default in the server-side reactive that reads `input[[inputId]]`, since
#'   the server only learns of a value once the user first interacts with it.
color_picker_input <- function(inputId, label, value = "#000000") {
  tags$div(
    class = "form-group shiny-input-container",
    tags$label(label, `for` = inputId, class = "control-label"),
    tags$input(
      type = "color", id = inputId, value = value,
      style = "display: block; width: 100%; height: 34px; padding: 2px; border: 1px solid #ccc; border-radius: 4px;",
      oninput = sprintf("Shiny.setInputValue('%s', this.value)", inputId)
    )
  )
}

#' A row of clickable preset low/high gradient swatches for two
#' `color_picker_input()` pickers, for a user who doesn't want to pick two
#' colors by hand. Pure client-side: a click sets each `<input type="color">`
#' element's own `.value` and dispatches a plain `"input"` event on it -- the
#' same event `color_picker_input()` already listens for -- so the visible
#' swatch and the server-side value both update, with no extra server code.
#' @param low_input_id,high_input_id The `color_picker_input()` ids to drive.
#' @param presets A list of `list(label=, low=, high=)` (hex colors).
color_preset_buttons <- function(low_input_id, high_input_id, presets) {
  tags$div(
    style = "display: flex; gap: 8px; margin: -6px 0 12px;",
    lapply(presets, function(p) {
      set_and_fire <- function(id, hex) {
        sprintf(
          "var el = document.getElementById('%s'); el.value = '%s'; el.dispatchEvent(new Event('input', {bubbles: true}));",
          id, hex
        )
      }
      tags$button(
        type = "button", title = p$label,
        onclick = paste(set_and_fire(low_input_id, p$low), set_and_fire(high_input_id, p$high)),
        style = sprintf(
          "flex: 1; height: 28px; padding: 0; border: 1px solid #ccc; border-radius: 4px; cursor: pointer; background: linear-gradient(to right, %s, %s);",
          p$low, p$high
        )
      )
    })
  )
}

#' This dashboard's 3 curated low/high color-scale presets for the "Naar
#' gebied" map, passed to `color_preset_buttons()`. Each is a light tint of
#' its high color paired with that color itself (one hue, light to full),
#' kept consistent with `ahti_branding$colors` (the first, red, matches the
#' map's own long-standing default).
GEBIED_KLEUR_PRESETS <- list(
  list(label = "Rood (standaard)", low = "#FBEAE9", high = ahti_branding$colors$fris_rood),
  list(label = "Blauw", low = "#E3F4FB", high = ahti_branding$colors$helder_blauw),
  list(label = "Groen", low = "#E3F7ED", high = ahti_branding$colors$fris_groen)
)

#' 3 more curated presets, this time with a genuinely different color at
#' each end (not one hue's light tint) -- for a user who wants the minimum
#' and maximum to read as two distinct colors rather than one color's
#' intensity.
GEBIED_KLEUR_PRESETS_TWEEKLEURIG <- list(
  # Was "Blauw naar rood" (helder_blauw -> fris_rood) -- too close in look to
  # "Grijsblauw naar rood" below (same high color, and the two blues read as
  # near-identical at a glance). Replaced with a pair sharing neither end.
  list(label = "Blauw naar groen", low = ahti_branding$colors$helder_blauw, high = ahti_branding$colors$fris_groen),
  list(label = "Groen naar paars", low = ahti_branding$colors$fris_groen, high = ahti_branding$colors$diep_paars),
  list(label = "Grijsblauw naar rood", low = ahti_branding$colors$grijs_blauw, high = ahti_branding$colors$fris_rood)
)

#' Dutch-formatted number (`.` thousands separator, `,` decimals) for a data
#' table cell -- never scientific notation, regardless of magnitude.
#' @param x Numeric vector.
#' @param digits Decimal places to round to.
fmt_num <- function(x, digits = 2) {
  format(round(x, digits), big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
}

#' Round `x` outward (down for a lower bound, up for an upper bound) to
#' `digits` decimal places -- unlike a plain `floor()`/`ceiling()` (which
#' round to whole numbers), this keeps enough precision for a small-scale
#' metric such as a rare outcome's population-wide percentage (e.g.
#' 0.18-0.34%): `floor(0.18)`/`ceiling(0.34)` both collapse to values that
#' don't even occur in the data (0 and 1), washing out the color scale.
#' @param x Numeric vector.
#' @param digits Decimal places to keep.
floor_dp <- function(x, digits = 2) floor(x * 10^digits) / 10^digits
#' @rdname floor_dp
ceiling_dp <- function(x, digits = 2) ceiling(x * 10^digits) / 10^digits

# Shared ggplot label formatters -- never scientific notation:
# - VALUE_AXIS_LABELS: for a count/percentage/rate axis, with a thousands
#   separator (e.g. "600.000").
# - YEAR_AXIS_LABELS: for a year axis -- no thousands separator, or 2024
#   would render as "2.024".
VALUE_AXIS_LABELS <- scales::label_number(big.mark = ".", decimal.mark = ",")
YEAR_AXIS_LABELS <- scales::label_number(big.mark = "", accuracy = 1)
YEAR_AXIS_BREAKS <- scales::breaks_pretty(n = 10)

#' A value-axis `scale_y_continuous()`, toggled by each tab's own "Y-as bij 0
#' laten beginnen" checkbox. Checked (the default, matching this dashboard's
#' long-standing behavior): floor the axis at 0, with `expand` set to no
#' padding below it -- `limits = c(0, NA)` alone still lets ggplot2's default
#' symmetric expansion dip the panel just under 0. Unchecked: ggplot2's own
#' auto-scaled range, for a selection whose values sit far from 0 and would
#' otherwise render as a nearly flat line.
#' @param start_at_zero The relevant tab's checkboxInput() value.
#' @param labels Passed through to `scale_y_continuous(labels = )`.
y_axis_scale <- function(start_at_zero, labels = VALUE_AXIS_LABELS) {
  if (isTRUE(start_at_zero)) {
    scale_y_continuous(labels = labels, limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))
  } else {
    scale_y_continuous(labels = labels)
  }
}

#' Shared choropleth-building logic for the "Naar gebied" tab's two map
#' views -- the single-year map (`gebied_map_plot`) and the delta map
#' (`gebied_delta_map_plot`) -- which differ only in which data feeds
#' `waarde` and what the fill legend is titled; everything else (polygon
#' grouping, aspect ratio, the user-adjustable color scale, the stadsdeel
#' name labels) is identical.
#' @param df Geo-joined data (`c4c_geo_join_stadsdeel()`/`_wijk()`/
#'   `_wijk25()` output) with a `waarde` column.
#' @param niveau One of `names(GEOGRAFISCHE_BREAKDOWNS)`.
#' @param fill_label Fill legend title.
#' @param kleur_min,kleur_max,kleur_laag,kleur_hoog The shared color-scale
#'   inputs (`gebied_kleur_*`) -- `kleur_laag`/`kleur_hoog` fall back to the
#'   map's original default (light red -> fris rood) when `NULL`, i.e.
#'   before the user has ever touched those color pickers.
gebied_choropleth <- function(df, niveau, fill_label, kleur_min, kleur_max, kleur_laag, kleur_hoog) {
  group_var <- if (identical(niveau, "stadsdeel")) {
    df$stadsdeel
  } else if (identical(niveau, "wijk")) {
    interaction(df$wijk, df$part)
  } else {
    interaction(df$wijk_25, df$part)
  }
  lat_ratio <- 1 / cos(mean(df$lat) * pi / 180)
  kleur_laag <- if (!is.null(kleur_laag)) kleur_laag else "#FBEAE9"
  kleur_hoog <- if (!is.null(kleur_hoog)) kleur_hoog else ahti_branding$colors$fris_rood
  p <- ggplot(df, aes(x = long, y = lat, group = group_var, fill = waarde)) +
    geom_polygon(color = "white", linewidth = 0.3) +
    coord_fixed(ratio = lat_ratio) +
    scale_fill_gradient(
      low = kleur_laag, high = kleur_hoog,
      name = fill_label,
      labels = VALUE_AXIS_LABELS,
      limits = c(kleur_min, kleur_max),
      na.value = "grey80"
    ) +
    labs(x = NULL, y = NULL) +
    theme_void() +
    theme(legend.position = "right")
  if (identical(niveau, "stadsdeel")) {
    labels_df <- c4c_geo_label_points(df, "stadsdeel")
    p <- p + geom_label(
      data = labels_df, aes(x = long, y = lat, label = stadsdeel), inherit.aes = FALSE,
      size = 3, fontface = "bold", color = "#1a1a1a", fill = "white", alpha = 0.75, label.size = 0
    )
  }
  p
}

app_ui <- fluidPage(
  tc_tab_color_theme(ahti_branding),
  titlePanel(DASHBOARD_TITLE),
  tabsetPanel(
    id = "main_nav",

    tabPanel(
      "Overzicht",
      fluidRow(column(
        width = 12,
        h3("Amsterdam: hart- en vaatziekten in de tijd"),
        p(
          "Cijfers zijn gebaseerd op alle Amsterdammers die op 31 december van elk ",
          "jaar in de Basisregistratie Personen stonden ingeschreven (2006–2024). ",
          "Aantallen komen uit ziekenhuisopnames (LBZ), doodsoorzakenstatistiek en ",
          "medicijngebruik (ATC-codes); cellen op basis van minder dan 10 personen ",
          "zijn conform CBS-regels niet in dit bestand opgenomen."
        )
      )),
      fluidRow(
        column(
          width = 3,
          selectInput(
            "overzicht_indicator", "Indicator",
            choices = c4c_outcome_choices(), selected = DEFAULT_INDICATOR
          ),
          uiOutput("overzicht_metric_ui"),
          uiOutput("overzicht_jaren_ui"),
          checkboxInput("overzicht_as_bij_nul", "Y-as bij 0 laten beginnen", value = TRUE)
        ),
        column(
          width = 9,
          plotOutput("overzicht_plot"),
          br(),
          chart_data_downloads_ui("overzicht_downloads", chart_type = "line"),
          h4("Onderliggende data"),
          tableOutput("overzicht_table")
        )
      )
    ),

    tabPanel(
      "Zorgpad",
      fluidRow(column(
        width = 12,
        h3("Eerste hartinfarct of beroerte: met of zonder eerdere medicatie?"),
        p(
          "Van iedere Amsterdammer met een eerste hartinfarct of acute beroerte ",
          "(ziekenhuisopname of overlijden) wordt hier bijgehouden of diegene in de ",
          "jaren daarvoor al medicijnen gebruikte tegen een risicofactor (hoge ",
          "bloeddruk, hoog cholesterol of diabetes type 2). Een grote groep ",
          "zonder eerdere medicatie kan wijzen op gemiste kansen voor vroege ",
          "opsporing en preventie."
        )
      )),
      fluidRow(
        column(
          width = 3,
          checkboxGroupInput(
            "zorgpad_groepen", "Groepen",
            choices = stats::setNames(C4C_ZORGPAD_ALL_NAMES, c4c_outcome_label(C4C_ZORGPAD_ALL_NAMES)),
            selected = C4C_ZORGPAD_ALL_NAMES
          ),
          selectInput(
            "zorgpad_dimensie", "Uitsplitsing",
            choices = c("Amsterdam totaal" = "yearly_total", breakdown_select_choices(C4C_BREAKDOWNS)),
            selected = "yearly_total"
          ),
          uiOutput("zorgpad_jaren_ui"),
          checkboxInput(
            "zorgpad_pct",
            "Toon als aandeel (%) binnen 'wel'/'geen gebeurtenis'",
            value = FALSE
          ),
          checkboxInput("zorgpad_as_bij_nul", "Y-as bij 0 laten beginnen", value = TRUE),
          conditionalPanel(
            condition = "input.zorgpad_dimensie != 'yearly_total'",
            uiOutput("zorgpad_dimensie_groepen_ui")
          )
        ),
        column(
          width = 9,
          plotOutput("zorgpad_plot"),
          br(),
          chart_data_downloads_ui("zorgpad_downloads", chart_type = "stacked_bar"),
          h4("Onderliggende data"),
          tableOutput("zorgpad_table"),
          hr(),
          h4("Incidentie: eerste hartinfarct of beroerte (% van de risicogroep)"),
          p(
            "Van de Amsterdammers die dat jaar nog geen eerder hartinfarct of ",
            "acute beroerte hadden gehad (de risicogroep), kreeg dit deel er voor ",
            "het eerst één."
          ),
          plotOutput("zorgpad_incidentie_plot"),
          br(),
          chart_data_downloads_ui("zorgpad_incidentie_downloads", chart_type = "line"),
          h4("Onderliggende data"),
          tableOutput("zorgpad_incidentie_table")
        )
      )
    ),

    tabPanel(
      "Naar achtergrond",
      fluidRow(column(
        width = 12,
        h3("Hart- en vaatziekten naar bevolkingsgroep"),
        p(
          "Vergelijk uitkomsten tussen groepen Amsterdammers: leeftijd, geslacht, ",
          "migratieachtergrond, sociaaleconomische positie (SESWOA), ",
          "huishoudsamenstelling of inkomensklasse."
        )
      )),
      fluidRow(
        column(
          width = 3,
          selectInput(
            "achtergrond_indicator", "Indicator",
            choices = c4c_outcome_choices(), selected = DEFAULT_INDICATOR
          ),
          selectInput(
            "achtergrond_dimensie", "Bevolkingsgroep",
            choices = breakdown_select_choices(DEMOGRAFISCHE_BREAKDOWNS)
          ),
          uiOutput("achtergrond_metric_ui"),
          checkboxInput(
            "achtergrond_trend",
            "Toon trend door de jaren (in plaats van één jaar)",
            value = FALSE
          ),
          conditionalPanel(
            condition = "!input.achtergrond_trend",
            uiOutput("achtergrond_jaar_ui")
          ),
          conditionalPanel(
            condition = "input.achtergrond_trend",
            uiOutput("achtergrond_jaren_ui")
          ),
          uiOutput("achtergrond_groepen_ui"),
          checkboxInput("achtergrond_as_bij_nul", "Y-as bij 0 laten beginnen", value = TRUE)
        ),
        column(
          width = 9,
          plotOutput("achtergrond_plot"),
          br(),
          conditionalPanel(
            condition = "!input.achtergrond_trend",
            chart_data_downloads_ui("achtergrond_downloads_bar", chart_type = "bar")
          ),
          conditionalPanel(
            condition = "input.achtergrond_trend",
            chart_data_downloads_ui("achtergrond_downloads_trend", chart_type = "line")
          ),
          h4("Onderliggende data"),
          tableOutput("achtergrond_table")
        )
      )
    ),

    tabPanel(
      "Naar gebied",
      fluidRow(column(
        width = 12,
        h3("Hart- en vaatziekten naar gebied in Amsterdam"),
        p("Vergelijk stadsdelen, gebieden (buurtcombinaties) of wijken met elkaar voor één gekozen jaar.")
      )),
      fluidRow(
        column(
          width = 3,
          selectInput(
            "gebied_indicator", "Indicator",
            choices = c4c_outcome_choices(), selected = DEFAULT_INDICATOR
          ),
          selectInput(
            "gebied_niveau", "Niveau",
            choices = breakdown_select_choices(GEOGRAFISCHE_BREAKDOWNS)
          ),
          uiOutput("gebied_metric_ui"),
          conditionalPanel(
            condition = "input.gebied_weergave != 'delta'",
            uiOutput("gebied_jaar_ui")
          ),
          conditionalPanel(
            condition = "input.gebied_weergave == 'delta'",
            uiOutput("gebied_delta_jaren_ui")
          ),
          conditionalPanel(
            condition = paste0("!(", GEBIED_MAP_LEVELS_JS, ") || input.gebied_weergave == 'bar'"),
            uiOutput("gebied_gebieden_ui"),
            checkboxInput("gebied_as_bij_nul", "Y-as bij 0 laten beginnen", value = TRUE)
          ),
          conditionalPanel(
            condition = GEBIED_MAP_LEVELS_JS,
            radioButtons(
              "gebied_weergave", "Weergave",
              choices = c("Staafdiagram" = "bar", "Kaart" = "map", "Delta kaart" = "delta")
            ),
            conditionalPanel(
              condition = "input.gebied_weergave == 'map' || input.gebied_weergave == 'delta'",
              numericInput("gebied_kleur_min", "Kleurschaal: minimum", value = NA),
              numericInput("gebied_kleur_max", "Kleurschaal: maximum", value = NA),
              color_picker_input("gebied_kleur_laag", "Kleurschaal: kleur bij minimum", value = "#FBEAE9"),
              color_picker_input("gebied_kleur_hoog", "Kleurschaal: kleur bij maximum", value = ahti_branding$colors$fris_rood),
              tags$label("Of kies een standaardcombinatie (één kleur)", class = "control-label", style = "font-weight: normal; color: #666;"),
              color_preset_buttons("gebied_kleur_laag", "gebied_kleur_hoog", GEBIED_KLEUR_PRESETS),
              tags$label("Of een combinatie met twee kleuren", class = "control-label", style = "font-weight: normal; color: #666;"),
              color_preset_buttons("gebied_kleur_laag", "gebied_kleur_hoog", GEBIED_KLEUR_PRESETS_TWEEKLEURIG)
            )
          )
        ),
        column(
          width = 9,
          conditionalPanel(
            condition = paste0("!(", GEBIED_MAP_LEVELS_JS, ") || input.gebied_weergave == 'bar'"),
            plotOutput("gebied_plot", height = "620px"),
            br(),
            chart_data_downloads_ui("gebied_downloads", chart_type = "bar"),
            h4("Onderliggende data"),
            tableOutput("gebied_table")
          ),
          conditionalPanel(
            condition = paste0("(", GEBIED_MAP_LEVELS_JS, ") && input.gebied_weergave == 'map'"),
            div(
              style = "position: relative;",
              plotOutput(
                "gebied_map", height = "620px",
                hover = hoverOpts("gebied_map_hover", delay = 60, delayType = "debounce", nullOutside = TRUE)
              ),
              uiOutput("gebied_map_tooltip"),
              # Shiny's own shiny-busy/shiny-idle classes on <html> (no
              # extra package) -- without this, a (re)computing map is just
              # blank/stale for a moment with no feedback, easily mistaken
              # for hovering "not working".
              conditionalPanel(
                condition = "$('html').hasClass('shiny-busy')",
                div(
                  "Kaart wordt geladen...",
                  style = paste0(
                    "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);",
                    "background: white; border: 1px solid #D1D5DB; border-radius: 6px;",
                    "padding: 8px 16px; font-size: 14px; color: #374151; z-index: 200;"
                  )
                )
              )
            ),
            p(
              style = "font-size:12px; color:#6B7280;",
              "Gebieden zonder actuele grens in de gebruikte open geodatabron (bijv. Weesp bij ",
              "Stadsdeel) en 'Onbekend' staan niet op de kaart, maar wel in het staafdiagram. ",
              "Beweeg de muis over een gebied voor de naam en waarde."
            ),
            downloadButton("gebied_map_download", "Download kaart (PNG)"),
            br(), br(),
            chart_data_downloads_ui("gebied_map_downloads", chart_type = "bar"),
            h4("Onderliggende data"),
            tableOutput("gebied_map_table")
          ),
          conditionalPanel(
            condition = paste0("(", GEBIED_MAP_LEVELS_JS, ") && input.gebied_weergave == 'delta'"),
            div(
              style = "position: relative;",
              plotOutput(
                "gebied_delta_map", height = "620px",
                hover = hoverOpts("gebied_delta_map_hover", delay = 60, delayType = "debounce", nullOutside = TRUE)
              ),
              uiOutput("gebied_delta_map_tooltip"),
              conditionalPanel(
                condition = "$('html').hasClass('shiny-busy')",
                div(
                  "Kaart wordt geladen...",
                  style = paste0(
                    "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);",
                    "background: white; border: 1px solid #D1D5DB; border-radius: 6px;",
                    "padding: 8px 16px; font-size: 14px; color: #374151; z-index: 200;"
                  )
                )
              )
            ),
            p(
              style = "font-size:12px; color:#6B7280;",
              "Toont per gebied het verschil tussen de twee hierboven gekozen jaren (\"Van jaar\" ",
              "min \"Naar jaar\" -- een afname over die periode is dus een positief getal). ",
              "Gebieden zonder actuele grens in de gebruikte open geodatabron en 'Onbekend' staan ",
              "niet op de kaart, maar wel in de tabel. Beweeg de muis over een gebied voor de ",
              "naam en het verschil."
            ),
            downloadButton("gebied_delta_map_download", "Download kaart (PNG)"),
            br(), br(),
            chart_data_downloads_ui("gebied_delta_downloads", chart_type = "bar"),
            h4("Onderliggende data"),
            tableOutput("gebied_delta_map_table")
          )
        )
      )
    ),

    tabPanel("Favorites", br(), favorites_panel_ui("favorites")),
    tabPanel("Export history", br(), export_history_panel_ui("export_history")),
    tabPanel("Manage templates", br(), template_admin_ui("template_admin")),
    tabPanel("Dictionary", br(), dictionary_admin_ui("dictionary"))
  )
)

ui <- app_ui

server <- function(input, output, session) {
  tc_register_app_context(
    input,
    dashboard_title = DASHBOARD_TITLE,
    nav_id = "main_nav",
    dl_option_prefixes = c(
      "overzicht_downloads"           = "^overzicht_",
      "zorgpad_downloads"             = "^zorgpad_",
      "zorgpad_incidentie_downloads"  = "^zorgpad_",
      "achtergrond_downloads_bar"     = "^achtergrond_",
      "achtergrond_downloads_trend"   = "^achtergrond_",
      "gebied_downloads"              = "^gebied_",
      "gebied_map_downloads"          = "^gebied_",
      "gebied_delta_downloads"        = "^gebied_"
    )
  )

  # ---------------------------------------------------------------------
  # Overzicht
  # ---------------------------------------------------------------------

  output$overzicht_metric_ui <- renderUI({
    kind <- c4c_outcome_kind(input$overzicht_indicator)
    selectInput("overzicht_metric", "Weergave", choices = c4c_available_metrics(kind, input$overzicht_indicator))
  })

  overzicht_title <- reactive({
    shiny::req(input$overzicht_indicator)
    paste0(c4c_outcome_label(input$overzicht_indicator), " — Amsterdam")
  })

  # All years for the chosen indicator, before the user's own year
  # selection is applied -- feeds the year picker's choices and the plot
  # data alike.
  overzicht_available_data <- reactive({
    shiny::req(input$overzicht_indicator)
    kind <- c4c_outcome_kind(input$overzicht_indicator)
    outcome_type <- c4c_outcome_type(kind)
    c4c_filter_outcome(OUTCOMES_DATA, "yearly_total", input$overzicht_indicator, outcome_type)
  })

  output$overzicht_jaren_ui <- renderUI({
    years <- sort(unique(overzicht_available_data()$year))
    shiny::validate(shiny::need(length(years) > 0, "Geen jaren beschikbaar voor deze selectie."))
    sliderInput(
      "overzicht_jaren", "Jaren",
      min = min(years), max = max(years), value = c(min(years), max(years)),
      step = 1, sep = ""
    )
  })

  overzicht_plot_data <- reactive({
    shiny::req(input$overzicht_metric, input$overzicht_jaren)
    df <- dplyr::filter(
      overzicht_available_data(),
      .data$year >= input$overzicht_jaren[1], .data$year <= input$overzicht_jaren[2]
    )
    df <- c4c_apply_metric(df, input$overzicht_metric, full_df = OUTCOMES_DATA, breakdown_id = "yearly_total")
    df$reeks <- c4c_outcome_label(input$overzicht_indicator)
    df
  })

  output$overzicht_plot <- renderPlot({
    df <- overzicht_plot_data()
    shiny::validate(shiny::need(
      nrow(df) > 0,
      "Onvoldoende data beschikbaar voor deze selectie (mogelijk afgeschermd vanwege CBS-geheimhoudingsregels)."
    ))
    ggplot(df, aes(x = year, y = waarde)) +
      geom_line(color = ahti_branding$colors$helder_blauw, linewidth = 1) +
      geom_point(color = ahti_branding$colors$helder_blauw, size = 2) +
      scale_x_continuous(breaks = YEAR_AXIS_BREAKS, labels = YEAR_AXIS_LABELS) +
      y_axis_scale(input$overzicht_as_bij_nul) +
      labs(
        title = overzicht_title(),
        x = "Jaar", y = c4c_metric_axis_label(input$overzicht_metric)
      ) +
      theme_minimal()
  })

  output$overzicht_table <- renderTable({
    df <- overzicht_plot_data()
    shiny::req(nrow(df) > 0)
    df %>%
      dplyr::arrange(.data$year) %>%
      dplyr::transmute(Jaar = .data$year, Waarde = fmt_num(.data$waarde))
  })

  chart_data_downloads_server(
    id = "overzicht_downloads",
    data = overzicht_plot_data,
    chart_type = "line",
    category_col = "year",
    series_col = "reeks",
    value_col = "waarde",
    filename_prefix = "cardio4cities_overzicht",
    agg_fun = NULL,
    figure_title = overzicht_title,
    slide_title = overzicht_title,
    source_output = OUTCOMES_SOURCE_FILE,
    source_sheet = "yearly_total",
    source_mtime = OUTCOMES_SOURCE_MTIME
  )

  # ---------------------------------------------------------------------
  # Zorgpad
  # ---------------------------------------------------------------------

  zorgpad_title <- reactive({
    shiny::req(input$zorgpad_dimensie)
    base <- "Eerste hartinfarct of beroerte: met of zonder eerdere medicatie"
    if (identical(input$zorgpad_dimensie, "yearly_total")) {
      paste0(base, " (Amsterdam)")
    } else {
      paste0(base, " naar ", C4C_BREAKDOWNS[[input$zorgpad_dimensie]]$label)
    }
  })

  # All 4 groups, all years, for the currently chosen breakdown -- feeds the
  # year picker's choices and the "groepen binnen uitsplitsing" checkbox.
  zorgpad_all_data <- reactive({
    shiny::req(input$zorgpad_dimensie)
    c4c_zorgpad_data(OUTCOMES_DATA, breakdown_id = input$zorgpad_dimensie)
  })

  output$zorgpad_jaren_ui <- renderUI({
    years <- sort(unique(zorgpad_all_data()$year))
    shiny::validate(shiny::need(length(years) > 0, "Geen jaren beschikbaar voor deze uitsplitsing."))
    sliderInput(
      "zorgpad_jaren", "Jaren",
      min = min(years), max = max(years), value = c(min(years), max(years)),
      step = 1, sep = ""
    )
  })

  output$zorgpad_dimensie_groepen_ui <- renderUI({
    dim_id <- input$zorgpad_dimensie
    groepen <- levels(droplevels(c4c_order_groep(unique(zorgpad_all_data()$groep), dim_id)))
    if (length(groepen) == 0) groepen <- sort(unique(zorgpad_all_data()$groep))
    labels <- c4c_relabel_groep(groepen, dim_id)
    checkboxGroupInput(
      "zorgpad_dimensie_groepen", "Groepen binnen uitsplitsing",
      choices = stats::setNames(groepen, labels), selected = groepen
    )
  })

  zorgpad_data <- reactive({
    shiny::req(input$zorgpad_groepen, input$zorgpad_jaren, input$zorgpad_dimensie)
    df <- c4c_zorgpad_data(
      OUTCOMES_DATA,
      names = input$zorgpad_groepen,
      years = seq(input$zorgpad_jaren[1], input$zorgpad_jaren[2]),
      breakdown_id = input$zorgpad_dimensie
    )
    if (!identical(input$zorgpad_dimensie, "yearly_total")) {
      shiny::req(input$zorgpad_dimensie_groepen)
      df <- dplyr::filter(df, .data$groep %in% input$zorgpad_dimensie_groepen)
    }
    df
  })

  output$zorgpad_plot <- renderPlot({
    df <- zorgpad_data()
    shiny::validate(shiny::need(
      nrow(df) > 0,
      "Onvoldoende data beschikbaar voor deze selectie (geen groepen/jaren geselecteerd, of afgeschermd vanwege CBS-geheimhoudingsregels)."
    ))
    show_pct <- isTRUE(input$zorgpad_pct)
    position <- if (show_pct) "fill" else "dodge"
    y_lab <- if (show_pct) "Aandeel binnen 'wel'/'geen gebeurtenis'" else "Aantal personen"
    # Facetted by heeft_event (free y-axis): the two "event" groups (a few
    # honderd people) and the two "no event" groups (most of the
    # population) sit on wildly different scales -- one shared axis would
    # make the event bars invisible. A chosen breakdown adds a second facet
    # dimension (columns) via facet_grid instead of facet_wrap, since
    # scales="free_y" only frees the y-axis per row, not per cell.
    p <- ggplot(df, aes(x = factor(year), y = waarde, fill = heeft_medicatie)) +
      geom_col(position = position)
    p <- if (identical(input$zorgpad_dimensie, "yearly_total")) {
      p + facet_wrap(~heeft_event, scales = "free_y")
    } else {
      p + facet_grid(heeft_event ~ breakdown_label, scales = "free_y")
    }
    p <- p +
      scale_fill_manual(values = c(ahti_branding$colors$fris_rood, ahti_branding$colors$helder_blauw)) +
      labs(title = zorgpad_title(), x = "Jaar", y = y_lab, fill = NULL) +
      theme_minimal() +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 30, hjust = 1))
    if (show_pct) {
      p <- p + y_axis_scale(input$zorgpad_as_bij_nul, labels = scales::percent)
    } else {
      p <- p + y_axis_scale(input$zorgpad_as_bij_nul)
    }
    p
  })

  output$zorgpad_table <- renderTable({
    df <- zorgpad_data()
    shiny::req(nrow(df) > 0)
    df %>%
      dplyr::arrange(.data$year, .data$breakdown_label, .data$groep_label) %>%
      dplyr::transmute(
        Jaar = .data$year,
        Uitsplitsing = as.character(.data$breakdown_label),
        Groep = as.character(.data$groep_label),
        Aantal = fmt_num(.data$waarde, digits = 0)
      )
  })

  chart_data_downloads_server(
    id = "zorgpad_downloads",
    data = zorgpad_data,
    chart_type = "stacked_bar",
    category_col = "year",
    series_col = "heeft_medicatie",
    value_col = "waarde",
    facet_col = "facet_key",
    filename_prefix = "cardio4cities_zorgpad",
    agg_fun = NULL,
    figure_title = zorgpad_title,
    slide_title = zorgpad_title,
    source_output = OUTCOMES_SOURCE_FILE,
    source_sheet = reactive(input$zorgpad_dimensie),
    source_mtime = OUTCOMES_SOURCE_MTIME
  )

  # -- Incidence-percentage chart: first major CVD event divided by its
  # at-risk ("heeft_geen_eerdere_...") population -- see
  # C4C_ZORGPAD_INCIDENCE_NAME. Distinct from the 4-group cross-tab above
  # (which splits by medication history); shares the same breakdown, year
  # range and group-deselect controls.

  zorgpad_incidentie_title <- reactive({
    shiny::req(input$zorgpad_dimensie)
    base <- "Incidentie: eerste hartinfarct of acute beroerte (% van de risicogroep)"
    if (identical(input$zorgpad_dimensie, "yearly_total")) {
      paste0(base, " (Amsterdam)")
    } else {
      paste0(base, " naar ", C4C_BREAKDOWNS[[input$zorgpad_dimensie]]$label)
    }
  })

  # All years for the currently chosen breakdown, before the year-range
  # slider is applied.
  zorgpad_incidentie_all_data <- reactive({
    shiny::req(input$zorgpad_dimensie)
    breakdown_id <- input$zorgpad_dimensie
    df <- c4c_filter_outcome(OUTCOMES_DATA, breakdown_id, C4C_ZORGPAD_INCIDENCE_NAME, "n_totaal_gebruikers")
    df <- c4c_apply_metric(df, "incidence", full_df = OUTCOMES_DATA, breakdown_id = breakdown_id)
    df$breakdown_label <- c4c_breakdown_label(df$groep, breakdown_id)
    df
  })

  zorgpad_incidentie_data <- reactive({
    shiny::req(input$zorgpad_jaren, input$zorgpad_dimensie)
    df <- dplyr::filter(
      zorgpad_incidentie_all_data(),
      .data$year >= input$zorgpad_jaren[1], .data$year <= input$zorgpad_jaren[2]
    )
    if (!identical(input$zorgpad_dimensie, "yearly_total")) {
      shiny::req(input$zorgpad_dimensie_groepen)
      df <- dplyr::filter(df, .data$groep %in% input$zorgpad_dimensie_groepen)
    }
    df
  })

  output$zorgpad_incidentie_plot <- renderPlot({
    df <- zorgpad_incidentie_data()
    shiny::validate(shiny::need(
      nrow(df) > 0,
      "Onvoldoende data beschikbaar voor deze selectie (geen groepen/jaren geselecteerd, of afgeschermd vanwege CBS-geheimhoudingsregels)."
    ))
    n_groups <- length(unique(df$breakdown_label))
    ggplot(df, aes(x = year, y = waarde, color = breakdown_label, group = breakdown_label)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_color_manual(values = c4c_palette(n_groups, ahti_branding$scale_discrete)) +
      scale_x_continuous(breaks = YEAR_AXIS_BREAKS, labels = YEAR_AXIS_LABELS) +
      y_axis_scale(input$zorgpad_as_bij_nul) +
      labs(
        title = zorgpad_incidentie_title(), x = "Jaar",
        y = c4c_metric_axis_label("incidence"), color = NULL
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })

  output$zorgpad_incidentie_table <- renderTable({
    df <- zorgpad_incidentie_data()
    shiny::req(nrow(df) > 0)
    df %>%
      dplyr::arrange(.data$year, .data$breakdown_label) %>%
      dplyr::transmute(
        Jaar = .data$year,
        Uitsplitsing = as.character(.data$breakdown_label),
        `Incidentie (%)` = fmt_num(.data$waarde)
      )
  })

  chart_data_downloads_server(
    id = "zorgpad_incidentie_downloads",
    data = zorgpad_incidentie_data,
    chart_type = "line",
    category_col = "year",
    series_col = "breakdown_label",
    value_col = "waarde",
    filename_prefix = "cardio4cities_zorgpad_incidentie",
    agg_fun = NULL,
    figure_title = zorgpad_incidentie_title,
    slide_title = zorgpad_incidentie_title,
    source_output = OUTCOMES_SOURCE_FILE,
    source_sheet = reactive(input$zorgpad_dimensie),
    source_mtime = OUTCOMES_SOURCE_MTIME,
    series_scope = reactive(input$zorgpad_dimensie)
  )

  # ---------------------------------------------------------------------
  # Naar achtergrond
  # ---------------------------------------------------------------------

  output$achtergrond_metric_ui <- renderUI({
    kind <- c4c_outcome_kind(input$achtergrond_indicator)
    selectInput("achtergrond_metric", "Weergave", choices = c4c_available_metrics(kind, input$achtergrond_indicator))
  })

  achtergrond_title <- reactive({
    shiny::req(input$achtergrond_indicator, input$achtergrond_dimensie)
    dim_label <- C4C_BREAKDOWNS[[input$achtergrond_dimensie]]$label
    paste0(c4c_outcome_label(input$achtergrond_indicator), " naar ", dim_label)
  })

  # All years and all groups for the chosen indicator/dimension, before the
  # user's own year/group selection is applied -- feeds the year picker
  # (bar mode), the group picker (both modes), and the trend-line data.
  achtergrond_unfiltered_data <- reactive({
    shiny::req(input$achtergrond_indicator, input$achtergrond_dimensie, input$achtergrond_metric)
    kind <- c4c_outcome_kind(input$achtergrond_indicator)
    outcome_type <- c4c_outcome_type(kind)
    df <- c4c_filter_outcome(OUTCOMES_DATA, input$achtergrond_dimensie, input$achtergrond_indicator, outcome_type)
    df <- c4c_apply_metric(df, input$achtergrond_metric, full_df = OUTCOMES_DATA, breakdown_id = input$achtergrond_dimensie)
    df$groep_label <- c4c_relabel_groep(
      c4c_order_groep(df$groep, input$achtergrond_dimensie),
      input$achtergrond_dimensie
    )
    df$reeks <- c4c_outcome_label(input$achtergrond_indicator)
    df
  })

  output$achtergrond_jaar_ui <- renderUI({
    years <- sort(unique(achtergrond_unfiltered_data()$year), decreasing = TRUE)
    shiny::validate(shiny::need(length(years) > 0, "Geen jaren beschikbaar voor deze selectie."))
    selectInput("achtergrond_jaar", "Jaar", choices = years, selected = years[1])
  })

  output$achtergrond_jaren_ui <- renderUI({
    years <- sort(unique(achtergrond_unfiltered_data()$year))
    shiny::validate(shiny::need(length(years) > 0, "Geen jaren beschikbaar voor deze selectie."))
    sliderInput(
      "achtergrond_jaren", "Jaren",
      min = min(years), max = max(years), value = c(min(years), max(years)),
      step = 1, sep = ""
    )
  })

  output$achtergrond_groepen_ui <- renderUI({
    groepen <- levels(droplevels(c4c_order_groep(unique(achtergrond_unfiltered_data()$groep), input$achtergrond_dimensie)))
    if (length(groepen) == 0) groepen <- sort(unique(achtergrond_unfiltered_data()$groep))
    labels <- c4c_relabel_groep(groepen, input$achtergrond_dimensie)
    checkboxGroupInput(
      "achtergrond_groepen", "Groepen",
      choices = stats::setNames(groepen, labels), selected = groepen
    )
  })

  # Both years (in trend mode) and groups (both modes) selected by the user.
  achtergrond_base_data <- reactive({
    shiny::req(input$achtergrond_groepen)
    df <- dplyr::filter(achtergrond_unfiltered_data(), .data$groep %in% input$achtergrond_groepen)
    if (isTRUE(input$achtergrond_trend)) {
      shiny::req(input$achtergrond_jaren)
      df <- dplyr::filter(df, .data$year >= input$achtergrond_jaren[1], .data$year <= input$achtergrond_jaren[2])
    }
    df
  })

  # Single-year slice, for the bar chart/download.
  achtergrond_bar_data <- reactive({
    shiny::req(input$achtergrond_jaar)
    dplyr::filter(achtergrond_base_data(), .data$year == as.integer(input$achtergrond_jaar))
  })

  output$achtergrond_plot <- renderPlot({
    y_lab <- c4c_metric_axis_label(input$achtergrond_metric)
    if (isTRUE(input$achtergrond_trend)) {
      df <- achtergrond_base_data()
      shiny::validate(shiny::need(
        nrow(df) > 0,
        "Onvoldoende data beschikbaar voor deze selectie (geen groepen/jaren geselecteerd, mogelijk afgeschermd vanwege CBS-geheimhoudingsregels, of nog niet gemeten in deze periode)."
      ))
      n_groups <- length(unique(df$groep_label))
      ggplot(df, aes(x = year, y = waarde, color = groep_label, group = groep_label)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        scale_color_manual(values = c4c_palette(n_groups, ahti_branding$scale_discrete)) +
        scale_x_continuous(breaks = YEAR_AXIS_BREAKS, labels = YEAR_AXIS_LABELS) +
        y_axis_scale(input$achtergrond_as_bij_nul) +
        labs(title = achtergrond_title(), x = "Jaar", y = y_lab, color = NULL) +
        theme_minimal() +
        theme(legend.position = "bottom")
    } else {
      df <- achtergrond_bar_data()
      shiny::validate(shiny::need(
        nrow(df) > 0,
        "Onvoldoende data beschikbaar voor deze selectie (geen groepen geselecteerd, of afgeschermd vanwege CBS-geheimhoudingsregels)."
      ))
      ggplot(df, aes(x = groep_label, y = waarde)) +
        geom_col(fill = ahti_branding$colors$helder_blauw) +
        y_axis_scale(input$achtergrond_as_bij_nul) +
        labs(title = achtergrond_title(), x = NULL, y = y_lab) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    }
  })

  output$achtergrond_table <- renderTable({
    df <- if (isTRUE(input$achtergrond_trend)) achtergrond_base_data() else achtergrond_bar_data()
    shiny::req(nrow(df) > 0)
    df %>%
      dplyr::arrange(.data$year, .data$groep_label) %>%
      dplyr::transmute(
        Jaar = .data$year,
        Groep = as.character(.data$groep_label),
        Waarde = fmt_num(.data$waarde)
      )
  })

  chart_data_downloads_server(
    id = "achtergrond_downloads_bar",
    data = achtergrond_bar_data,
    chart_type = "bar",
    category_col = "groep_label",
    series_col = "reeks",
    value_col = "waarde",
    filename_prefix = "cardio4cities_naar_achtergrond",
    agg_fun = NULL,
    figure_title = achtergrond_title,
    slide_title = achtergrond_title,
    source_output = OUTCOMES_SOURCE_FILE,
    source_sheet = reactive(input$achtergrond_dimensie),
    source_mtime = OUTCOMES_SOURCE_MTIME,
    category_scope = reactive(input$achtergrond_dimensie)
  )

  chart_data_downloads_server(
    id = "achtergrond_downloads_trend",
    data = achtergrond_base_data,
    chart_type = "line",
    category_col = "year",
    series_col = "groep_label",
    value_col = "waarde",
    filename_prefix = "cardio4cities_naar_achtergrond_trend",
    agg_fun = NULL,
    figure_title = achtergrond_title,
    slide_title = achtergrond_title,
    source_output = OUTCOMES_SOURCE_FILE,
    source_sheet = reactive(input$achtergrond_dimensie),
    source_mtime = OUTCOMES_SOURCE_MTIME,
    series_scope = reactive(input$achtergrond_dimensie)
  )

  # ---------------------------------------------------------------------
  # Naar gebied
  # ---------------------------------------------------------------------

  output$gebied_metric_ui <- renderUI({
    kind <- c4c_outcome_kind(input$gebied_indicator)
    selectInput("gebied_metric", "Weergave", choices = c4c_available_metrics(kind, input$gebied_indicator))
  })

  gebied_title <- reactive({
    shiny::req(input$gebied_indicator, input$gebied_niveau, input$gebied_jaar)
    dim_label <- C4C_BREAKDOWNS[[input$gebied_niveau]]$label
    paste0(c4c_outcome_label(input$gebied_indicator), " naar ", dim_label, " (", input$gebied_jaar, ")")
  })

  gebied_available_data <- reactive({
    shiny::req(input$gebied_indicator, input$gebied_niveau, input$gebied_metric)
    kind <- c4c_outcome_kind(input$gebied_indicator)
    outcome_type <- c4c_outcome_type(kind)
    c4c_filter_outcome(OUTCOMES_DATA, input$gebied_niveau, input$gebied_indicator, outcome_type)
  })

  output$gebied_jaar_ui <- renderUI({
    years <- sort(unique(gebied_available_data()$year), decreasing = TRUE)
    shiny::validate(shiny::need(length(years) > 0, "Geen jaren beschikbaar voor deze selectie."))
    selectInput("gebied_jaar", "Jaar", choices = years, selected = years[1])
  })

  output$gebied_gebieden_ui <- renderUI({
    gebieden <- sort(unique(gebied_available_data()$groep))
    shiny::validate(shiny::need(length(gebieden) > 0, "Geen gebieden beschikbaar voor deze selectie."))
    labels <- c4c_relabel_groep(gebieden, input$gebied_niveau)
    selectInput(
      "gebied_gebieden", "Gebieden",
      choices = stats::setNames(gebieden, labels), selected = gebieden, multiple = TRUE
    )
  })
  # Hidden (not just visually collapsed) while the map is showing -- the
  # selector matters for the bar chart, not for the map, which always shows
  # every area. Force it to keep re-rendering even while hidden, though:
  # Shiny's default suspendWhenHidden would otherwise leave input$gebied_gebieden
  # stuck on a stale niveau's area codes until the user flips back to the bar
  # chart, making the map briefly render as "no data" after a niveau switch.
  outputOptions(output, "gebied_gebieden_ui", suspendWhenHidden = FALSE)

  # Single-year slice with the user's own metric/area selection applied --
  # shared by the bar chart, the map, the download and the data table.
  gebied_selected_data <- reactive({
    shiny::req(input$gebied_jaar, input$gebied_metric, input$gebied_gebieden)
    df <- dplyr::filter(
      gebied_available_data(),
      .data$year == as.integer(input$gebied_jaar), .data$groep %in% input$gebied_gebieden
    )
    c4c_apply_metric(df, input$gebied_metric, full_df = OUTCOMES_DATA, breakdown_id = input$gebied_niveau)
  })

  gebied_plot_data <- reactive({
    df <- gebied_selected_data()
    df$groep_label <- c4c_relabel_groep(df$groep, input$gebied_niveau)
    df$reeks <- c4c_outcome_label(input$gebied_indicator)
    df
  })

  output$gebied_plot <- renderPlot({
    df <- gebied_plot_data()
    shiny::validate(shiny::need(
      nrow(df) > 0,
      "Onvoldoende data beschikbaar voor deze selectie (geen gebieden geselecteerd, of afgeschermd vanwege CBS-geheimhoudingsregels)."
    ))
    ggplot(df, aes(x = stats::reorder(groep_label, waarde), y = waarde)) +
      geom_col(fill = ahti_branding$colors$helder_blauw) +
      coord_flip() +
      y_axis_scale(input$gebied_as_bij_nul) +
      labs(title = gebied_title(), x = NULL, y = c4c_metric_axis_label(input$gebied_metric)) +
      theme_minimal()
  })

  output$gebied_table <- renderTable({
    df <- gebied_plot_data()
    shiny::req(nrow(df) > 0)
    df %>%
      dplyr::arrange(dplyr::desc(.data$waarde)) %>%
      dplyr::transmute(Gebied = as.character(.data$groep_label), Waarde = fmt_num(.data$waarde))
  })

  gebied_map_data <- reactive({
    shiny::validate(shiny::need(
      input$gebied_niveau %in% GEBIED_MAP_LEVELS,
      paste0(
        "Kaart is alleen beschikbaar voor: ",
        paste(vapply(GEBIED_MAP_LEVELS, function(id) C4C_BREAKDOWNS[[id]]$label, character(1)), collapse = ", "),
        "."
      )
    ))
    switch(input$gebied_niveau,
      stadsdeel = c4c_geo_join_stadsdeel(GEO_STADSDEEL, gebied_selected_data()),
      wijk = c4c_geo_join_wijk(GEO_WIJK, gebied_selected_data()),
      wijk_25 = c4c_geo_join_wijk25(GEO_WIJK25, gebied_selected_data())
    )
  })

  # Shared by output$gebied_map (on-screen) and gebied_map_download (PNG
  # export) -- built once so the two can never drift apart.
  gebied_map_plot <- reactive({
    df <- gebied_map_data()
    shiny::validate(shiny::need(
      nrow(df) > 0,
      "Onvoldoende data beschikbaar voor deze selectie (mogelijk afgeschermd vanwege CBS-geheimhoudingsregels)."
    ))
    # Color-scale bounds are user-adjustable (gebied_kleur_min/max, reset to
    # the current selection's own data range by the observeEvent() below) --
    # NA on either side falls back to ggplot2's own data-range default for
    # that side. The low/high colors are user-adjustable too
    # (gebied_kleur_laag/hoog, plain HTML5 color pickers -- see
    # color_picker_input()) and shared with the delta map below.
    gebied_choropleth(
      df, input$gebied_niveau, c4c_metric_axis_label(input$gebied_metric),
      input$gebied_kleur_min, input$gebied_kleur_max, input$gebied_kleur_laag, input$gebied_kleur_hoog
    )
  })

  # On screen the map keeps its title; the PNG download (gebied_map_download,
  # below) deliberately omits it, using gebied_map_plot() directly.
  output$gebied_map <- renderPlot({
    gebied_map_plot() + labs(title = gebied_title())
  })

  # Reset the color-scale bounds to the currently selected data's own min/max
  # whenever indicator/metric/niveau/jaar/gebieden/weergave changes -- i.e.
  # the fields always default to the exact selection shown on screen
  # (gebied_selected_data() for the single-year map, gebied_delta_data() for
  # the delta map -- both feed the same shared kleur controls). A
  # manually-typed override is only kept until the next such change, since
  # there is no way to tell "the user typed this on purpose" apart from
  # "this is last selection's stale default" once the underlying selection
  # itself changes.
  observeEvent(
    list(
      input$gebied_indicator, input$gebied_metric, input$gebied_niveau,
      input$gebied_jaar, input$gebied_gebieden, input$gebied_weergave,
      input$gebied_delta_jaar_start, input$gebied_delta_jaar_eind
    ),
    {
      df <- if (identical(input$gebied_weergave, "delta")) gebied_delta_data() else gebied_selected_data()
      shiny::req(nrow(df) > 0)
      waarde <- df$waarde[is.finite(df$waarde)]
      shiny::req(length(waarde) > 0)
      updateNumericInput(session, "gebied_kleur_min", value = floor_dp(min(waarde)))
      updateNumericInput(session, "gebied_kleur_max", value = ceiling_dp(max(waarde)))
    },
    ignoreInit = FALSE
  )

  # Hover tooltip (both niveaus, but the only way to see an area's name on
  # the map itself for wijk, which has no permanent labels): a plain
  # point-in-polygon test (c4c_area_at_point()) against the same polygons
  # just plotted -- no plotly/sf dependency needed. input$gebied_map_hover's
  # $x/$y are already in the plot's data units (lon/lat), regardless of
  # coord_fixed()'s display ratio.
  gebied_map_hover_info <- reactive({
    hover <- input$gebied_map_hover
    shiny::req(hover)
    df <- gebied_map_data()
    switch(input$gebied_niveau,
      stadsdeel = c4c_area_at_point(df, hover$x, hover$y, "stadsdeel"),
      wijk = c4c_area_at_point(df, hover$x, hover$y, "wijk", part_col = "part"),
      wijk_25 = c4c_area_at_point(df, hover$x, hover$y, "wijk_25", part_col = "part")
    )
  })

  output$gebied_map_tooltip <- renderUI({
    hit <- gebied_map_hover_info()
    shiny::req(hit)
    hover <- input$gebied_map_hover
    waarde_txt <- if (is.na(hit$waarde)) {
      "geen data"
    } else {
      paste(fmt_num(hit$waarde), c4c_metric_axis_label(input$gebied_metric))
    }
    style <- paste0(
      "position: absolute; z-index: 100; pointer-events: none;",
      "left:", hover$coords_css$x + 12, "px; top:", hover$coords_css$y + 12, "px;",
      "background: white; border: 1px solid #D1D5DB; border-radius: 4px;",
      "padding: 4px 8px; font-size: 13px; box-shadow: 0 1px 4px rgba(0,0,0,0.2);"
    )
    div(style = style, strong(hit$name), br(), waarde_txt)
  })

  output$gebied_map_download <- downloadHandler(
    filename = function() {
      paste0("cardio4cities_naar_gebied_kaart_", input$gebied_niveau, "_", input$gebied_jaar, ".png")
    },
    content = function(file) {
      ggsave(file, plot = gebied_map_plot(), width = 9, height = 7, dpi = 200, bg = "white")
    }
  )

  # -----------------------------------------------------------------------
  # Naar gebied -- delta map (year_end minus year_start per area)
  # -----------------------------------------------------------------------

  # Both years are freely user-selectable (gebied_delta_jaar_start/eind),
  # from whichever years are actually available for the current
  # indicator/niveau -- defaulting to 2013 (or 2016 for a handful of
  # outcomes only measured from then on, see c4c_delta_start_year()) vs the
  # latest available year, a sensible starting point the user can override.
  output$gebied_delta_jaren_ui <- renderUI({
    years <- sort(unique(gebied_available_data()$year))
    shiny::validate(shiny::need(
      length(years) >= 2,
      "Onvoldoende jaren beschikbaar voor deze selectie om een verschil te tonen."
    ))
    tagList(
      selectInput(
        "gebied_delta_jaar_start", "Van jaar",
        choices = years, selected = c4c_delta_start_year(years)
      ),
      selectInput("gebied_delta_jaar_eind", "Naar jaar", choices = years, selected = max(years))
    )
  })
  # Keeps re-rendering even while its conditionalPanel is hidden (weergave
  # != 'delta'), same reasoning as gebied_gebieden_ui above: otherwise a
  # niveau/indicator change made while not looking at the delta map would
  # leave these two selects (and so gebied_delta_years()) stuck on stale
  # year choices until the user happens to revisit the delta view once
  # first, at which point stale choices should have already been visible
  # rather than reset silently.
  outputOptions(output, "gebied_delta_jaren_ui", suspendWhenHidden = FALSE)

  gebied_delta_years <- reactive({
    shiny::req(input$gebied_delta_jaar_start, input$gebied_delta_jaar_eind)
    list(start = as.integer(input$gebied_delta_jaar_start), end = as.integer(input$gebied_delta_jaar_eind))
  })

  # Uses the same (hidden-while-not-the-bar-chart) gebied_gebieden selection
  # as gebied_selected_data() -- in practice always "every area", since that
  # selector resets to select-all on every niveau change.
  gebied_delta_data <- reactive({
    shiny::req(input$gebied_metric, input$gebied_gebieden)
    years <- gebied_delta_years()
    df <- dplyr::filter(gebied_available_data(), .data$groep %in% input$gebied_gebieden)
    c4c_year_delta(
      df, years$start, years$end, input$gebied_metric,
      full_df = OUTCOMES_DATA, breakdown_id = input$gebied_niveau
    )
  })

  gebied_delta_plot_data <- reactive({
    df <- gebied_delta_data()
    df$groep_label <- c4c_relabel_groep(df$groep, input$gebied_niveau)
    df$reeks <- c4c_outcome_label(input$gebied_indicator)
    df
  })

  gebied_delta_title <- reactive({
    shiny::req(input$gebied_indicator, input$gebied_niveau)
    years <- gebied_delta_years()
    dim_label <- C4C_BREAKDOWNS[[input$gebied_niveau]]$label
    paste0(
      c4c_outcome_label(input$gebied_indicator), " naar ", dim_label,
      ": verandering ", years$start, "–", years$end
    )
  })

  gebied_delta_map_data <- reactive({
    shiny::validate(shiny::need(
      input$gebied_niveau %in% GEBIED_MAP_LEVELS,
      paste0(
        "Kaart is alleen beschikbaar voor: ",
        paste(vapply(GEBIED_MAP_LEVELS, function(id) C4C_BREAKDOWNS[[id]]$label, character(1)), collapse = ", "),
        "."
      )
    ))
    switch(input$gebied_niveau,
      stadsdeel = c4c_geo_join_stadsdeel(GEO_STADSDEEL, gebied_delta_data()),
      wijk = c4c_geo_join_wijk(GEO_WIJK, gebied_delta_data()),
      wijk_25 = c4c_geo_join_wijk25(GEO_WIJK25, gebied_delta_data())
    )
  })

  # Shared by output$gebied_delta_map (on-screen) and
  # gebied_delta_map_download (PNG export), same split as gebied_map_plot().
  gebied_delta_map_plot <- reactive({
    df <- gebied_delta_map_data()
    shiny::validate(shiny::need(
      nrow(df) > 0,
      paste0(
        "Onvoldoende data beschikbaar voor deze selectie (mogelijk afgeschermd vanwege ",
        "CBS-geheimhoudingsregels, of geen van beide referentiejaren beschikbaar)."
      )
    ))
    # The fill legend itself deliberately doesn't repeat the two chosen
    # years -- that'd need updating every time the user picks a different
    # "Van jaar"/"Naar jaar" pair, unlike the plot's own title (gebied_delta_title(),
    # only added on screen, not to this shared reactive -- see gebied_map_plot()'s
    # comment on that split) which already states them.
    fill_label <- paste0("Verschil in ", c4c_metric_axis_label(input$gebied_metric))
    gebied_choropleth(
      df, input$gebied_niveau, fill_label,
      input$gebied_kleur_min, input$gebied_kleur_max, input$gebied_kleur_laag, input$gebied_kleur_hoog
    )
  })

  # On screen the map keeps its title; the PNG download deliberately omits
  # it, using gebied_delta_map_plot() directly -- same split as the
  # single-year map.
  output$gebied_delta_map <- renderPlot({
    gebied_delta_map_plot() + labs(title = gebied_delta_title())
  })

  # Hover tooltip -- same c4c_area_at_point() approach as the single-year
  # map's gebied_map_hover_info(), kept as its own reactive/hover id rather
  # than shared, since both plotOutputs are always present in the DOM (just
  # conditionalPanel-hidden) and would otherwise fight over one hover input.
  gebied_delta_map_hover_info <- reactive({
    hover <- input$gebied_delta_map_hover
    shiny::req(hover)
    df <- gebied_delta_map_data()
    switch(input$gebied_niveau,
      stadsdeel = c4c_area_at_point(df, hover$x, hover$y, "stadsdeel"),
      wijk = c4c_area_at_point(df, hover$x, hover$y, "wijk", part_col = "part"),
      wijk_25 = c4c_area_at_point(df, hover$x, hover$y, "wijk_25", part_col = "part")
    )
  })

  output$gebied_delta_map_tooltip <- renderUI({
    hit <- gebied_delta_map_hover_info()
    shiny::req(hit)
    hover <- input$gebied_delta_map_hover
    waarde_txt <- if (is.na(hit$waarde)) "geen data" else fmt_num(hit$waarde)
    style <- paste0(
      "position: absolute; z-index: 100; pointer-events: none;",
      "left:", hover$coords_css$x + 12, "px; top:", hover$coords_css$y + 12, "px;",
      "background: white; border: 1px solid #D1D5DB; border-radius: 4px;",
      "padding: 4px 8px; font-size: 13px; box-shadow: 0 1px 4px rgba(0,0,0,0.2);"
    )
    div(style = style, strong(hit$name), br(), waarde_txt)
  })

  output$gebied_delta_map_download <- downloadHandler(
    filename = function() {
      years <- gebied_delta_years()
      paste0("cardio4cities_naar_gebied_delta_", input$gebied_niveau, "_", years$start, "_", years$end, ".png")
    },
    content = function(file) {
      ggsave(file, plot = gebied_delta_map_plot(), width = 9, height = 7, dpi = 200, bg = "white")
    }
  )

  # Always shows both the percentage-point and the absolute-count delta,
  # regardless of input$gebied_metric -- same convention as
  # gebied_map_table_data() for the single-year map.
  gebied_delta_map_table_data <- reactive({
    shiny::req(input$gebied_gebieden, input$gebied_niveau)
    years <- gebied_delta_years()
    base <- dplyr::filter(gebied_available_data(), .data$groep %in% input$gebied_gebieden)
    pct <- c4c_year_delta(base, years$start, years$end, "percentage")
    aantal <- c4c_year_delta(base, years$start, years$end, "absolute")
    dplyr::inner_join(
      dplyr::transmute(
        pct,
        groep = .data$groep,
        groep_label = c4c_relabel_groep(.data$groep, input$gebied_niveau),
        percentage_delta = .data$waarde
      ),
      dplyr::transmute(aantal, groep = .data$groep, aantal_delta = .data$waarde),
      by = "groep"
    )
  })

  output$gebied_delta_map_table <- renderTable({
    df <- gebied_delta_map_table_data()
    shiny::req(nrow(df) > 0)
    df %>%
      dplyr::arrange(.data$groep_label) %>%
      dplyr::transmute(
        Gebied = as.character(.data$groep_label),
        `Verschil aandeel (%-punt)` = fmt_num(.data$percentage_delta),
        `Verschil aantal` = fmt_num(.data$aantal_delta, digits = 0)
      )
  })

  chart_data_downloads_server(
    id = "gebied_delta_downloads",
    data = gebied_delta_plot_data,
    chart_type = "bar",
    category_col = "groep_label",
    series_col = "reeks",
    value_col = "waarde",
    filename_prefix = "cardio4cities_naar_gebied_delta",
    agg_fun = NULL,
    figure_title = gebied_delta_title,
    slide_title = gebied_delta_title,
    source_output = OUTCOMES_SOURCE_FILE,
    source_sheet = reactive(input$gebied_niveau),
    source_mtime = OUTCOMES_SOURCE_MTIME,
    category_scope = reactive(input$gebied_niveau)
  )

  # Always shows both percentage and absolute count, regardless of
  # input$gebied_metric -- computed independently via c4c_add_metric()
  # rather than reading gebied_selected_data()'s already-metric-applied
  # waarde, since that column only ever holds the one currently-picked
  # metric.
  gebied_map_table_data <- reactive({
    shiny::req(input$gebied_jaar, input$gebied_gebieden, input$gebied_niveau)
    base <- dplyr::filter(
      gebied_available_data(),
      .data$year == as.integer(input$gebied_jaar), .data$groep %in% input$gebied_gebieden
    )
    tibble::tibble(
      groep_label = c4c_relabel_groep(base$groep, input$gebied_niveau),
      percentage = c4c_add_metric(base, "percentage")$waarde,
      aantal = c4c_add_metric(base, "absolute")$waarde
    )
  })

  output$gebied_map_table <- renderTable({
    df <- gebied_map_table_data()
    shiny::req(nrow(df) > 0)
    df %>%
      dplyr::arrange(.data$groep_label) %>%
      dplyr::transmute(
        Gebied = as.character(.data$groep_label),
        `Aandeel (%)` = fmt_num(.data$percentage),
        Aantal = fmt_num(.data$aantal, digits = 0)
      )
  })

  chart_data_downloads_server(
    id = "gebied_downloads",
    data = gebied_plot_data,
    chart_type = "bar",
    category_col = "groep_label",
    series_col = "reeks",
    value_col = "waarde",
    filename_prefix = "cardio4cities_naar_gebied",
    agg_fun = NULL,
    figure_title = gebied_title,
    slide_title = gebied_title,
    source_output = OUTCOMES_SOURCE_FILE,
    source_sheet = reactive(input$gebied_niveau),
    source_mtime = OUTCOMES_SOURCE_MTIME,
    category_scope = reactive(input$gebied_niveau)
  )

  # Same underlying selection as "gebied_downloads" above -- just offered
  # again here so the data is downloadable from the map view too, without
  # switching to the bar chart first.
  chart_data_downloads_server(
    id = "gebied_map_downloads",
    data = gebied_plot_data,
    chart_type = "bar",
    category_col = "groep_label",
    series_col = "reeks",
    value_col = "waarde",
    filename_prefix = "cardio4cities_naar_gebied_kaart",
    agg_fun = NULL,
    figure_title = gebied_title,
    slide_title = gebied_title,
    source_output = OUTCOMES_SOURCE_FILE,
    source_sheet = reactive(input$gebied_niveau),
    source_mtime = OUTCOMES_SOURCE_MTIME,
    category_scope = reactive(input$gebied_niveau)
  )

  # ---------------------------------------------------------------------
  # Shared admin tabs
  # ---------------------------------------------------------------------

  favorites_panel_server("favorites")
  export_history_panel_server("export_history")
  template_admin_server("template_admin")
  dictionary_admin_server("dictionary")
}

shinyApp(ui = ui, server = server)
