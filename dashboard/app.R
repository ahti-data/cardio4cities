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
source("utils/auth.R")
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

# "Naar gebied" niveaus that offer a choropleth map, in addition to the bar
# chart every geografisch niveau always gets -- see gebied_map_data().
GEBIED_MAP_LEVELS <- c("stadsdeel", "wijk")
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

#' Dutch-formatted number (`.` thousands separator, `,` decimals) for a data
#' table cell -- never scientific notation, regardless of magnitude.
#' @param x Numeric vector.
#' @param digits Decimal places to round to.
fmt_num <- function(x, digits = 2) {
  format(round(x, digits), big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
}

# Shared ggplot label formatters -- never scientific notation:
# - VALUE_AXIS_LABELS: for a count/percentage/rate axis, with a thousands
#   separator (e.g. "600.000").
# - YEAR_AXIS_LABELS: for a year axis -- no thousands separator, or 2024
#   would render as "2.024".
VALUE_AXIS_LABELS <- scales::label_number(big.mark = ".", decimal.mark = ",")
YEAR_AXIS_LABELS <- scales::label_number(big.mark = "", accuracy = 1)
YEAR_AXIS_BREAKS <- scales::breaks_pretty(n = 10)

app_ui <- fluidPage(
  auth_ui_head(),
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
          uiOutput("overzicht_jaren_ui")
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
          uiOutput("achtergrond_groepen_ui")
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
          uiOutput("gebied_jaar_ui"),
          uiOutput("gebied_gebieden_ui"),
          conditionalPanel(
            condition = GEBIED_MAP_LEVELS_JS,
            radioButtons(
              "gebied_weergave", "Weergave",
              choices = c("Staafdiagram" = "bar", "Kaart" = "map")
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
            plotOutput("gebied_map", height = "620px"),
            p(
              style = "font-size:12px; color:#6B7280;",
              "Gebieden zonder actuele grens in de gebruikte open geodatabron (bijv. Weesp bij ",
              "Stadsdeel) en 'Onbekend' staan niet op de kaart, maar wel in het staafdiagram. ",
              "Ga naar de staafdiagram-weergave om de onderliggende data te downloaden."
            )
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

ui <- if (is_auth_enabled()) shinymanager::secure_app(app_ui) else app_ui

server <- function(input, output, session) {
  if (is_auth_enabled()) {
    setup_dashboard_auth(session)
  } else {
    show_app_without_auth(session)
  }

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
      "gebied_downloads"              = "^gebied_"
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
      scale_y_continuous(labels = VALUE_AXIS_LABELS) +
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
      p <- p + scale_y_continuous(labels = scales::percent)
    } else {
      p <- p + scale_y_continuous(labels = VALUE_AXIS_LABELS)
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
      scale_y_continuous(labels = VALUE_AXIS_LABELS) +
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
        scale_y_continuous(labels = VALUE_AXIS_LABELS) +
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
        scale_y_continuous(labels = VALUE_AXIS_LABELS) +
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
      scale_y_continuous(labels = VALUE_AXIS_LABELS) +
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
    if (identical(input$gebied_niveau, "stadsdeel")) {
      c4c_geo_join_stadsdeel(GEO_STADSDEEL, gebied_selected_data())
    } else {
      c4c_geo_join_wijk(GEO_WIJK, gebied_selected_data())
    }
  })

  output$gebied_map <- renderPlot({
    df <- gebied_map_data()
    shiny::validate(shiny::need(
      nrow(df) > 0,
      "Onvoldoende data beschikbaar voor deze selectie (mogelijk afgeschermd vanwege CBS-geheimhoudingsregels)."
    ))
    # Stadsdeel polygons are single-piece (group = stadsdeel); wijk polygons
    # can have several disjoint pieces per named area (group = wijk x part;
    # see c4c_load_geo_wijk()) -- either way ggplot draws one shape per
    # group and fills it by the shared waarde for that name.
    group_var <- if (identical(input$gebied_niveau, "stadsdeel")) {
      df$stadsdeel
    } else {
      interaction(df$wijk, df$part)
    }
    ggplot(df, aes(x = long, y = lat, group = group_var, fill = waarde)) +
      geom_polygon(color = "white", linewidth = 0.3) +
      coord_equal() +
      scale_fill_gradient(
        low = "#FBEAE9", high = ahti_branding$colors$fris_rood,
        name = c4c_metric_axis_label(input$gebied_metric),
        labels = VALUE_AXIS_LABELS
      ) +
      labs(title = gebied_title(), x = NULL, y = NULL) +
      theme_void() +
      theme(legend.position = "right")
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

  # ---------------------------------------------------------------------
  # Shared admin tabs
  # ---------------------------------------------------------------------

  favorites_panel_server("favorites")
  export_history_panel_server("export_history")
  template_admin_server("template_admin")
  dictionary_admin_server("dictionary")
}

shinyApp(ui = ui, server = server)
