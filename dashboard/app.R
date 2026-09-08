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
#
# (Deploy trigger: no functional change. Re-check after SFTP permissions update.)

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

LATEST_YEAR <- max(OUTCOMES_DATA$year[OUTCOMES_DATA$breakdown == "yearly_total"])

GEO_STADSDEEL <- c4c_load_geo_stadsdeel()

DEFAULT_INDICATOR <- "heeft_hartinfarct_of_acute_beroerte_lbz_of_do"

DEMOGRAFISCHE_BREAKDOWNS <- Filter(function(b) identical(b$kind, "demografisch"), C4C_BREAKDOWNS)
GEOGRAFISCHE_BREAKDOWNS <- Filter(function(b) identical(b$kind, "geografisch"), C4C_BREAKDOWNS)

#' Named vector of breakdown ids for a `selectInput`, named by their
#' `C4C_BREAKDOWNS` label.
breakdown_select_choices <- function(breakdowns) {
  stats::setNames(names(breakdowns), vapply(breakdowns, function(b) b$label, character(1)))
}

#' One KPI card: a big number over a short label, with a coloured left
#' accent bar. Presentation-only (not reusable business logic), so kept
#' inline here rather than in `utils/` -- see CLAUDE.md's "Add reusable
#' logic to utils/" convention, which is about shared logic, not one-off
#' markup for this dashboard's own KPI row.
kpi_box <- function(value, label, accent = ahti_branding$colors$helder_blauw) {
  tags$div(
    style = paste0(
      "flex:1; min-width:190px; background:#fff; border:1px solid #E4E7EE; ",
      "border-left:4px solid ", accent, "; border-radius:8px; ",
      "padding:14px 16px; margin:6px;"
    ),
    tags$div(style = "font-size:26px; font-weight:700; color:#111827;", value),
    tags$div(style = "font-size:13px; color:#6B7280; margin-top:2px;", label)
  )
}

#' Dutch-formatted big number (`.` thousands separator), `"–"` for `NA`.
fmt_n <- function(x) {
  if (is.na(x)) return("–")
  format(round(x), big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
}

#' Dutch-formatted percentage (`,` decimal separator), `"–"` for `NA`.
fmt_pct <- function(x, digits = 1) {
  if (is.na(x)) return("–")
  paste0(format(round(x, digits), decimal.mark = ",", nsmall = digits), "%")
}

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
      uiOutput("overzicht_kpis"),
      fluidRow(
        column(
          width = 3,
          selectInput(
            "overzicht_indicator", "Indicator",
            choices = c4c_outcome_choices(), selected = DEFAULT_INDICATOR
          ),
          uiOutput("overzicht_metric_ui")
        ),
        column(
          width = 9,
          plotOutput("overzicht_plot"),
          br(),
          chart_data_downloads_ui("overzicht_downloads", chart_type = "line")
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
      uiOutput("zorgpad_kpis"),
      fluidRow(
        column(
          width = 3,
          checkboxInput(
            "zorgpad_pct",
            "Toon als aandeel (%) van eerste gebeurtenissen dat jaar",
            value = FALSE
          )
        ),
        column(
          width = 9,
          plotOutput("zorgpad_plot"),
          br(),
          chart_data_downloads_ui("zorgpad_downloads", chart_type = "stacked_bar")
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
          )
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
          )
        )
      )
    ),

    tabPanel(
      "Naar gebied",
      fluidRow(column(
        width = 12,
        h3("Hart- en vaatziekten naar gebied in Amsterdam"),
        p("Vergelijk stadsdelen of gebieden (buurtcombinaties) met elkaar voor één gekozen jaar.")
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
          conditionalPanel(
            condition = "input.gebied_niveau == 'stadsdeel'",
            radioButtons(
              "gebied_weergave", "Weergave",
              choices = c("Staafdiagram" = "bar", "Kaart" = "map")
            )
          )
        ),
        column(
          width = 9,
          conditionalPanel(
            condition = "input.gebied_niveau != 'stadsdeel' || input.gebied_weergave == 'bar'",
            plotOutput("gebied_plot", height = "620px"),
            br(),
            chart_data_downloads_ui("gebied_downloads", chart_type = "bar")
          ),
          conditionalPanel(
            condition = "input.gebied_niveau == 'stadsdeel' && input.gebied_weergave == 'map'",
            plotOutput("gebied_map", height = "620px"),
            p(
              style = "font-size:12px; color:#6B7280;",
              "Weesp (nog geen actuele grens in de gebruikte open geodatabron) en ",
              "'Onbekend' staan niet op de kaart, maar wel in het staafdiagram. ",
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
    selectInput("overzicht_metric", "Weergave", choices = c4c_available_metrics(kind))
  })

  output$overzicht_kpis <- renderUI({
    kpi <- c4c_kpi_overzicht(OUTCOMES_DATA, LATEST_YEAR)
    tags$div(
      style = "display:flex; flex-wrap:wrap; margin: 0 -6px 10px;",
      kpi_box(fmt_n(kpi$population), paste0("Inwoners Amsterdam (", LATEST_YEAR, ")")),
      kpi_box(
        fmt_pct(kpi$medicatie_pct),
        "Gebruikt medicatie tegen een risicofactor",
        accent = ahti_branding$colors$fris_groen
      ),
      kpi_box(
        fmt_n(kpi$event_n),
        paste0("Personen met hartinfarct/beroerte in ", LATEST_YEAR),
        accent = ahti_branding$colors$fris_rood
      ),
      kpi_box(
        fmt_pct(kpi$event_pct, digits = 2),
        "Aandeel van de bevolking met zo'n gebeurtenis",
        accent = ahti_branding$colors$fris_rood
      )
    )
  })

  overzicht_title <- reactive({
    shiny::req(input$overzicht_indicator)
    paste0(c4c_outcome_label(input$overzicht_indicator), " — Amsterdam")
  })

  overzicht_plot_data <- reactive({
    shiny::req(input$overzicht_indicator, input$overzicht_metric)
    kind <- c4c_outcome_kind(input$overzicht_indicator)
    outcome_type <- c4c_outcome_type(kind)
    df <- c4c_filter_outcome(OUTCOMES_DATA, "yearly_total", input$overzicht_indicator, outcome_type)
    df <- c4c_add_metric(df, input$overzicht_metric)
    df$reeks <- c4c_outcome_label(input$overzicht_indicator)
    df
  })

  output$overzicht_plot <- renderPlot({
    df <- overzicht_plot_data()
    shiny::validate(shiny::need(
      nrow(df) > 0,
      "Onvoldoende data beschikbaar voor deze indicator (mogelijk afgeschermd vanwege CBS-geheimhoudingsregels)."
    ))
    ggplot(df, aes(x = year, y = waarde)) +
      geom_line(color = ahti_branding$colors$helder_blauw, linewidth = 1) +
      geom_point(color = ahti_branding$colors$helder_blauw, size = 2) +
      labs(
        title = overzicht_title(),
        x = "Jaar", y = c4c_metric_axis_label(input$overzicht_metric)
      ) +
      theme_minimal()
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
    "Eerste hartinfarct of beroerte: met of zonder eerdere medicatie (Amsterdam)"
  })

  zorgpad_data <- reactive({
    c4c_zorgpad_data(OUTCOMES_DATA)
  })

  output$zorgpad_kpis <- renderUI({
    kpi <- c4c_kpi_zorgpad(OUTCOMES_DATA, LATEST_YEAR)
    tags$div(
      style = "display:flex; flex-wrap:wrap; margin: 0 -6px 10px;",
      kpi_box(
        fmt_n(kpi$eerste_event_totaal_n),
        paste0("Eerste hartinfarct/beroerte in ", LATEST_YEAR)
      ),
      kpi_box(
        fmt_n(kpi$eerste_event_zonder_medicatie_n),
        "...zonder eerdere medicatie",
        accent = ahti_branding$colors$fris_rood
      ),
      kpi_box(
        fmt_pct(kpi$eerste_event_zonder_medicatie_pct),
        "Aandeel zonder eerdere medicatie",
        accent = ahti_branding$colors$fris_rood
      )
    )
  })

  output$zorgpad_plot <- renderPlot({
    df <- zorgpad_data()
    shiny::validate(shiny::need(nrow(df) > 0, "Onvoldoende data beschikbaar."))
    show_pct <- isTRUE(input$zorgpad_pct)
    position <- if (show_pct) "fill" else "stack"
    y_lab <- if (show_pct) "Aandeel van eerste gebeurtenissen" else "Aantal personen"
    p <- ggplot(df, aes(x = factor(year), y = waarde, fill = groep_label)) +
      geom_col(position = position) +
      scale_fill_manual(values = c(ahti_branding$colors$fris_rood, ahti_branding$colors$helder_blauw)) +
      labs(title = zorgpad_title(), x = "Jaar", y = y_lab, fill = NULL) +
      theme_minimal() +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 30, hjust = 1))
    if (show_pct) {
      p <- p + scale_y_continuous(labels = scales::percent)
    }
    p
  })

  chart_data_downloads_server(
    id = "zorgpad_downloads",
    data = zorgpad_data,
    chart_type = "stacked_bar",
    category_col = "year",
    series_col = "groep_label",
    value_col = "waarde",
    filename_prefix = "cardio4cities_zorgpad",
    agg_fun = NULL,
    figure_title = zorgpad_title,
    slide_title = zorgpad_title,
    source_output = OUTCOMES_SOURCE_FILE,
    source_sheet = "yearly_total",
    source_mtime = OUTCOMES_SOURCE_MTIME
  )

  # ---------------------------------------------------------------------
  # Naar achtergrond
  # ---------------------------------------------------------------------

  output$achtergrond_metric_ui <- renderUI({
    kind <- c4c_outcome_kind(input$achtergrond_indicator)
    selectInput("achtergrond_metric", "Weergave", choices = c4c_available_metrics(kind))
  })

  achtergrond_title <- reactive({
    shiny::req(input$achtergrond_indicator, input$achtergrond_dimensie)
    dim_label <- C4C_BREAKDOWNS[[input$achtergrond_dimensie]]$label
    paste0(c4c_outcome_label(input$achtergrond_indicator), " naar ", dim_label)
  })

  # All years for the chosen indicator/dimension -- feeds both the year
  # picker (bar mode) and the trend-line chart/download directly.
  achtergrond_base_data <- reactive({
    shiny::req(input$achtergrond_indicator, input$achtergrond_dimensie, input$achtergrond_metric)
    kind <- c4c_outcome_kind(input$achtergrond_indicator)
    outcome_type <- c4c_outcome_type(kind)
    df <- c4c_filter_outcome(OUTCOMES_DATA, input$achtergrond_dimensie, input$achtergrond_indicator, outcome_type)
    df <- c4c_add_metric(df, input$achtergrond_metric)
    df$groep_label <- c4c_relabel_groep(
      c4c_order_groep(df$groep, input$achtergrond_dimensie),
      input$achtergrond_dimensie
    )
    df$reeks <- c4c_outcome_label(input$achtergrond_indicator)
    df
  })

  output$achtergrond_jaar_ui <- renderUI({
    years <- sort(unique(achtergrond_base_data()$year), decreasing = TRUE)
    shiny::validate(shiny::need(length(years) > 0, "Geen jaren beschikbaar voor deze selectie."))
    selectInput("achtergrond_jaar", "Jaar", choices = years, selected = years[1])
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
        "Onvoldoende data beschikbaar voor deze selectie (mogelijk afgeschermd vanwege CBS-geheimhoudingsregels, of nog niet gemeten in deze periode)."
      ))
      n_groups <- length(unique(df$groep_label))
      ggplot(df, aes(x = year, y = waarde, color = groep_label, group = groep_label)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        scale_color_manual(values = c4c_palette(n_groups, ahti_branding$scale_discrete)) +
        labs(title = achtergrond_title(), x = "Jaar", y = y_lab, color = NULL) +
        theme_minimal() +
        theme(legend.position = "bottom")
    } else {
      df <- achtergrond_bar_data()
      shiny::validate(shiny::need(
        nrow(df) > 0,
        "Onvoldoende data beschikbaar voor deze selectie (mogelijk afgeschermd vanwege CBS-geheimhoudingsregels)."
      ))
      ggplot(df, aes(x = groep_label, y = waarde)) +
        geom_col(fill = ahti_branding$colors$helder_blauw) +
        labs(title = achtergrond_title(), x = NULL, y = y_lab) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    }
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
    selectInput("gebied_metric", "Weergave", choices = c4c_available_metrics(kind))
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

  gebied_plot_data <- reactive({
    shiny::req(input$gebied_jaar, input$gebied_metric)
    df <- dplyr::filter(gebied_available_data(), .data$year == as.integer(input$gebied_jaar))
    df <- c4c_add_metric(df, input$gebied_metric)
    df$groep_label <- c4c_relabel_groep(df$groep, input$gebied_niveau)
    df$reeks <- c4c_outcome_label(input$gebied_indicator)
    df
  })

  output$gebied_plot <- renderPlot({
    df <- gebied_plot_data()
    shiny::validate(shiny::need(
      nrow(df) > 0,
      "Onvoldoende data beschikbaar voor deze selectie (mogelijk afgeschermd vanwege CBS-geheimhoudingsregels)."
    ))
    ggplot(df, aes(x = stats::reorder(groep_label, waarde), y = waarde)) +
      geom_col(fill = ahti_branding$colors$helder_blauw) +
      coord_flip() +
      labs(title = gebied_title(), x = NULL, y = c4c_metric_axis_label(input$gebied_metric)) +
      theme_minimal()
  })

  gebied_map_data <- reactive({
    shiny::req(input$gebied_jaar, input$gebied_metric)
    shiny::validate(shiny::need(identical(input$gebied_niveau, "stadsdeel"), "Kaart is alleen beschikbaar voor Stadsdeel."))
    df <- dplyr::filter(gebied_available_data(), .data$year == as.integer(input$gebied_jaar))
    df <- c4c_add_metric(df, input$gebied_metric)
    c4c_geo_join_stadsdeel(GEO_STADSDEEL, df)
  })

  output$gebied_map <- renderPlot({
    df <- gebied_map_data()
    shiny::validate(shiny::need(
      nrow(df) > 0,
      "Onvoldoende data beschikbaar voor deze selectie (mogelijk afgeschermd vanwege CBS-geheimhoudingsregels)."
    ))
    ggplot(df, aes(x = long, y = lat, group = stadsdeel, fill = waarde)) +
      geom_polygon(color = "white", linewidth = 0.3) +
      coord_equal() +
      scale_fill_gradient(
        low = "#FBEAE9", high = ahti_branding$colors$fris_rood,
        name = c4c_metric_axis_label(input$gebied_metric)
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
