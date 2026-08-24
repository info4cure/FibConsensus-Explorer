# R/mod_overview.R
# FibConsensus Explorer v2.2 — Overview

mod_overview_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(class = "fc-section-kicker", "FIBCONSENSUS v2.2"),
    h1("Resolving cell identity across human skin fibroblast single-cell studies"),
    p(
      class = "fc-lead",
      paste(
        "FibConsensus reconstructs recurrent fibroblast identity from",
        "multidimensional published evidence while preserving distinctions",
        "among identity, hierarchical substructure, partial correspondence",
        "and context-dependent state."
      )
    ),

    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        title = "Systematic-review studies",
        value = textOutput(ns("n_studies"), inline = TRUE),
        showcase = bsicons::bs_icon("journal-text")
      ),
      value_box(
        title = "Published populations",
        value = textOutput(ns("n_populations"), inline = TRUE),
        showcase = bsicons::bs_icon("diagram-3")
      ),
      value_box(
        title = "Cross-study relationships",
        value = textOutput(ns("n_relationships"), inline = TRUE),
        showcase = bsicons::bs_icon("share")
      ),
      value_box(
        title = "Consensus families",
        value = textOutput(ns("n_families"), inline = TRUE),
        showcase = bsicons::bs_icon("layers")
      )
    ),

    br(),

    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        card_header("Evidence reconstruction"),
        p("51 studies and 334 author-reported fibroblast populations were integrated with explicit attention to dataset reuse and evidence independence."),
        div(class = "fc-stat-line", strong("73"), " informative cross-study relationships"),
        div(class = "fc-stat-line", strong("29"), " identity relationships"),
        div(class = "fc-stat-line", strong("16"), " identity components")
      ),
      card(
        card_header("Frozen architecture"),
        p("The v2.2 architecture resolves six recurrent fibroblast families while retaining meaningful substructure without forcing all recurrent patterns into additional identities."),
        div(class = "fc-stat-line", strong("6"), " recurrent identity families"),
        div(class = "fc-stat-line", strong("1"), " retained within-family subtype"),
        div(class = "fc-stat-line", strong("1"), " cross-family state axis")
      ),
      card(
        card_header("Independent atlas evaluation"),
        p("The frozen architecture was projected onto an independently constructed cross-disease fibroblast atlas."),
        div(class = "fc-stat-line", strong("166,620"), " fibroblasts"),
        div(class = "fc-stat-line", strong("21"), " datasets"),
        div(class = "fc-stat-line", strong("19"), " L2 fibroblast clusters"),
        div(class = "fc-stat-line", strong("13"), " disease contexts")
      )
    ),

    br(),

    card(
      card_header("FibConsensus v2.2 architecture"),
      uiOutput(ns("architecture_summary"))
    ),

    br(),

    card(
      card_header("Interpretation"),
      p("FibConsensus is not a synonym dictionary. It is an evidence- and provenance-aware framework for deciding whether populations reported across studies represent recurrent identity, hierarchical structure, partial biological correspondence, or context-dependent state.")
    )
  )
}

mod_overview_server <- function(id, db_pool, config) {
  moduleServer(id, function(input, output, session) {

    overview <- reactive({
      read_database_overview(db_pool, config)
    })

    architecture <- reactive({
      read_consensus_architecture(db_pool, config)
    })

    get_n <- function(entity) {
      x <- overview()
      y <- x$n_rows[x$entity == entity]
      if (!length(y)) return(NA_integer_)
      as.integer(y[[1]])
    }

    output$n_studies <- renderText(format(get_n("studies"), big.mark = ","))
    output$n_populations <- renderText(format(get_n("populations"), big.mark = ","))
    output$n_relationships <- renderText(format(get_n("relationships"), big.mark = ","))
    output$n_families <- renderText(format(get_n("consensus_families"), big.mark = ","))

    output$architecture_summary <- renderUI({
      a <- architecture()

      id_col <- intersect(
        c("consensus_family_id", "family_id", "entity_id"),
        names(a)
      )
      name_col <- intersect(
        c("consensus_family_name", "family_name", "entity_name"),
        names(a)
      )

      if (!length(id_col) || !length(name_col)) {
        return(tags$div(
          class = "alert alert-warning",
          "Architecture table loaded, but display columns could not be resolved."
        ))
      }

      id_col <- id_col[[1]]
      name_col <- name_col[[1]]

      tags$div(
        class = "fc-family-grid",
        lapply(seq_len(nrow(a)), function(i) {
          tags$div(
            class = "fc-family-card",
            tags$div(class = "fc-family-id", as.character(a[[id_col]][i])),
            tags$div(class = "fc-family-name", as.character(a[[name_col]][i]))
          )
        })
      )
    })
  })
}
