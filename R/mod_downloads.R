# R/mod_downloads.R
# FibConsensus Explorer v2.2 — Downloads

download_card <- function(download_id, title, description, rows_label) {
  card(
    class = "fc-download-card",
    card_header(title),
    p(description),
    card_footer(
      div(
        class = "d-flex justify-content-between align-items-center",
        tags$span(class = "fc-row-count", rows_label),
        downloadButton(download_id, "Download CSV", class = "btn-outline-primary")
      )
    )
  )
}

mod_downloads_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(class = "fc-section-kicker", "DATA ACCESS"),
    h1("Downloads"),
    p(
      class = "fc-lead",
      paste(
        "Export validated analytical views from the frozen FibConsensus v2.2",
        "Supabase/PostgreSQL release. Downloads are generated directly from",
        "the read-only database views used by this application."
      )
    ),

    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(
        title = "Exportable views",
        value = "7",
        showcase = bsicons::bs_icon("download")
      ),
      value_box(
        title = "Frozen architecture",
        value = "v2.2",
        showcase = bsicons::bs_icon("snow")
      ),
      value_box(
        title = "Database mode",
        value = "Read-only",
        showcase = bsicons::bs_icon("shield-lock")
      )
    ),

    br(),
    h3("Validated v2.2 exports"),

    layout_columns(
      col_widths = c(6, 6),
      download_card(ns("download_database_overview"), "Database overview",
                    "Release-level counts for the frozen v2.2 database.", "8 entities"),
      download_card(ns("download_consensus_architecture"), "Consensus architecture",
                    "The six recurrent FibConsensus identity families.", "6 rows"),
      download_card(ns("download_family_membership"), "Family membership",
                    "Canonical and architecture-support population assignments across the six families.", "37 rows"),
      download_card(ns("download_relationships"), "Cross-study relationships",
                    "Identity, partial-overlap and hierarchical relationships used in reconstruction.", "73 rows"),
      download_card(ns("download_atlas_recovery"), "Atlas recovery",
                    "Independent atlas recovery of the eight frozen architectural entities.", "8 rows"),
      download_card(ns("download_disease_remodeling"), "Disease remodeling",
                    "Disease-associated shifts across all 13 disease contexts.", "104 rows"),
      download_card(ns("download_replicated_remodeling"), "Replicated disease remodeling",
                    "Replication-aware disease-associated shifts restricted to seven multi-dataset contexts.", "56 rows")
    ),

    br(),
    card(
      card_header("Release provenance"),
      p("All exports correspond to FibConsensus v2.2. The legacy database release is preserved separately and is not used as the scientific source of truth for this application.")
    )
  )
}

mod_downloads_server <- function(id, db_pool, config) {
  moduleServer(id, function(input, output, session) {

    csv_download <- function(output_id, filename, reader) {
      output[[output_id]] <- downloadHandler(
        filename = function() filename,
        content = function(file) {
          x <- reader(db_pool, config)
          write.csv(x, file, row.names = FALSE, na = "")
        },
        contentType = "text/csv"
      )
    }

    csv_download("download_database_overview",
                 "FibConsensus_v2_2_database_overview.csv",
                 read_database_overview)
    csv_download("download_consensus_architecture",
                 "FibConsensus_v2_2_consensus_architecture.csv",
                 read_consensus_architecture)
    csv_download("download_family_membership",
                 "FibConsensus_v2_2_family_membership.csv",
                 read_family_membership)
    csv_download("download_relationships",
                 "FibConsensus_v2_2_cross_study_relationships.csv",
                 read_relationships)
    csv_download("download_atlas_recovery",
                 "FibConsensus_v2_2_atlas_recovery.csv",
                 read_atlas_recovery)
    csv_download("download_disease_remodeling",
                 "FibConsensus_v2_2_disease_remodeling.csv",
                 read_disease_remodeling)
    csv_download("download_replicated_remodeling",
                 "FibConsensus_v2_2_replicated_disease_remodeling.csv",
                 read_replicated_disease_remodeling)
  })
}
