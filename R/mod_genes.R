# R/mod_genes.R
# FibConsensus Explorer v2.3
# Cross-study marker evidence explorer

mod_genes_ui <- function(id) {

  ns <- NS(id)

  tagList(

    div(
      class = "fc-page-head fc-page-head-markers",

      div(
        class = "fc-section-kicker",
        "SYSTEMATIC EVIDENCE"
      ),

      h1("Marker evidence explorer"),

      p(
        class = "fc-lead",
        paste(
          "Explore curated fibroblast marker genes across",
          "author-reported populations, studies and biological contexts."
        )
      )
    ),

    layout_columns(
      col_widths = c(3, 3, 3, 3),

      value_box(
        title = "Marker genes",
        value = textOutput(ns("n_genes"), inline = TRUE),
        showcase = bsicons::bs_icon("activity"),
        class = "fc-value-box fc-value-blue"
      ),

      value_box(
        title = "Marker records",
        value = textOutput(ns("n_records"), inline = TRUE),
        showcase = bsicons::bs_icon("list-check"),
        class = "fc-value-box fc-value-orange"
      ),

      value_box(
        title = "Populations",
        value = textOutput(ns("n_populations"), inline = TRUE),
        showcase = bsicons::bs_icon("diagram-3"),
        class = "fc-value-box fc-value-teal"
      ),

      value_box(
        title = "Studies",
        value = textOutput(ns("n_studies"), inline = TRUE),
        showcase = bsicons::bs_icon("journal-text"),
        class = "fc-value-box fc-value-magenta"
      )
    ),

    br(),

    layout_sidebar(

      sidebar = sidebar(

        width = 295,

        div(
          class = "fc-filter-title",
          "Filter marker evidence"
        ),

        textInput(
          ns("search"),
          "Search",
          placeholder = "Gene, population, study, disease…"
        ),

        selectInput(
          ns("study"),
          "Study",
          choices = "All studies"
        ),

        selectInput(
          ns("role"),
          "Marker role",
          choices = "All marker roles"
        ),

        selectInput(
          ns("direction"),
          "Direction",
          choices = "All directions"
        ),

        selectInput(
          ns("confidence"),
          "Confidence",
          choices = "All confidence levels"
        ),

        actionButton(
          ns("reset"),
          "Reset filters",
          class = "btn btn-sm btn-outline-secondary w-100"
        )
      ),

      div(
        class = "fc-explorer-stack",

        card(
          class = "fc-table-card fc-gene-table-card",

          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",

              span("Curated marker genes"),

              span(
                class = "fc-record-count",
                textOutput(
                  ns("n_filtered"),
                  inline = TRUE
                )
              )
            )
          ),

          DT::DTOutput(ns("genes"))
        ),

        card(
          class = "fc-detail-card fc-gene-detail-card",

          card_header("Selected marker gene"),

          uiOutput(ns("detail"))
        )
      )
    )
  )
}


mod_genes_server <- function(id, config, db_pool) {

  moduleServer(id, function(input, output, session) {

    # -------------------------------------------------------------------------
    # Helpers
    # -------------------------------------------------------------------------

    clean_chr <- function(x) {

      if (is.null(x)) {
        return(character(0))
      }

      x <- as.character(x)
      x[is.na(x)] <- ""
      trimws(x)
    }


    valid_chr <- function(x) {

      x <- clean_chr(x)
      x[nzchar(x)]
    }


    has_text <- function(x) {

      if (length(x) == 0) {
        return(FALSE)
      }

      x <- clean_chr(x)

      length(x) > 0 &&
        !is.na(x[[1]]) &&
        nzchar(x[[1]])
    }


    first_text <- function(x, candidates, default = "") {

      hit <- intersect(
        candidates,
        names(x)
      )

      if (!length(hit)) {
        return(default)
      }

      z <- clean_chr(
        x[[hit[[1]]]]
      )

      if (
        !length(z) ||
        is.na(z[[1]]) ||
        !nzchar(z[[1]])
      ) {
        return(default)
      }

      z[[1]]
    }


    unique_count <- function(x) {

      x <- valid_chr(x)

      length(unique(x))
    }


    collapse_unique <- function(x, n = Inf) {

      x <- sort(
        unique(
          valid_chr(x)
        )
      )

      if (!length(x)) {
        return("")
      }

      if (is.finite(n) && length(x) > n) {
        return(
          paste0(
            paste(
              head(x, n),
              collapse = "; "
            ),
            "; …"
          )
        )
      }

      paste(
        x,
        collapse = "; "
      )
    }


    # -------------------------------------------------------------------------
    # Authoritative marker evidence view
    # -------------------------------------------------------------------------

    evidence <- reactive({

      x <- read_population_marker_profiles(
        db_pool,
        config
      )

      required <- c(
        "gene_symbol",
        "population_stable_key",
        "citation_key"
      )

      missing <- setdiff(
        required,
        names(x)
      )

      if (length(missing)) {
        stop(
          paste0(
            "Marker evidence view is missing required fields: ",
            paste(missing, collapse = ", ")
          )
        )
      }

      # Explicit normalization eliminates NA-related if() failures.
      char_cols <- intersect(
        c(
          "gene_symbol",
          "population_stable_key",
          "citation_key",
          "study",
          "original_label",
          "population_name",
          "marker_role",
          "direction",
          "confidence",
          "biological_state",
          "disease_state",
          "spatial_location",
          "anatomical_context",
          "anatomical_site",
          "title",
          "journal",
          "doi",
          "doi_url"
        ),
        names(x)
      )

      for (cc in char_cols) {
        x[[cc]] <- clean_chr(x[[cc]])
      }

      x
    })


    # -------------------------------------------------------------------------
    # Filter choices
    # -------------------------------------------------------------------------

    observe({

      x <- evidence()

      if ("study" %in% names(x)) {

        vals <- sort(
          unique(
            valid_chr(x$study)
          )
        )

        updateSelectInput(
          session,
          "study",
          choices = c(
            "All studies",
            vals
          )
        )
      }

      if ("marker_role" %in% names(x)) {

        vals <- sort(
          unique(
            valid_chr(x$marker_role)
          )
        )

        updateSelectInput(
          session,
          "role",
          choices = c(
            "All marker roles",
            vals
          )
        )
      }

      if ("direction" %in% names(x)) {

        vals <- sort(
          unique(
            valid_chr(x$direction)
          )
        )

        updateSelectInput(
          session,
          "direction",
          choices = c(
            "All directions",
            vals
          )
        )
      }

      if ("confidence" %in% names(x)) {

        vals <- sort(
          unique(
            valid_chr(x$confidence)
          )
        )

        updateSelectInput(
          session,
          "confidence",
          choices = c(
            "All confidence levels",
            vals
          )
        )
      }
    })


    observeEvent(
      input$reset,
      {

        updateTextInput(
          session,
          "search",
          value = ""
        )

        updateSelectInput(
          session,
          "study",
          selected = "All studies"
        )

        updateSelectInput(
          session,
          "role",
          selected = "All marker roles"
        )

        updateSelectInput(
          session,
          "direction",
          selected = "All directions"
        )

        updateSelectInput(
          session,
          "confidence",
          selected = "All confidence levels"
        )
      }
    )


    # -------------------------------------------------------------------------
    # Filtered record-level evidence
    # -------------------------------------------------------------------------

    records <- reactive({

      x <- evidence()

      if (
        "study" %in% names(x) &&
        !is.null(input$study) &&
        !is.na(input$study) &&
        input$study != "All studies"
      ) {

        x <- x[
          x$study == input$study,
          ,
          drop = FALSE
        ]
      }

      if (
        "marker_role" %in% names(x) &&
        !is.null(input$role) &&
        !is.na(input$role) &&
        input$role != "All marker roles"
      ) {

        x <- x[
          x$marker_role == input$role,
          ,
          drop = FALSE
        ]
      }

      if (
        "direction" %in% names(x) &&
        !is.null(input$direction) &&
        !is.na(input$direction) &&
        input$direction != "All directions"
      ) {

        x <- x[
          x$direction == input$direction,
          ,
          drop = FALSE
        ]
      }

      if (
        "confidence" %in% names(x) &&
        !is.null(input$confidence) &&
        !is.na(input$confidence) &&
        input$confidence != "All confidence levels"
      ) {

        x <- x[
          x$confidence == input$confidence,
          ,
          drop = FALSE
        ]
      }

      q <- if (
        is.null(input$search) ||
        is.na(input$search)
      ) {
        ""
      } else {
        trimws(input$search)
      }

      if (nzchar(q) && nrow(x)) {

        search_cols <- intersect(
          c(
            "gene_symbol",
            "study",
            "original_label",
            "population_name",
            "marker_role",
            "direction",
            "confidence",
            "biological_state",
            "disease_state",
            "spatial_location",
            "anatomical_context",
            "anatomical_site",
            "title",
            "journal",
            "doi"
          ),
          names(x)
        )

        search_data <- x[
          ,
          search_cols,
          drop = FALSE
        ]

        for (cc in names(search_data)) {
          search_data[[cc]] <- clean_chr(
            search_data[[cc]]
          )
        }

        txt <- apply(
          search_data,
          1,
          paste,
          collapse = " | "
        )

        x <- x[
          grepl(
            q,
            txt,
            ignore.case = TRUE
          ),
          ,
          drop = FALSE
        ]
      }

      x
    })


    # -------------------------------------------------------------------------
    # Correct gene-level aggregation
    # -------------------------------------------------------------------------

    gene_summary <- reactive({

      x <- records()

      if (!nrow(x)) {
        return(
          data.frame(
            Gene = character(),
            Records = integer(),
            Populations = integer(),
            Studies = integer(),
            Contexts = character(),
            Confidence = character(),
            stringsAsFactors = FALSE
          )
        )
      }

      x$gene_symbol <- clean_chr(
        x$gene_symbol
      )

      x <- x[
        nzchar(x$gene_symbol),
        ,
        drop = FALSE
      ]

      if (!nrow(x)) {
        return(
          data.frame()
        )
      }

      split_gene <- split(
        x,
        x$gene_symbol
      )

      rows <- lapply(
        names(split_gene),
        function(g) {

          z <- split_gene[[g]]

          population_n <- unique_count(
            z$population_stable_key
          )

          study_n <- if ("study" %in% names(z)) {
            unique_count(z$study)
          } else {
            unique_count(z$citation_key)
          }

          context_values <- character(0)

          for (
            cc in c(
              "disease_state",
              "biological_state",
              "spatial_location",
              "anatomical_context",
              "anatomical_site"
            )
          ) {

            if (cc %in% names(z)) {
              context_values <- c(
                context_values,
                valid_chr(z[[cc]])
              )
            }
          }

          context_values <- unique(
            context_values
          )

          confidence <- ""

          if ("confidence" %in% names(z)) {

            conf <- tolower(
              valid_chr(z$confidence)
            )

            if (any(conf == "high")) {
              confidence <- "High"
            } else if (
              any(
                conf %in% c(
                  "moderate",
                  "medium"
                )
              )
            ) {
              confidence <- "Moderate"
            } else if (length(conf)) {
              confidence <- tools::toTitleCase(
                conf[[1]]
              )
            }
          }

          data.frame(
            Gene = g,
            Records = nrow(z),
            Populations = population_n,
            Studies = study_n,
            Contexts = if (length(context_values)) {
              length(context_values)
            } else {
              0L
            },
            Confidence = confidence,
            stringsAsFactors = FALSE
          )
        }
      )

      out <- do.call(
        rbind,
        rows
      )

      out <- out[
        order(
          -out$Studies,
          -out$Populations,
          -out$Records,
          out$Gene
        ),
        ,
        drop = FALSE
      ]

      rownames(out) <- NULL

      out
    })


    # -------------------------------------------------------------------------
    # Top metrics
    # -------------------------------------------------------------------------

    output$n_genes <- renderText({

      x <- evidence()

      format(
        unique_count(x$gene_symbol),
        big.mark = ","
      )
    })


    output$n_records <- renderText({

      format(
        nrow(evidence()),
        big.mark = ","
      )
    })


    output$n_populations <- renderText({

      x <- evidence()

      format(
        unique_count(
          x$population_stable_key
        ),
        big.mark = ","
      )
    })


    output$n_studies <- renderText({

      x <- evidence()

      n <- if ("study" %in% names(x)) {
        unique_count(x$study)
      } else {
        unique_count(x$citation_key)
      }

      format(
        n,
        big.mark = ","
      )
    })


    output$n_filtered <- renderText({

      paste0(
        format(
          nrow(gene_summary()),
          big.mark = ","
        ),
        " genes shown"
      )
    })


    # -------------------------------------------------------------------------
    # Gene table
    # -------------------------------------------------------------------------

    output$genes <- DT::renderDT({

      x <- gene_summary()

      if (!nrow(x)) {
        return(
          DT::datatable(
            data.frame(
              Message = "No marker genes match the current filters."
            ),
            rownames = FALSE,
            options = list(
              dom = "t"
            )
          )
        )
      }

      # Confidence is rendered as Nature-style badge.
      x$Confidence <- vapply(
        x$Confidence,
        function(v) {

          v <- clean_chr(v)

          if (
            !length(v) ||
            !nzchar(v[[1]])
          ) {
            return("")
          }

          cls <- if (
            tolower(v[[1]]) == "high"
          ) {
            "fc-confidence-high"
          } else if (
            tolower(v[[1]]) %in% c(
              "moderate",
              "medium"
            )
          ) {
            "fc-confidence-moderate"
          } else {
            "fc-confidence-low"
          }

          sprintf(
            '<span class="fc-confidence-pill %s">%s</span>',
            cls,
            htmltools::htmlEscape(v[[1]])
          )
        },
        character(1)
      )

      DT::datatable(
        x,
        selection = "single",
        rownames = FALSE,
        escape = FALSE,

        options = list(
          pageLength = 20,
          dom = "tip",
          lengthChange = FALSE,
          autoWidth = FALSE,
          order = list(
            list(3, "desc"),
            list(2, "desc"),
            list(1, "desc")
          ),

          columnDefs = list(
            list(
              width = "135px",
              targets = 0
            ),
            list(
              className = "dt-center",
              width = "88px",
              targets = c(1, 2, 3, 4)
            ),
            list(
              className = "dt-center",
              width = "115px",
              targets = 5
            )
          )
        )
      )
    })


    # -------------------------------------------------------------------------
    # Selected gene evidence
    # -------------------------------------------------------------------------

    output$detail <- renderUI({

      idx <- input$genes_rows_selected
      gs <- gene_summary()

      if (
        !length(idx) ||
        is.na(idx[[1]]) ||
        idx[[1]] < 1 ||
        idx[[1]] > nrow(gs)
      ) {

        return(
          div(
            class = "fc-empty-state",

            bsicons::bs_icon("cursor"),

            span(
              paste(
                "Select a marker gene to inspect the populations",
                "and studies supporting its evidence profile."
              )
            )
          )
        )
      }

      gene <- gs$Gene[[idx[[1]]]]

      x <- records()

      q <- x[
        clean_chr(x$gene_symbol) == gene,
        ,
        drop = FALSE
      ]

      if (!nrow(q)) {
        return(
          div(
            class = "fc-empty-state",
            "No evidence records available for the selected gene."
          )
        )
      }

      study_n <- if ("study" %in% names(q)) {
        unique_count(q$study)
      } else {
        unique_count(q$citation_key)
      }

      pop_n <- unique_count(
        q$population_stable_key
      )

      study_labels <- if ("study" %in% names(q)) {
        q$study
      } else {
        q$citation_key
      }

      evidence_rows <- lapply(
        seq_len(nrow(q)),
        function(i) {

          r <- q[
            i,
            ,
            drop = FALSE
          ]

          study <- first_text(
            r,
            c(
              "study",
              "citation_key"
            ),
            "Study unavailable"
          )

          pop <- first_text(
            r,
            c(
              "original_label",
              "population_name",
              "population_stable_key"
            ),
            "Population unavailable"
          )

          role <- first_text(
            r,
            c("marker_role")
          )

          direction <- first_text(
            r,
            c("direction")
          )

          context <- first_text(
            r,
            c(
              "disease_state",
              "biological_state",
              "spatial_location",
              "anatomical_context",
              "anatomical_site"
            )
          )

          doi_url <- first_text(
            r,
            c("doi_url")
          )

          div(
            class = "fc-gene-evidence-row",

            div(
              class = "fc-gene-evidence-main",

              div(
                class = "fc-gene-evidence-study",
                study
              ),

              div(
                class = "fc-gene-evidence-pop",
                pop
              )
            ),

            div(
              class = "fc-gene-evidence-metadata",

              if (nzchar(role)) {
                span(
                  class = "fc-evidence-badge fc-evidence-role",
                  role
                )
              },

              if (nzchar(direction)) {
                span(
                  class = "fc-evidence-badge fc-evidence-direction",
                  direction
                )
              },

              if (nzchar(context)) {
                span(
                  class = "fc-evidence-badge fc-evidence-context",
                  context
                )
              }
            ),

            if (nzchar(doi_url)) {
              tags$a(
                href = doi_url,
                target = "_blank",
                rel = "noopener noreferrer",
                class = "fc-mini-doi",
                "DOI ↗"
              )
            }
          )
        }
      )

      div(
        class = "fc-gene-profile",

        div(
          class = "fc-gene-hero",

          div(
            class = "fc-gene-symbol-block",

            div(
              class = "fc-section-kicker",
              "MARKER GENE"
            ),

            h2(gene)
          ),

          div(
            class = "fc-gene-metrics",

            div(
              class = "fc-gene-metric",

              strong(
                format(
                  nrow(q),
                  big.mark = ","
                )
              ),

              span("records")
            ),

            div(
              class = "fc-gene-metric",

              strong(
                format(
                  pop_n,
                  big.mark = ","
                )
              ),

              span("populations")
            ),

            div(
              class = "fc-gene-metric",

              strong(
                format(
                  study_n,
                  big.mark = ","
                )
              ),

              span("studies")
            )
          )
        ),

        div(
          class = "fc-science-section",

          div(
            class = "fc-science-title",
            "Cross-study evidence"
          ),

          div(
            class = "fc-gene-evidence-list",
            evidence_rows
          )
        )
      )
    })
  })
}
