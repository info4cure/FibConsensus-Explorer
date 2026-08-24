# R/mod_populations.R
# FibConsensus Explorer v2.3 — Published population explorer

mod_populations_ui <- function(id) {

  ns <- NS(id)

  tagList(

    div(
      class = "fc-page-head",

      div(
        class = "fc-section-kicker",
        "PUBLISHED TAXONOMIES"
      ),

      h1("Published population explorer"),

      p(
        class = "fc-lead",
        paste(
          "Explore author-reported fibroblast populations together with",
          "their marker genes, biological context and source publication."
        )
      )
    ),

    layout_columns(
      col_widths = c(3, 3, 3, 3),

      value_box(
        "Published populations",
        textOutput(ns("n_pop"), inline = TRUE),
        showcase = bsicons::bs_icon("diagram-3")
      ),

      value_box(
        "Studies",
        textOutput(ns("n_studies"), inline = TRUE),
        showcase = bsicons::bs_icon("journal-text")
      ),

      value_box(
        "Marker records",
        textOutput(ns("n_marker_records"), inline = TRUE),
        showcase = bsicons::bs_icon("activity")
      ),

      value_box(
        "Marker genes",
        textOutput(ns("n_genes"), inline = TRUE),
        showcase = bsicons::bs_icon("activity")
      )
    ),

    br(),

    layout_sidebar(

      sidebar = sidebar(

        width = 300,

        div(
          class = "fc-filter-title",
          "Filter populations"
        ),

        textInput(
          ns("search"),
          "Search",
          placeholder = "Population, marker, author, disease, DOI…"
        ),

        selectInput(
          ns("study"),
          "Study",
          choices = "All studies"
        ),

        selectInput(
          ns("year"),
          "Publication year",
          choices = "All years"
        ),

        selectInput(
          ns("disease"),
          "Disease / context",
          choices = "All contexts"
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
          class = "fc-table-card",

          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",

              span("Author-reported populations"),

              span(
                class = "fc-record-count",
                textOutput(
                  ns("n_filtered"),
                  inline = TRUE
                )
              )
            )
          ),

          DT::DTOutput(ns("table"))
        ),

        card(
          class = "fc-detail-card",
          card_header("Selected population"),
          uiOutput(ns("detail"))
        )
      )
    )
  )
}


mod_populations_server <- function(id, config, db_pool) {

  moduleServer(id, function(input, output, session) {

    pick_col <- function(x, candidates) {
      hit <- intersect(candidates, names(x))
      if (!length(hit)) NULL else hit[[1]]
    }

    value1 <- function(x, candidates, default = "") {
      c <- pick_col(x, candidates)

      if (is.null(c)) {
        return(default)
      }

      z <- as.character(x[[c]][1])

      if (is.na(z) || !nzchar(trimws(z))) {
        return(default)
      }

      z
    }

    pops_raw <- reactive({
      read_published_populations(
        db_pool,
        config
      )
    })

    markers_raw <- reactive({
      read_population_marker_profiles(
        db_pool,
        config
      )
    })

    population_markers <- reactive({

      m <- markers_raw()

      if (
        !nrow(m) ||
        !"population_stable_key" %in% names(m) ||
        !"gene_symbol" %in% names(m)
      ) {
        return(data.frame())
      }

      m$gene_symbol <- trimws(as.character(m$gene_symbol))

      m <- m[
        !is.na(m$gene_symbol) &
        nzchar(m$gene_symbol),
        ,
        drop = FALSE
      ]

      groups <- split(
        m,
        as.character(m$population_stable_key)
      )

      rows <- lapply(
        names(groups),
        function(k) {

          z <- groups[[k]]

          genes <- sort(
            unique(z$gene_symbol)
          )

          positive <- character(0)
          negative <- character(0)

          if ("direction" %in% names(z)) {

            direction <- tolower(
              as.character(z$direction)
            )

            negative <- unique(
              z$gene_symbol[
                grepl(
                  "negative|downreg|decreas|excluded|absent|low",
                  direction
                )
              ]
            )

            positive <- setdiff(
              genes,
              negative
            )

          } else {

            positive <- genes
          }

          data.frame(
            population_stable_key = k,
            n_marker_genes = length(genes),
            reported_markers = paste(
              genes,
              collapse = ", "
            ),
            positive_markers = paste(
              sort(positive),
              collapse = ", "
            ),
            negative_markers = paste(
              sort(negative),
              collapse = ", "
            ),
            stringsAsFactors = FALSE
          )
        }
      )

      do.call(rbind, rows)
    })

    dat <- reactive({

      x <- pops_raw()
      m <- population_markers()

      x$reported_markers <- ""
      x$positive_markers <- ""
      x$negative_markers <- ""
      x$n_marker_genes <- 0L

      if (
        nrow(m) &&
        "population_stable_key" %in% names(x)
      ) {

        idx <- match(
          x$population_stable_key,
          m$population_stable_key
        )

        hit <- !is.na(idx)

        x$reported_markers[hit] <-
          m$reported_markers[idx[hit]]

        x$positive_markers[hit] <-
          m$positive_markers[idx[hit]]

        x$negative_markers[hit] <-
          m$negative_markers[idx[hit]]

        x$n_marker_genes[hit] <-
          m$n_marker_genes[idx[hit]]
      }

      x
    })

    observe({

      x <- dat()

      if ("study" %in% names(x)) {

        z <- sort(
          unique(as.character(x$study))
        )

        z <- z[
          !is.na(z) &
          nzchar(z)
        ]

        updateSelectInput(
          session,
          "study",
          choices = c("All studies", z)
        )
      }

      if ("publication_year" %in% names(x)) {

        z <- sort(
          unique(as.character(x$publication_year))
        )

        z <- z[
          !is.na(z) &
          nzchar(z)
        ]

        updateSelectInput(
          session,
          "year",
          choices = c("All years", z)
        )
      }

      dc <- pick_col(
        x,
        c(
          "disease_context",
          "disease_state",
          "disease_biological_context",
          "study_context"
        )
      )

      if (!is.null(dc)) {

        z <- sort(
          unique(as.character(x[[dc]]))
        )

        z <- z[
          !is.na(z) &
          nzchar(z)
        ]

        updateSelectInput(
          session,
          "disease",
          choices = c("All contexts", z)
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
          "year",
          selected = "All years"
        )

        updateSelectInput(
          session,
          "disease",
          selected = "All contexts"
        )
      }
    )

    filtered <- reactive({

      x <- dat()

      if (
        "study" %in% names(x) &&
        !is.null(input$study) &&
        input$study != "All studies"
      ) {

        x <- x[
          as.character(x$study) == input$study,
          ,
          drop = FALSE
        ]
      }

      if (
        "publication_year" %in% names(x) &&
        !is.null(input$year) &&
        input$year != "All years"
      ) {

        x <- x[
          as.character(x$publication_year) == input$year,
          ,
          drop = FALSE
        ]
      }

      dc <- pick_col(
        x,
        c(
          "disease_context",
          "disease_state",
          "disease_biological_context",
          "study_context"
        )
      )

      if (
        !is.null(dc) &&
        !is.null(input$disease) &&
        input$disease != "All contexts"
      ) {

        x <- x[
          as.character(x[[dc]]) == input$disease,
          ,
          drop = FALSE
        ]
      }

      q <- if (is.null(input$search)) {
        ""
      } else {
        trimws(input$search)
      }

      if (nzchar(q) && nrow(x)) {

        cols <- intersect(
          c(
            "study",
            "publication_year",
            "population_name",
            "original_label",
            "cluster_label",
            "biological_state",
            "spatial_location",
            "anatomical_context",
            "anatomical_site",
            "disease_state",
            "disease_biological_context",
            "study_context",
            "reported_markers",
            "positive_markers",
            "negative_markers",
            "title",
            "journal",
            "doi",
            "our_interpretation"
          ),
          names(x)
        )

        txt <- apply(
          x[, cols, drop = FALSE],
          1,
          function(z) {
            paste(z, collapse = " | ")
          }
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

    output$n_pop <- renderText({
      format(
        nrow(dat()),
        big.mark = ","
      )
    })

    output$n_studies <- renderText({

      x <- dat()

      if (!"study" %in% names(x)) {
        return("—")
      }

      format(
        length(
          unique(
            x$study[
              !is.na(x$study) &
              nzchar(x$study)
            ]
          )
        ),
        big.mark = ","
      )
    })

    output$n_marker_records <- renderText({
      format(
        nrow(markers_raw()),
        big.mark = ","
      )
    })

    output$n_genes <- renderText({

      m <- markers_raw()

      if (!"gene_symbol" %in% names(m)) {
        return("—")
      }

      format(
        length(
          unique(
            m$gene_symbol[
              !is.na(m$gene_symbol) &
              nzchar(m$gene_symbol)
            ]
          )
        ),
        big.mark = ","
      )
    })

    output$n_filtered <- renderText({
      paste0(
        format(
          nrow(filtered()),
          big.mark = ","
        ),
        " shown"
      )
    })

    output$table <- DT::renderDT({

      x <- filtered()

      context_col <- pick_col(
        x,
        c(
          "disease_context",
          "disease_state",
          "disease_biological_context",
          "study_context"
        )
      )

      location_col <- pick_col(
        x,
        c(
          "spatial_location",
          "anatomical_context",
          "anatomical_site"
        )
      )

      show <- data.frame(

        Study = if ("study" %in% names(x)) {
          x$study
        } else {
          ""
        },

        Population = if ("original_label" %in% names(x)) {
          x$original_label
        } else {
          x$population_name
        },

        Context = if (!is.null(context_col)) {
          x[[context_col]]
        } else {
          ""
        },

        Location = if (!is.null(location_col)) {
          x[[location_col]]
        } else {
          ""
        },

        `Reported markers` = x$reported_markers,

        Confidence = if ("confidence" %in% names(x)) {
          x$confidence
        } else {
          ""
        },

        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      DT::datatable(
        show,
        selection = "single",
        rownames = FALSE,
        escape = TRUE,

        options = list(
          pageLength = 12,
          scrollX = TRUE,
          autoWidth = FALSE,
          dom = "tip",
          lengthChange = FALSE,

          columnDefs = list(
            list(
              width = "155px",
              targets = 0
            ),
            list(
              width = "230px",
              targets = 1
            ),
            list(
              width = "180px",
              targets = 2
            ),
            list(
              width = "170px",
              targets = 3
            ),
            list(
              width = "360px",
              targets = 4
            )
          )
        )
      )
    })

    output$detail <- renderUI({

      idx <- input$table_rows_selected
      x <- filtered()

      if (
        !length(idx) ||
        idx > nrow(x)
      ) {

        return(
          div(
            class = "fc-empty-state",

            bsicons::bs_icon("cursor"),

            span(
              paste(
                "Select a population to inspect its reported markers,",
                "FibConsensus interpretation and source publication."
              )
            )
          )
        )
      }

      r <- x[idx, , drop = FALSE]

      population_label <- value1(
        r,
        c(
          "original_label",
          "population_name"
        )
      )

      study <- value1(
        r,
        c("study")
      )

      make_chips <- function(text) {

        if (!nzchar(text)) {
          return(
            div(
              class = "fc-muted",
              "No structured marker evidence available for this population."
            )
          )
        }

        genes <- trimws(
          strsplit(
            text,
            ",",
            fixed = TRUE
          )[[1]]
        )

        genes <- genes[nzchar(genes)]

        div(
          class = "fc-marker-cloud",

          lapply(
            genes,
            function(g) {
              span(
                class = "fc-marker-chip",
                g
              )
            }
          )
        )
      }

      doi <- value1(r, c("doi"))
      doi_url <- value1(r, c("doi_url"))

      div(
        class = "fc-population-profile",

        div(
          class = "fc-population-hero",

          div(
            class = "fc-section-kicker",
            if (nzchar(study)) {
              study
            } else {
              "SOURCE STUDY"
            }
          ),

          h2(population_label),

          div(
            class = "fc-badge-row",

            if (
              nzchar(
                value1(
                  r,
                  c("biological_state")
                )
              )
            ) {
              span(
                class = "fc-soft-badge",
                value1(
                  r,
                  c("biological_state")
                )
              )
            },

            if (
              nzchar(
                value1(
                  r,
                  c(
                    "spatial_location",
                    "anatomical_context",
                    "anatomical_site"
                  )
                )
              )
            ) {
              span(
                class = "fc-soft-badge",
                value1(
                  r,
                  c(
                    "spatial_location",
                    "anatomical_context",
                    "anatomical_site"
                  )
                )
              )
            },

            if (
              nzchar(
                value1(
                  r,
                  c("confidence")
                )
              )
            ) {
              span(
                class = "fc-soft-badge",
                paste0(
                  "Confidence: ",
                  value1(
                    r,
                    c("confidence")
                  )
                )
              )
            }
          )
        ),

        div(
          class = "fc-science-section",

          div(
            class = "fc-science-title",
            "Reported markers"
          ),

          make_chips(
            value1(
              r,
              c("positive_markers")
            )
          )
        ),

        if (
          nzchar(
            value1(
              r,
              c("negative_markers")
            )
          )
        ) {
          div(
            class = "fc-science-section",

            div(
              class = "fc-science-title",
              "Negative / exclusion markers"
            ),

            make_chips(
              value1(
                r,
                c("negative_markers")
              )
            )
          )
        },

        if (
          nzchar(
            value1(
              r,
              c(
                "our_interpretation",
                "description"
              )
            )
          )
        ) {
          div(
            class = "fc-science-section",

            div(
              class = "fc-science-title",
              "FibConsensus interpretation"
            ),

            div(
              class = "fc-science-text",

              value1(
                r,
                c(
                  "our_interpretation",
                  "description"
                )
              )
            )
          )
        },

        div(
          class = "fc-science-section fc-publication-block",

          div(
            class = "fc-science-title",
            "Source publication"
          ),

          div(
            class = "fc-publication-title",

            value1(
              r,
              c("title"),
              "Publication title unavailable"
            )
          ),

          if (nzchar(study)) {
            div(
              class = "fc-publication-meta",
              study
            )
          },

          if (
            nzchar(
              value1(
                r,
                c("journal")
              )
            )
          ) {
            div(
              class = "fc-publication-meta",
              value1(
                r,
                c("journal")
              )
            )
          },

          if (nzchar(doi_url)) {
            tags$a(
              href = doi_url,
              target = "_blank",
              rel = "noopener noreferrer",
              class = "fc-doi-link",
              paste0(
                "DOI: ",
                doi,
                " ↗"
              )
            )
          }
        )
      )
    })
  })
}
