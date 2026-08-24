# R/mod_evidence.R
# FibConsensus Explorer v2.2 — Relationship explorer

mod_evidence_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(class="fc-page-head",
      div(class="fc-section-kicker","CROSS-STUDY ADJUDICATION"),
      h1("Relationship explorer"),
      p(class="fc-lead",
        "Inspect how published fibroblast populations were adjudicated as shared identity, partial biological overlap or hierarchical correspondence.")
    ),

    layout_columns(
      col_widths=c(3,3,3,3),
      value_box("Informative relationships",textOutput(ns("n_total"),inline=TRUE),
                showcase=bsicons::bs_icon("share")),
      value_box("Identity",textOutput(ns("n_identity"),inline=TRUE),
                showcase=bsicons::bs_icon("intersect")),
      value_box("Partial overlap",textOutput(ns("n_partial"),inline=TRUE),
                showcase=bsicons::bs_icon("circle-half")),
      value_box("Hierarchy",textOutput(ns("n_hierarchy"),inline=TRUE),
                showcase=bsicons::bs_icon("diagram-2"))
    ),

    br(),

    layout_sidebar(
      sidebar=sidebar(
        width=320,
        div(class="fc-filter-title","Filter relationships"),
        textInput(ns("search"),"Search",
                  placeholder="Population, paper, rationale, marker…"),
        selectInput(ns("class"),"Relationship class",choices="All classes"),
        selectInput(ns("confidence"),"Confidence",choices="All confidence levels"),
        actionButton(ns("reset"),"Reset filters",
                     class="btn btn-sm btn-outline-secondary w-100")
      ),

      div(class="fc-explorer-stack",
        card(
          class="fc-table-card",
          card_header(
            div(class="d-flex justify-content-between align-items-center",
                span("Adjudicated cross-study relationships"),
                span(class="fc-record-count",textOutput(ns("n_filtered"),inline=TRUE)))
          ),
          DT::DTOutput(ns("table"))
        ),

        card(
          class="fc-detail-card",
          card_header("Selected relationship"),
          uiOutput(ns("detail"))
        )
      )
    )
  )
}

mod_evidence_server <- function(id, config, db_pool) {
  moduleServer(id,function(input,output,session){

    dat <- reactive(read_relationships(db_pool,config))

    pick_col <- function(x,candidates){
      hit <- intersect(candidates,names(x))
      if (!length(hit)) NULL else hit[[1]]
    }

    class_col <- reactive(pick_col(dat(),c("relationship_class","relationship","edge_class")))
    conf_col <- reactive(pick_col(dat(),c("relationship_confidence","confidence")))

    observe({
      x <- dat(); cc <- class_col(); cf <- conf_col()
      if (!is.null(cc)) {
        v <- sort(unique(as.character(x[[cc]]))); v <- v[!is.na(v)&nzchar(v)]
        updateSelectInput(session,"class",choices=c("All classes",v))
      }
      if (!is.null(cf)) {
        v <- sort(unique(as.character(x[[cf]]))); v <- v[!is.na(v)&nzchar(v)]
        updateSelectInput(session,"confidence",choices=c("All confidence levels",v))
      }
    })

    observeEvent(input$reset,{
      updateTextInput(session,"search",value="")
      updateSelectInput(session,"class",selected="All classes")
      updateSelectInput(session,"confidence",selected="All confidence levels")
    })

    filtered <- reactive({
      x <- dat(); cc <- class_col(); cf <- conf_col()
      if (!is.null(cc) && !is.null(input$class) && input$class!="All classes")
        x <- x[as.character(x[[cc]])==input$class,,drop=FALSE]
      if (!is.null(cf) && !is.null(input$confidence) && input$confidence!="All confidence levels")
        x <- x[as.character(x[[cf]])==input$confidence,,drop=FALSE]

      q <- if (is.null(input$search)) "" else trimws(input$search)
      if (nzchar(q) && nrow(x)) {
        txt <- apply(x,1,function(z) paste(z,collapse=" | "))
        x <- x[grepl(q,txt,ignore.case=TRUE),,drop=FALSE]
      }
      x
    })

    count_rel <- function(pattern){
      x <- dat(); cc <- class_col()
      if (is.null(cc)) return(NA_integer_)
      sum(grepl(pattern,as.character(x[[cc]]),ignore.case=TRUE),na.rm=TRUE)
    }

    output$n_total <- renderText(format(nrow(dat()),big.mark=","))
    output$n_identity <- renderText(format(count_rel("^IDENTITY$|EQUIVALENT|PROBABLE"),big.mark=","))
    output$n_partial <- renderText(format(count_rel("PARTIAL"),big.mark=","))
    output$n_hierarchy <- renderText(format(count_rel("BROADER|NARROWER|HIERARCH"),big.mark=","))
    output$n_filtered <- renderText(paste0(format(nrow(filtered()),big.mark=",")," shown"))

    output$table <- DT::renderDT({
      x <- filtered()
      cols <- intersect(c(
        "pair_id","paper_a","name_a","paper_b","name_b",
        "relationship_class","relationship_confidence"
      ),names(x))
      if (length(cols)) x <- x[,cols,drop=FALSE]
      names(x) <- c("Pair","Study A","Population A","Study B","Population B",
                    "Class","Confidence")[seq_len(ncol(x))]
      DT::datatable(
        x,selection="single",rownames=FALSE,
        options=list(pageLength=10,scrollX=TRUE,autoWidth=TRUE,
                     dom="tip",lengthChange=FALSE)
      )
    })

    output$detail <- renderUI({
      idx <- input$table_rows_selected
      x <- filtered()
      if (!length(idx) || idx>nrow(x)) {
        return(div(class="fc-empty-state",
                   bsicons::bs_icon("cursor"),
                   span("Select a relationship to inspect its multidimensional adjudication.")))
      }
      r <- x[idx,,drop=FALSE]

      val <- function(cands){
        c <- pick_col(r,cands)
        if (is.null(c)) return(NULL)
        v <- as.character(r[[c]][1])
        if (is.na(v)||!nzchar(v)) NULL else v
      }

      badge <- function(x, cls="fc-badge") {
        if (is.null(x)) NULL else span(class=cls,x)
      }

      detail_item <- function(label, cands, wide=FALSE){
        v <- val(cands)
        if (is.null(v)) return(NULL)
        div(class=if(wide) "fc-detail-item fc-detail-wide" else "fc-detail-item",
            div(class="fc-detail-label",label),
            div(class="fc-detail-value",v))
      }

      div(
        div(class="fc-relationship-head",
          div(
            div(class="fc-rel-study", val(c("paper_a"))),
            div(class="fc-rel-pop", val(c("name_a","population_a")))
          ),
          div(class="fc-rel-arrow", bsicons::bs_icon("arrow-left-right")),
          div(
            div(class="fc-rel-study", val(c("paper_b"))),
            div(class="fc-rel-pop", val(c("name_b","population_b")))
          )
        ),
        div(class="fc-badge-row",
            badge(val(c("relationship_class","relationship")),"fc-badge fc-badge-primary"),
            badge(val(c("relationship_confidence","confidence")),"fc-badge")),
        div(class="fc-detail-grid mt-3",
          detail_item("Molecular",c("molecular_assessment"),TRUE),
          detail_item("Anatomical / spatial",c("anatomical_spatial_assessment"),TRUE),
          detail_item("Functional / state",c("functional_state_assessment"),TRUE),
          detail_item("Disease context",c("disease_context_assessment"),TRUE),
          detail_item("Granularity",c("granularity_assessment"),TRUE),
          detail_item("Independence",c("independence_interpretation"),TRUE),
          detail_item("Overall rationale",c("overall_rationale"),TRUE)
        )
      )
    })
  })
}
