library(shiny)
library(bslib)

source("R/config.R")
source("R/database.R")
source("R/utils.R")
source("R/mod_overview.R")
source("R/mod_populations.R")
source("R/mod_genes.R")
source("R/mod_evidence.R")
source("R/mod_downloads.R")
source("R/mod_about.R")

app_config <- load_app_config()

db_pool <- create_fibconsensus_pool(app_config)

onStop(function() {
  pool::poolClose(db_pool)
})

app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2F5D50",
  secondary = "#6B7D73",
  success = "#5D8A72",
  info = "#789F8B",
  bg = "#F7FAF8",
  fg = "#23352E",
  base_font = font_google("Inter"),
  heading_font = font_google("Source Sans 3")
)

brand_ui <- div(
  class = "app-brand",
  
  tags$img(
    src = "favicon.png?v=3",
    alt = "FibConsensus logo",
    class = "app-brand-logo"
  ),
  
  div(
    class = "app-brand-copy",
    
    span(
      class = "app-brand-title",
      
      span(
        class = "brand-fib",
        "Fib"
      ),
      
      span(
        class = "brand-consensus",
        "Consensus"
      )
    )
  )
)

ui <- page_navbar(
  title = brand_ui,
  id = "main_navigation",
  theme = app_theme,
  fillable = FALSE,
  
  header = tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    ),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "population_styles.css"
    ),
    tags$link(
      rel = "icon",
      type = "image/png",
      href = "favicon.png?v=1"
    ),
    tags$link(
      rel = "apple-touch-icon",
      href = "favicon.png?v=1"
    ),
    tags$meta(
      name = "description",
      content = paste(
        "FibConsensus Explorer: evidence-based annotation",
        "of human skin fibroblasts."
      )
    ),
    tags$meta(
      name = "theme-color",
      content = "#2F5D50"
    )
  ),
  
  nav_panel(
    "Overview",
    mod_overview_ui("overview")
  ),
  nav_panel(
    "Published populations",
    mod_populations_ui("populations")
  ),
  nav_panel(
    "Evidence base",
    mod_genes_ui("genes")
  ),
  nav_panel(
    "Relationships",
    mod_evidence_ui("evidence")
  ),
  nav_panel(
    "Downloads",
    mod_downloads_ui("downloads")
  ),
  nav_panel(
    "About",
    mod_about_ui("about")
  ),
  
  footer = div(
    class = "app-footer",
    div(
      class = "app-footer-inner",
      span(
        class = "footer-brand",
        "FibConsensus Explorer"
      ),
      span(
        class = "footer-separator",
        "·"
      ),
      span(
        paste0(
          "App v",
          app_config$app_version
        )
      ),
      span(
        class = "footer-separator",
        "·"
      ),
      span(
        paste0(
          "Database release v",
          app_config$database_release_version
        )
      )
    )
  )
)

server <- function(input, output, session) {
  mod_overview_server(
    id = "overview",
    config = app_config,
    db_pool = db_pool
  )
  
  mod_populations_server(
    id = "populations",
    config = app_config,
    db_pool = db_pool
  )
  
  mod_genes_server(
    id = "genes",
    config = app_config,
    db_pool = db_pool
)

mod_evidence_server(
    id = "evidence",
    config = app_config,
    db_pool = db_pool
)
  mod_downloads_server(
    id = "downloads",
    config = app_config,
    db_pool = db_pool
  )
  mod_about_server(
    id = "about",
    config = app_config
  )
}

shinyApp(ui, server)
