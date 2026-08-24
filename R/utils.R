module_placeholder <- function(title, description, next_step) {
  tagList(
    div(class = "section-intro", h2(title), p(description)),
    card(
      class = "placeholder-card",
      card_header("Module scaffold ready"),
      p(paste(
        "The UI and server module are loaded correctly.",
        "No database connection is required at this stage."
      )),
      tags$strong("Next implementation step"),
      p(next_step)
    )
  )
}
