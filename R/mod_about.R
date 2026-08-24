mod_about_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("content"))
}


mod_about_server <- function(id, config) {
  moduleServer(id, function(input, output, session) {

    output$content <- renderUI({
      tagList(
        tags$style(HTML("
          .about-page {
            padding-bottom: 2rem;
          }


          .about-hero {
            background:
              linear-gradient(
                135deg,
                rgba(47, 93, 80, .08),
                rgba(117, 80, 142, .08)
              );
            border: 1px solid #D7E4DE;
            border-radius: 17px;
            margin-bottom: 1.25rem;
            padding: 1.6rem 1.7rem;
          }

          .about-eyebrow {
            color: #6A4B7E;
            font-size: .82rem;
            font-weight: 750;
            letter-spacing: .09em;
            margin-bottom: .35rem;
            text-transform: uppercase;
          }

          .about-hero h1 {
            color: #244A40;
            font-size: 2.15rem;
            font-weight: 650;
            margin-bottom: .55rem;
          }

          .about-lead {
            color: #425B51;
            font-size: 1.04rem;
            line-height: 1.65;
            margin: 0;
            max-width: 1000px;
          }

          .about-grid {
            display: grid;
            gap: 1rem;
            grid-template-columns: repeat(2, minmax(0, 1fr));
            margin-bottom: 1rem;
          }

          .about-card {
            background: #FFFFFF;
            border: 1px solid #D8E5DF;
            border-radius: 14px;
            box-shadow: 0 8px 24px rgba(44, 78, 67, .045);
            overflow: hidden;
          }

          .about-card-header {
            border-top: 5px solid #75508E;
            color: #294F44;
            font-size: 1.08rem;
            font-weight: 700;
            padding: 1rem 1.15rem .75rem;
          }

          .about-card-body {
            color: #50665D;
            font-size: .94rem;
            line-height: 1.6;
            padding: .15rem 1.15rem 1.15rem;
          }

          .about-card-body p:last-child {
            margin-bottom: 0;
          }

          .about-card-body ul {
            margin-bottom: 0;
            padding-left: 1.2rem;
          }

          .about-card-body li {
            margin-bottom: .4rem;
          }

          .about-full {
            grid-column: 1 / -1;
          }

          .about-principles {
            display: grid;
            gap: .75rem;
            grid-template-columns: repeat(4, minmax(0, 1fr));
          }

          .about-principle {
            background: #F8F4FA;
            border: 1px solid #E2D6E9;
            border-radius: 12px;
            padding: .9rem;
          }

          .about-principle-title {
            color: #66427C;
            font-size: .91rem;
            font-weight: 700;
            margin-bottom: .3rem;
          }

          .about-principle-copy {
            color: #576A62;
            font-size: .84rem;
            line-height: 1.45;
          }

          .about-detail-grid {
            display: grid;
            gap: .15rem .85rem;
            grid-template-columns: minmax(120px, 165px) 1fr;
            margin: 0;
          }

          .about-detail-grid dt {
            color: #31594C;
            font-weight: 700;
            margin: 0;
            padding: .38rem 0;
          }

          .about-detail-grid dd {
            border-bottom: 1px solid #E7EEEA;
            color: #50635B;
            margin: 0;
            overflow-wrap: anywhere;
            padding: .38rem 0;
          }

          .about-citation {
            background: #F5F8F6;
            border-left: 4px solid #70B493;
            border-radius: 0 10px 10px 0;
            color: #40584E;
            font-size: .91rem;
            line-height: 1.55;
            padding: .9rem 1rem;
          }

          .about-disclaimer {
            background: #FBF8F2;
            border: 1px solid #E9DDC4;
            border-radius: 12px;
            color: #695D46;
            font-size: .87rem;
            line-height: 1.55;
            padding: .9rem 1rem;
          }

          @media (max-width: 950px) {
            .about-grid,
            .about-principles {
              grid-template-columns: 1fr;
            }

            .about-full {
              grid-column: auto;
            }
          }
        ")),

        div(
          class = "about-page",

          div(
            class = "about-hero",
            div(class = "about-eyebrow", "About the resource"),
            h1("FibConsensus"),
            p(
              class = "about-lead",
              paste(
                "FibConsensus is an evidence-based reference framework",
                "for the annotation of human skin fibroblasts. It",
                "reconstructs published fibroblast nomenclature, retains",
                "the terminology used by original investigators and",
                "connects populations, genes, disease contexts and",
                "supporting evidence in a traceable relational resource."
              )
            )
          ),

          div(
            class = "about-grid",

            div(
              class = "about-card",
              div(class = "about-card-header", "Purpose"),
              div(
                class = "about-card-body",
                p(
                  paste(
                    "The resource is designed to support transparent",
                    "interpretation and cross-study comparison rather than",
                    "to impose another closed fibroblast taxonomy."
                  )
                ),
                tags$ul(
                  tags$li(
                    "Preserve original population labels and study context."
                  ),
                  tags$li(
                    "Connect reported populations to reproducible marker evidence."
                  ),
                  tags$li(
                    "Distinguish author interpretation from curated interpretation."
                  ),
                  tags$li(
                    "Provide stable identifiers and versioned analytical outputs."
                  )
                )
              )
            ),

            div(
              class = "about-card",
              div(class = "about-card-header", "Scope"),
              div(
                class = "about-card-body",
                p(
                  paste(
                    "FibConsensus focuses on human skin fibroblast",
                    "populations characterised using single-cell,",
                    "single-nucleus or spatial transcriptomic approaches,",
                    "together with relevant experimental and computational",
                    "validation evidence."
                  )
                ),
                p(
                  paste(
                    "Population identity, anatomical localisation,",
                    "functional interpretation and disease-associated state",
                    "are represented as related but non-equivalent annotation",
                    "dimensions."
                  )
                )
              )
            ),

            div(
              class = "about-card about-full",
              div(
                class = "about-card-header",
                "Guiding principles"
              ),
              div(
                class = "about-card-body",
                div(
                  class = "about-principles",

                  div(
                    class = "about-principle",
                    div(
                      class = "about-principle-title",
                      "Traceability"
                    ),
                    div(
                      class = "about-principle-copy",
                      paste(
                        "Curated assertions remain connected to their",
                        "publication and evidence record."
                      )
                    )
                  ),

                  div(
                    class = "about-principle",
                    div(
                      class = "about-principle-title",
                      "Separation of layers"
                    ),
                    div(
                      class = "about-principle-copy",
                      paste(
                        "Original nomenclature, author interpretation and",
                        "consensus interpretation are retained separately."
                      )
                    )
                  ),

                  div(
                    class = "about-principle",
                    div(
                      class = "about-principle-title",
                      "Hierarchical annotation"
                    ),
                    div(
                      class = "about-principle-copy",
                      paste(
                        "Identity, anatomical niche, function and state",
                        "are not collapsed into a single label."
                      )
                    )
                  ),

                  div(
                    class = "about-principle",
                    div(
                      class = "about-principle-title",
                      "Versioned reproducibility"
                    ),
                    div(
                      class = "about-principle-copy",
                      paste(
                        "Database releases, SQL views and downloadable",
                        "outputs are explicitly versioned."
                      )
                    )
                  )
                )
              )
            ),

            div(
              class = "about-card",
              div(
                class = "about-card-header",
                "How the framework was built"
              ),
              div(
                class = "about-card-body",
                tags$ol(
                  tags$li(
                    paste(
                      "Systematic identification of eligible human skin",
                      "single-cell and spatial studies."
                    )
                  ),
                  tags$li(
                    paste(
                      "Structured extraction of populations, markers,",
                      "disease contexts and supporting evidence."
                    )
                  ),
                  tags$li(
                    paste(
                      "Evidence-aware mapping of original populations to",
                      "broader consensus families."
                    )
                  ),
                  tags$li(
                    paste(
                      "Relational modelling with stable keys and explicit",
                      "many-to-many evidence links."
                    )
                  ),
                  tags$li(
                    paste(
                      "Independent evaluation against harmonised",
                      "cross-disease single-cell data."
                    )
                  )
                )
              )
            ),

            div(
              class = "about-card",
              div(
                class = "about-card-header",
                "Evidence model"
              ),
              div(
                class = "about-card-body",
                p(
                  paste(
                    "Evidence records are classified into interpretable",
                    "modalities, including marker evidence, experimental",
                    "validation, spatial evidence, computational inference,",
                    "quantitative or compositional evidence, annotation",
                    "evidence and contextual synthesis."
                  )
                ),
                p(
                  paste(
                    "Confidence reflects the strength of the curated record",
                    "within its source context; it does not constitute a",
                    "formal clinical-grade evidence rating."
                  )
                )
              )
            ),

            div(
              class = "about-card",
              div(
                class = "about-card-header",
                "Current release"
              ),
              div(
                class = "about-card-body",
                tags$dl(
                  class = "about-detail-grid",
                  tags$dt("Application"),
                  tags$dd(paste0("v", config$app_version)),
                  tags$dt("Database"),
                  tags$dd(
                    paste0("v", config$database_release_version)
                  ),
                  tags$dt("Database schema"),
                  tags$dd(config$schema),
                  tags$dt("Resource"),
                  tags$dd("FibConsensus Explorer"),
                  tags$dt("Domain"),
                  tags$dd("Human skin fibroblast annotation"),
                  tags$dt("Export formats"),
                  tags$dd("CSV and ZIP release bundle")
                )
              )
            ),

            div(
              class = "about-card",
              div(
                class = "about-card-header",
                "Citation"
              ),
              div(
                class = "about-card-body",
                div(
                  class = "about-citation",
                  paste(
                    "When using this resource, cite the accompanying",
                    "FibConsensus manuscript and report both the application",
                    "version and database release shown on this page.",
                    "The final bibliographic citation will be added following",
                    "publication."
                  )
                )
              )
            ),

            div(
              class = "about-card about-full",
              div(
                class = "about-card-header",
                "Provenance, interpretation and reuse"
              ),
              div(
                class = "about-card-body",
                p(
                  paste(
                    "FibConsensus is a curated research resource.",
                    "Original publications remain the authoritative source",
                    "for study-specific methods, results and interpretation.",
                    "Database transformations are designed to improve",
                    "comparability without erasing source terminology or",
                    "uncertainty."
                  )
                ),
                div(
                  class = "about-disclaimer",
                  tags$strong("Research-use disclaimer: "),
                  paste(
                    "This application is not a diagnostic device and should",
                    "not be used as the sole basis for clinical decisions.",
                    "Users should independently verify relevant assertions",
                    "against the cited primary literature."
                  )
                )
              )
            )
          )
        )
      )
    })
  })
}
