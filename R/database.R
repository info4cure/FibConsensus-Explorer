# R/database.R
# FibConsensus Explorer
# Read-only database access for Supabase/PostgreSQL

required_db_env <- function() {
  c(
    "FIBCONSENSUS_DB_HOST",
    "FIBCONSENSUS_DB_NAME",
    "FIBCONSENSUS_DB_USER",
    "FIBCONSENSUS_DB_PASSWORD"
  )
}


validate_db_environment <- function() {
  required <- required_db_env()
  values <- Sys.getenv(required, unset = "")
  missing <- required[!nzchar(values)]

  if (length(missing) > 0) {
    stop(
      paste0(
        "Missing required database environment variables: ",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


safe_sql_identifier <- function(x) {
  if (
    length(x) != 1 ||
    is.na(x) ||
    !grepl("^[A-Za-z_][A-Za-z0-9_]*$", x)
  ) {
    stop(
      sprintf("Invalid SQL identifier: %s", x),
      call. = FALSE
    )
  }

  x
}


create_fibconsensus_pool <- function(config) {
  validate_db_environment()
  safe_sql_identifier(config$schema)

  pool::dbPool(
    drv = RPostgres::Postgres(),
    host = Sys.getenv("FIBCONSENSUS_DB_HOST"),
    port = as.integer(
      Sys.getenv("FIBCONSENSUS_DB_PORT", unset = "5432")
    ),
    dbname = Sys.getenv(
      "FIBCONSENSUS_DB_NAME",
      unset = "postgres"
    ),
    user = Sys.getenv("FIBCONSENSUS_DB_USER"),
    password = Sys.getenv("FIBCONSENSUS_DB_PASSWORD"),
    sslmode = Sys.getenv(
      "FIBCONSENSUS_DB_SSLMODE",
      unset = "require"
    ),
    minSize = 1,
    maxSize = as.integer(
      Sys.getenv("FIBCONSENSUS_DB_POOL_MAX", unset = "5")
    ),
    idleTimeout = 60000,
    validationInterval = 60000
  )
}


fibconsensus_views <- function(schema = "fibconsensus") {
  schema <- safe_sql_identifier(schema)

  if (identical(schema, "fibconsensus")) {
    return(c(
      "v_database_overview",
      "v_paper_summary",
      "v_population_catalog",
      "v_population_marker_profiles",
      "v_gene_evidence",
      "v_population_disease_evidence",
      "v_evidence_linkage",
      "v_evidence_modalities"
    ))
  }

  if (identical(schema, "fibconsensus_v2_2")) {
    return(c(
      "v_database_overview",
      "v_consensus_architecture",
      "v_family_membership",
      "v_relationships",
      "v_atlas_recovery",
      "v_disease_remodeling",
      "v_replicated_disease_remodeling",
      "v_studies",
      "v_published_populations",
      "v_population_marker_profiles"))
  }

  stop(paste0("Unsupported FibConsensus schema: ", schema), call. = FALSE)
}


validate_view_name <- function(view_name, schema) {
  if (
    length(view_name) != 1 ||
    is.na(view_name) ||
    !view_name %in% fibconsensus_views(schema)
  ) {
    stop(
      paste0(
        "Invalid or unauthorized FibConsensus view for schema ",
        schema, ": ", view_name
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


read_fibconsensus_view <- function(
  db_pool,
  config,
  view_name
) {
  schema <- safe_sql_identifier(config$schema)
  validate_view_name(view_name, schema)

  identifier <- DBI::Id(
    schema = schema,
    table = view_name
  )

  pool::dbGetQuery(
    db_pool,
    glue::glue_sql(
      "SELECT * FROM {`identifier`}",
      .con = db_pool
    )
  )
}


read_database_overview <- function(db_pool, config) {
  read_fibconsensus_view(
    db_pool,
    config,
    "v_database_overview"
  )
}


read_paper_summary <- function(db_pool, config) {
  read_fibconsensus_view(
    db_pool,
    config,
    "v_paper_summary"
  )
}


read_population_catalog <- function(db_pool, config) {
  read_fibconsensus_view(
    db_pool,
    config,
    "v_population_catalog"
  )
}


read_population_marker_profiles <- function(db_pool, config) {
  read_fibconsensus_view(
    db_pool,
    config,
    "v_population_marker_profiles"
  )
}


read_gene_evidence <- function(db_pool, config) {
  read_fibconsensus_view(
    db_pool,
    config,
    "v_gene_evidence"
  )
}


read_population_disease_evidence <- function(db_pool, config) {
  read_fibconsensus_view(
    db_pool,
    config,
    "v_population_disease_evidence"
  )
}


read_evidence_linkage <- function(db_pool, config) {
  read_fibconsensus_view(
    db_pool,
    config,
    "v_evidence_linkage"
  )
}


read_evidence_modalities <- function(db_pool, config) {
  read_fibconsensus_view(
    db_pool,
    config,
    "v_evidence_modalities"
  )
}


test_fibconsensus_connection <- function(
  db_pool,
  config
) {
  schema <- safe_sql_identifier(config$schema)

  connection <- pool::dbGetQuery(
    db_pool,
    "
    SELECT
      current_database() AS database_name,
      current_user AS database_user,
      now() AS checked_at
    "
  )

  views <- pool::dbGetQuery(
    db_pool,
    "
    SELECT table_name
    FROM information_schema.views
    WHERE table_schema = $1
    ORDER BY table_name
    ",
    params = list(schema)
  )

  required_views <- fibconsensus_views(schema)
  missing_views <- setdiff(required_views, views$table_name)

  if (length(missing_views) > 0) {
    stop(
      paste0(
        "Connection succeeded, but required views are missing from schema ",
        schema, ": ", paste(missing_views, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  list(
    connection = connection,
    views = views$table_name,
    required_views = required_views,
    schema = schema,
    status = "pass"
  )
}

read_consensus_architecture <- function(db_pool, config) {
  read_fibconsensus_view(db_pool, config, "v_consensus_architecture")
}
read_family_membership <- function(db_pool, config) {
  read_fibconsensus_view(db_pool, config, "v_family_membership")
}

# ==============================================================================
# FibConsensus Shiny v2.3 — presentation/read layer
# ==============================================================================

fc_pick_col <- function(x, candidates) {
  hit <- intersect(candidates, names(x))
  if (!length(hit)) NULL else hit[[1]]
}


fc_blank <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}


fc_add_doi_url <- function(x) {
  if (!"doi" %in% names(x)) {
    x$doi_url <- rep("", nrow(x))
    return(x)
  }

  doi <- fc_blank(x$doi)

  doi <- sub(
    "^https?://(dx\\.)?doi\\.org/",
    "",
    doi,
    ignore.case = TRUE
  )

  doi <- sub(
    "^doi:\\s*",
    "",
    doi,
    ignore.case = TRUE
  )

  x$doi <- doi

  x$doi_url <- ifelse(
    nzchar(doi),
    paste0("https://doi.org/", doi),
    ""
  )

  x
}


read_studies <- function(db_pool, config) {
  x <- read_fibconsensus_view(
    db_pool,
    config,
    "v_studies"
  )

  x <- fc_add_doi_url(x)

  # `study` is the canonical human-readable label in v2.2,
  # e.g. Steel et al. (2025).
  if (!"study" %in% names(x)) {
    x$study <- x$citation_key
  }

  x
}


fc_study_lookup <- function(db_pool, config) {
  s <- read_studies(db_pool, config)

  if (!"citation_key" %in% names(s)) {
    return(NULL)
  }

  keep <- intersect(
    c(
      "citation_key",
      "study",
      "publication_year",
      "title",
      "journal",
      "doi",
      "doi_url",
      "study_context",
      "disease_biological_context",
      "technology"
    ),
    names(s)
  )

  s <- s[, keep, drop = FALSE]
  s <- s[!duplicated(s$citation_key), , drop = FALSE]
  s
}


read_relationships <- function(db_pool, config) {

  x <- read_fibconsensus_view(
    db_pool,
    config,
    "v_relationships"
  )

  studies <- tryCatch(
    fc_study_lookup(db_pool, config),
    error = function(e) NULL
  )

  if (is.null(studies)) {
    return(x)
  }

  # Preserve machine/provenance identifiers explicitly.
  if ("paper_a" %in% names(x)) {
    x$paper_a_key <- x$paper_a

    ia <- match(
      as.character(x$paper_a),
      as.character(studies$citation_key)
    )

    label <- studies$study[ia]
    bad <- is.na(label) | !nzchar(label)
    label[bad] <- x$paper_a[bad]

    x$paper_a <- label
  }

  if ("paper_b" %in% names(x)) {
    x$paper_b_key <- x$paper_b

    ib <- match(
      as.character(x$paper_b),
      as.character(studies$citation_key)
    )

    label <- studies$study[ib]
    bad <- is.na(label) | !nzchar(label)
    label[bad] <- x$paper_b[bad]

    x$paper_b <- label
  }

  x
}


read_atlas_recovery <- function(db_pool, config) {
  read_fibconsensus_view(
    db_pool,
    config,
    "v_atlas_recovery"
  )
}


read_disease_remodeling <- function(db_pool, config) {
  read_fibconsensus_view(
    db_pool,
    config,
    "v_disease_remodeling"
  )
}


read_replicated_disease_remodeling <- function(db_pool, config) {
  read_fibconsensus_view(
    db_pool,
    config,
    "v_replicated_disease_remodeling"
  )
}


read_population_marker_profiles <- function(db_pool, config) {
  x <- read_fibconsensus_view(
    db_pool,
    config,
    "v_population_marker_profiles"
  )

  x <- fc_add_doi_url(x)

  # Standardise human-readable study label.
  if (!"study" %in% names(x)) {
    x$study <- x$citation_key
  }

  x
}


read_published_populations <- function(db_pool, config) {

  x <- read_fibconsensus_view(
    db_pool,
    config,
    "v_published_populations"
  )

  studies <- tryCatch(
    fc_study_lookup(db_pool, config),
    error = function(e) NULL
  )

  if (
    !is.null(studies) &&
    "citation_key" %in% names(x)
  ) {

    idx <- match(
      as.character(x$citation_key),
      as.character(studies$citation_key)
    )

    x$study <- studies$study[idx]
    x$publication_year <- studies$publication_year[idx]
    x$title <- studies$title[idx]
    x$journal <- studies$journal[idx]
    x$doi <- studies$doi[idx]
    x$doi_url <- studies$doi_url[idx]

    if ("study_context" %in% names(studies)) {
      x$study_context <- studies$study_context[idx]
    }

    if ("disease_biological_context" %in% names(studies)) {
      x$disease_biological_context <-
        studies$disease_biological_context[idx]
    }

    if ("technology" %in% names(studies)) {
      x$technology <- studies$technology[idx]
    }
  }

  x
}
