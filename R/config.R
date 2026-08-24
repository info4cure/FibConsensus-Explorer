load_app_config <- function() {
  list(
    app_name = Sys.getenv(
      "FIBCONSENSUS_APP_NAME",
      unset = "FibConsensus Explorer"
    ),
    app_version = Sys.getenv(
      "FIBCONSENSUS_APP_VERSION",
      unset = "0.2.0-preview"
    ),
    database_release_version = Sys.getenv(
      "FIBCONSENSUS_DATABASE_RELEASE_VERSION",
      unset = "2.2"
    ),
    schema = Sys.getenv(
      "FIBCONSENSUS_DB_SCHEMA",
      unset = "fibconsensus_v2_2"
    ),
    data_mode = Sys.getenv(
      "FIBCONSENSUS_DATA_MODE",
      unset = "database"
    )
  )
}
