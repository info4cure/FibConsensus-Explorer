#!/usr/bin/env Rscript
# Optional explicit health probe for FibConsensus Explorer.
# Dockerfile/Compose use curl for a lightweight HTTP probe.

url <- Sys.getenv("FIBCONSENSUS_HEALTH_URL", "http://127.0.0.1:3838/")
status <- tryCatch({
  con <- url(url, open = "rb")
  on.exit(close(con), add = TRUE)
  readBin(con, what = "raw", n = 1L)
  TRUE
}, error = function(e) FALSE)

quit(status = if (isTRUE(status)) 0L else 1L)
