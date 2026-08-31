# FibConsensus Explorer — Docker deployment v1.0
# Runtime application files are copied from the repository root.
# No credentials are embedded in this image.

ARG R_VERSION=4.4.3
FROM rocker/r-ver:${R_VERSION}

LABEL org.opencontainers.image.title="FibConsensus Explorer"
LABEL org.opencontainers.image.description="Read-only Shiny Explorer for the FibConsensus evidence resource"
LABEL org.opencontainers.image.version="1.0.0"

ENV DEBIAN_FRONTEND=noninteractive \
    RENV_CONFIG_AUTOLOADER_ENABLED=FALSE \
    SHINY_HOST=0.0.0.0 \
    SHINY_PORT=3838

# System libraries required by RPostgres and common Shiny/web packages.
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        libcurl4-openssl-dev \
        libpq-dev \
        libssl-dev \
        libxml2-dev \
        libuv1-dev \
        zlib1g-dev \
        pkg-config \
        make \
        g++ \
    && rm -rf /var/lib/apt/lists/*

# Install runtime packages detected by the Docker preflight audit.
# Warnings are promoted to errors so Docker cannot silently accept a failed CRAN install.
# Every required namespace is verified before the image can proceed.
RUN Rscript -e "options(repos=c(CRAN='https://cloud.r-project.org'), warn=2); pkgs <- c('DBI','DT','RPostgres','bsicons','bslib','glue','htmltools','pool','shiny'); install.packages(pkgs, dependencies=NA, Ncpus=max(1L, parallel::detectCores(logical=FALSE))); missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly=TRUE)]; if (length(missing)) stop('Missing R packages after installation: ', paste(missing, collapse=', ')); cat('R_RUNTIME_DEPENDENCIES_VERIFIED\n')"

WORKDIR /opt/fibconsensus

# Copy the application from the FibConsensus-Explorer repository root.
COPY . /opt/fibconsensus/

# Fail the build early if the expected Shiny entrypoint is absent.
RUN test -f /opt/fibconsensus/app.R

EXPOSE 3838

HEALTHCHECK --interval=30s --timeout=5s --start-period=30s --retries=5 \
  CMD curl -fsS http://127.0.0.1:3838/ >/dev/null || exit 1

# Explicit all-interface binding is required for host/container access.
CMD ["R", "-q", "-e", "shiny::runApp('/opt/fibconsensus', host='0.0.0.0', port=3838, launch.browser=FALSE)"]
