# FibConsensus Explorer — Docker deployment bundle v1.0

This bundle adds container deployment files to the already validated
`FibConsensus-Explorer` Shiny application. It does **not** replace or modify the
application source.

## Basis of this bundle

The Docker preflight detected one Shiny entry point (`app.R`), no existing Docker
configuration, and the following runtime R dependencies:

- DBI
- DT
- RPostgres
- bsicons
- bslib
- glue
- htmltools
- pool
- shiny

`tools` was also referenced by the application but belongs to base R and is not
installed from CRAN.

The application already obtains PostgreSQL credentials from environment
variables. The password is therefore supplied only at container runtime and is
not baked into the image.

## Install into the Explorer repository

From the extracted bundle:

```bash
python scripts/install_docker_bundle.py \
  --repo "/path/to/FibConsensus-Explorer"
```

Or copy the six deployment files manually.

## Configure runtime credentials

From the Explorer repository:

```bash
cp .env.example .env
```

Edit `.env` locally. Never commit that file.

The preflight explicitly confirmed these variable names:

- `FIBCONSENSUS_DB_HOST`
- `FIBCONSENSUS_DB_PORT`
- `FIBCONSENSUS_DB_USER`
- `FIBCONSENSUS_DB_PASSWORD`
- `FIBCONSENSUS_DB_POOL_MAX`

The application also uses environment-driven DB name and SSL-mode configuration.
The Compose file currently uses the conventional names
`FIBCONSENSUS_DB_NAME` and `FIBCONSENSUS_DB_SSLMODE`; verify those two names
against `R/database.R` before the first production deployment.

## Build

```bash
docker compose build --no-cache
```

## Start

```bash
docker compose up -d
```

## Inspect

```bash
docker compose ps
docker compose logs --tail=100 fibconsensus-explorer
```

## Smoke test

```bash
bash scripts/docker_smoke_test.sh
```

Expected terminal marker:

```text
DOCKER_SMOKE_TEST_PASSED
```

This first smoke test validates build/start/health/HTTP availability. A second,
FibConsensus-specific semantic smoke test should then verify the canonical v2.2
resource counts against the database.

## Stop

```bash
docker compose down
```

## Security

- `.env` is excluded from the Docker build context.
- No database password is copied into the image.
- The Shiny process binds explicitly to `0.0.0.0:3838`.
- Do not publish PostgreSQL credentials in GitHub, Docker Hub, Zenodo, logs, or
  screenshots.
