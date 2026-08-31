#!/usr/bin/env bash
set -euo pipefail

SERVICE="${1:-fibconsensus-explorer}"
HOST_PORT="${FIBCONSENSUS_HTTP_PORT:-3838}"
BASE_URL="${FIBCONSENSUS_BASE_URL:-http://127.0.0.1:${HOST_PORT}}"

echo "=============================================================="
echo "FibConsensus Explorer — Docker smoke test v1.0"
echo "=============================================================="
echo "Service : ${SERVICE}"
echo "URL     : ${BASE_URL}"
echo

echo "[1/4] Compose service state"
docker compose ps "${SERVICE}"

echo
echo "[2/4] Container health"
CID="$(docker compose ps -q "${SERVICE}")"
if [[ -z "${CID}" ]]; then
  echo "FAIL: no container id returned for ${SERVICE}" >&2
  exit 2
fi

HEALTH="$(docker inspect --format='{{if .State.Health}}{{.State.Health.Status}}{{else}}no-healthcheck{{end}}' "${CID}")"
echo "Health: ${HEALTH}"
if [[ "${HEALTH}" != "healthy" ]]; then
  echo "Container is not healthy."
  echo "--- recent logs ---"
  docker compose logs --tail=100 "${SERVICE}" || true
  exit 3
fi

echo
echo "[3/4] HTTP reachability"
curl -fsS "${BASE_URL}/" >/tmp/fibconsensus_explorer_smoke.html
BYTES="$(wc -c </tmp/fibconsensus_explorer_smoke.html | tr -d ' ')"
echo "HTTP response bytes: ${BYTES}"
if [[ "${BYTES}" -lt 100 ]]; then
  echo "FAIL: unexpectedly small HTTP response" >&2
  exit 4
fi

echo
echo "[4/4] Shiny application marker"
if grep -Eqi 'FibConsensus|shiny' /tmp/fibconsensus_explorer_smoke.html; then
  echo "Application marker detected."
else
  echo "WARN: page is reachable but expected FibConsensus/Shiny marker was not found."
fi

echo
echo "DOCKER_SMOKE_TEST_PASSED"
