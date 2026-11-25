#!/usr/bin/env bash
# Builds the example and two hand-written net/http servers, then hammers all three with oha.
#
#   bench/bench.sh && bench/summarize.sh
#
# The example listens on 8080, the hand-written ones on 18081 and 18082, so keep those free.
# Environment: IRIS (the iris binary).
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IRIS="${IRIS:-iris}"
RESULTS="$HERE/results"
BIN="$HERE/bin"
OUT="$HERE/out"
mkdir -p "$RESULTS" "$BIN" "$OUT"

printf 'module example.com/yoga-go-http-om-example\n\ngo 1.27.0\n\nrequire github.com/i-am-the-slime/iris-go v0.6.0\n' >"$OUT/go.mod"
(cd "$HERE/.." && "$IRIS" go -p yoga-go-http-om-example --entry Main.main "$OUT")
(cd "$OUT" && go mod tidy && go build -buildvcs=false -o "$BIN/ps" .)
(cd "$HERE" && go build -buildvcs=false -o "$BIN/hrjson" ./json && go build -buildvcs=false -o "$BIN/hrbytes" ./bytes)

pids=()
trap 'kill "${pids[@]}" 2>/dev/null || true' EXIT
for server in ps hrjson hrbytes; do
  "$BIN/$server" >/dev/null &
  pids+=($!)
done
sleep 1

for server in ps:8080 hrjson:18081 hrbytes:18082; do
  name=${server%%:*}
  port=${server##*:}
  for route in 42 7; do
    for connections in 50 200; do
      url="http://127.0.0.1:$port/users/$route"
      oha -z 3s -c "$connections" --no-tui --output-format quiet "$url" >/dev/null
      for run in 1 2 3; do
        oha -z 10s -c "$connections" --no-tui --output-format json "$url" >"$RESULTS/$name-$route-c$connections-r$run.json"
      done
    done
  done
done
