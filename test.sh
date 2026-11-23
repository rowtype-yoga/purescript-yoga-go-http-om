#!/usr/bin/env bash
# Compiles the specs to Go with Iris and runs them.
#
#   ./test.sh
#
# Environment: IRIS (the iris binary), IRIS_GO (a local iris-go checkout to build against).
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IRIS="${IRIS:-iris}"
OUT="$HERE/out"
mkdir -p "$OUT"
{
  echo "module example.com/yoga-go-http-om-test"
  echo
  echo "go 1.27.0"
  echo
  echo "require github.com/i-am-the-slime/iris-go v0.3.0"
  if [ -n "${IRIS_GO:-}" ]; then
    echo
    echo "replace github.com/i-am-the-slime/iris-go => $IRIS_GO"
  fi
} >"$OUT/go.mod"
(cd "$HERE" && "$IRIS" go -p yoga-go-http-om --entry Test.Main.main "$OUT")
(cd "$OUT" && go mod tidy && go build -buildvcs=false -o spec-runner .)
"$OUT/spec-runner"
(cd "$OUT" && go build -buildvcs=false -race -o spec-race .)
"$OUT/spec-race"
