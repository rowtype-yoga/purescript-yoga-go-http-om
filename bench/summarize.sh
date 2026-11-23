#!/usr/bin/env bash
# Prints the median of the three runs for every server, route and connection count in results/.
set -euo pipefail
export LC_ALL=C
RESULTS="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/results"

median() { jq -r "$1" "${files[@]}" | sort -g | sed -n 2p; }

printf "%-8s %-5s %-4s %10s %9s %9s  %s\n" server route c "req/s" "p50 ms" "p99 ms" codes
for name in ps hrjson hrbytes; do
  for route in 42 7; do
    for connections in 50 200; do
      files=("$RESULTS/$name-$route-c$connections"-r*.json)
      rps=$(median .summary.requestsPerSec)
      p50=$(median '.latencyPercentiles.p50*1000')
      p99=$(median '.latencyPercentiles.p99*1000')
      codes=$(jq -c '.statusCodeDistribution|keys' "${files[@]}" | sort -u | tr '\n' ' ')
      printf "%-8s %-5s %-4s %10.0f %9.3f %9.3f  %s\n" "$name" "$route" "$connections" "$rps" "$p50" "$p99" "$codes"
    done
  done
done
