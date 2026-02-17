#!/bin/bash
# CI gate: fail if any Rust source file exceeds the line limit.
# This prevents new monolithic files from being introduced.

MAX_LINES=${1:-800}

violations=$(find dupes-core dupes-rust cargo-dupes code-dupes -name '*.rs' -exec wc -l {} + | \
  awk -v max="$MAX_LINES" '$1 > max && $0 !~ /total/' | sort -rn)

if [ -n "$violations" ]; then
  echo "Files exceeding $MAX_LINES lines:"
  echo "$violations"
  exit 1
fi

echo "All Rust files are within the $MAX_LINES line limit."
