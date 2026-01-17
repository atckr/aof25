#!/bin/bash

# Embed all of the inputs into one text file to allow embedding it into the
# OCaml input parser binaries

for file in *.txt *.in ../examples_for_testing/inputs/*.txt; do
  fname=$(basename "$file")
  # macOS base64 doesn't have -w flag, use different syntax
  if [[ "$OSTYPE" == "darwin"* ]]; then
    b64=$(base64 < "$file" | tr -d '\n')
  else
    b64=$(base64 -w 0 "$file")
  fi
  echo "$fname $b64"
done
