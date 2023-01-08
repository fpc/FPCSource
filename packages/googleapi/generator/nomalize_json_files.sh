#!/usr/bin/env bash

# This script sorts the JSON files to make it easy to diff
# which also makes the generated units to be easy to diff. :-)
#
# ** NOTICE **
# This script requires jq to be installed.  jq is
# included in most distros package repos.
# https://github.com/stedolan/jq/

shopt -s nocaseglob  #ignore case for filename matches
shopt -s nullglob    #if no matches return a null string

SCRIPT_DIR="$(dirname "$0")"
JSON_DIR="$SCRIPT_DIR/_google_api_json_tmp"
TIMESTAMP=$(date +%F@%H%M)      #format: 2006-09-15@1228
LOG_FILE="$JSON_DIR/${TIMESTAMP}-$(basename "$0").txt"  #log file with same name as script

{

  TMP_FILE=$(mktemp --tmpdir="$JSON_DIR")
  ((COUNT=0))
  for FILE in "$JSON_DIR/"*.json; do
    echo "# running jq --sortkeys on: $FILE"
    jq --sort-keys '.' "$FILE" > "$TMP_FILE"
    mv -f "$TMP_FILE" "$FILE"
    ((COUNT++))
  done
  echo ""
  echo "Processed File Count = $COUNT"

} |& tee "$LOG_FILE"  #output both stdout and stderr to logfile and terminal
