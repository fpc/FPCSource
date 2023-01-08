#!/usr/bin/env bash

# ** NOTICE **
#   This script deletes the "./_google_api_json_tmp" directory
#   (if it exists) before attempting to download new definitions.

SCRIPT_DIR="$(dirname "$0")"
JSON_DIR="$SCRIPT_DIR/_google_api_json_tmp"
TIMESTAMP=$(date +%F@%H%M)      #format: 2006-09-15@1228
LOG_FILE="$JSON_DIR/${TIMESTAMP}-$(basename "$0").txt"  #log file with same name as script

rm -r "$JSON_DIR" &> /dev/null
mkdir "$JSON_DIR" &> /dev/null

{
  echo "Run Timestamp:  $TIMESTAMP"
  echo "Using Google API Converter: " $("$SCRIPT_DIR/googleapiconv" --version)
  echo

  echo "# googleapiconv --verbose --all --onlydownload --keepjson --output=$JSON_DIR/"
  echo
  "$SCRIPT_DIR/googleapiconv" --verbose --all --onlydownload --keepjson --output="$JSON_DIR/"

} |& tee "$LOG_FILE"  #output both stdout and stderr to logfile and terminal
