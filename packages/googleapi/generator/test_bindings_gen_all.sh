#!/usr/bin/env bash

# ** NOTICE **
#   If it exists, this script deletes the "./_gen_all_test"
#   directory before attempting to download and convert new files.

shopt -s nocaseglob  #ignore case for filename matches
shopt -s nullglob    #if no matches return a null string

SCRIPT_DIR="$(dirname "$0")"
DEST_DIR="$SCRIPT_DIR/_gen_all_test"

TIMESTAMP=$(date +%F@%H%M)      #Example format:  2006-09-15@1228
LOG_FILE="$DEST_DIR/${TIMESTAMP}-$(basename "$0").txt"  #log file with same name as script

rm -r "$DEST_DIR" &> /dev/null
mkdir "$DEST_DIR" &> /dev/null

{
  echo "Run Timestamp:  $TIMESTAMP"
  echo "Using Google API Converter: " $("$SCRIPT_DIR/googleapiconv" --version)
  echo

  # Use option --verbose for more messages
  echo "googleapiconv --all --icon --keepjson --timestamp --output=$DEST_DIR/"
  "$SCRIPT_DIR/googleapiconv" --all --icon --keepjson --timestamp --output="$DEST_DIR/"

  # Check to see if we have any missing generated .pp files
  echo ""
  ((COUNT=0))
  for FILE in "$DEST_DIR/"*.json; do
    JSON_NAME="${FILE##*/}"
    PASCAL_NAME="${JSON_NAME%.*}.pp"
    if [ ! -s "$DEST_DIR/$PASCAL_NAME" ]; then
      echo "** Missing or empty file: JSON file $JSON_NAME did not generate $PASCAL_NAME"
      ((COUNT++))
    fi
  done
  echo "Missing File Count = $COUNT"

} |& tee "$LOG_FILE"  #output both stdout and stderr to logfile and terminal
