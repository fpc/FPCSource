#!/usr/bin/env bash

# ** NOTICE **
#   If they exist, this script deletes the "./_google_api_bindings_tmp"
#   and "./_google_api_icons_tmp" directories before attempting
#   to convert new files. It does not download any JSON, but works on
#   files which must already be present in "./_google_api_json_tmp"
#   (use "./fetch_google_json.sh" to download the JSON files)

shopt -s nocaseglob  #ignore case for filename matches
shopt -s nullglob    #if no matches return a null string

SCRIPT_DIR="$(dirname "$0")"
JSON_DIR="$SCRIPT_DIR/_google_api_json_tmp"
ICONS_DIR="$SCRIPT_DIR/_google_api_icons_tmp"
BINDINGS_DIR="$SCRIPT_DIR/_google_api_bindings_tmp"

TIMESTAMP=$(date +%F@%H%M)      #Example format:  2006-09-15@1228
LOG_FILE="$BINDINGS_DIR/${TIMESTAMP}-$(basename "$0").txt"  #log file with same name as script

if [ ! -d "$JSON_DIR" ]; then
  echo "** Nothing to do:  $JSON_DIR directory does not exist **"
  echo '  1) First run "fetch_google_json.sh" to download JSON files'
  echo '  2) Then it is recommended to run "nomalize_json_files.sh"'
  echo '     (if you have "jq" installed on your system)'
  exit 1
fi

rm -r "$BINDINGS_DIR" &> /dev/null
mkdir "$BINDINGS_DIR" &> /dev/null
rm -r "$ICONS_DIR" &> /dev/null
mkdir "$ICONS_DIR" &> /dev/null

{
  echo "Run Timestamp:  $TIMESTAMP"
  echo "Using Google API Converter: " $("$SCRIPT_DIR/googleapiconv" --version)
  echo

#  Test convert a single JSON file
#  echo "googleapiconv --verbose --input=$JSON_DIR/googlegmail.json --output=$BINDINGS_DIR/googlegmail.pp"
#  "$SCRIPT_DIR/googleapiconv" --verbose --input="$JSON_DIR/googlegmail.json" --output="$BINDINGS_DIR/googlegmail.pp"

  for FILE in "$JSON_DIR/"*.json; do
    OUT_NAME="${FILE##*/}"; OUT_NAME="${OUT_NAME%.*}.pp"
    #--verbose
    echo "googleapiconv --icon --timestamp --input=$FILE --output=$BINDINGS_DIR/$OUT_NAME"
    "$SCRIPT_DIR/googleapiconv" --icon --input="$FILE" --output="$BINDINGS_DIR/$OUT_NAME"
  done

  mv "$BINDINGS_DIR/"*.png "$ICONS_DIR" &> /dev/null
  mv "$BINDINGS_DIR/"*.gif "$ICONS_DIR" &> /dev/null

  # Check to see if we have any missing generated .pp files
  echo ""
  ((COUNT=0))
  for FILE in "$JSON_DIR/"*.json; do
    JSON_NAME="${FILE##*/}"
    PASCAL_NAME="${JSON_NAME%.*}.pp"
    if [ ! -s "$BINDINGS_DIR/$PASCAL_NAME" ]; then
      echo "** Missing or empty file: JSON file $JSON_NAME did not generate $PASCAL_NAME"
      ((COUNT++))
    fi
  done
  echo "Missing File Count = $COUNT"

} |& tee "$LOG_FILE"  #output both stdout and stderr to logfile and terminal
