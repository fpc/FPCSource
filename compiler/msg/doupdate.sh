#!/bin/bash
#
# Files to correct. DO NOT include errore.msg, the original file.
#
MSGFILES="errorct errord errorfi errorhe errorid errorn errorpl" 
MSGFILES="$MSGFILES errorptu errorru errorues errorda errordu errores"
MSGFILES="$MSGFILES errorf errorheu erroriu errorpli errorpt errorr"
MSGFILES="$MSGFILES errortr"
#
# Msgdif tool
#
MSGDIFF=../utils/msgdif
#
# Go 
#
for msg in $MSGFILES
do
  MSGFILE="${msg}.msg"
  ORGFILE="${msg}.org"
  ERRFILE="${msg}.err"
  OUTPUTFILE="${msg}-err.msg"
  echo "Handling $MSGFILE"
  $MSGDIFF errore.msg "$MSGFILE" > ${msg}.out 
  ERR=$?
  if [ "$ERR" = "0" ]; then
    grep -a -A 2 '^Error' ${msg}.out  > "$ERRFILE"
    ERRCOUNT=$(grep ^Error "$ERRFILE" | wc -l)
    echo "  -> $ERRFILE contains $ERRCOUNT errors"
    echo "  -> Creating backup $ORGFILE"
    mv "$MSGFILE" "$ORGFILE"
    echo "  -> Putting new file in place"
    mv new.msg  "$MSGFILE"
  else
    echo "!! Error ($ERR) processing $MSGFILE. Saving new.msg to ${OUTPUTFILE}"     
    mv new.msg "$OUTPUTFILE"
  fi
done