#!/bin/bash

# Generates the tests in tests/test/cg/variants by
# a) generating the test programs
# b) compiling them with kylix and running them
# c) changing them based on the Kylix compilation and running result, so
#    they become self-checking
# Only tested under Linux with Kylix installed, might also work under cygwin

./genvartests
BIGTEST=tnofalvarol.pp
BIGTESTMAIN=tnofalvarol.inc
rm ivarol*
rm $BIGTEST
rm $BIGTESTMAIN

echo '{$ifdef fpc}' > $BIGTEST
echo '{$mode delphi}' >> $BIGTEST
echo '{$else fpc}' >> $BIGTEST
echo '{$define FPC_HAS_TYPE_EXTENDED}' >> $BIGTEST
echo '{$endif fpc}' >> $BIGTEST
echo '{$define bigfile}' >> $BIGTEST
echo >> $BIGTEST

for file in tvarol*.pp
do
  dcc $file
  if [ $? -ne 0 ]; then
    echo '{ %fail }' > $file.new
    cat $file >> $file.new
    mv $file.new $file
  else
    ./`basename $file .pp` > output

    if grep XXX output >/dev/null; then
      sed -e "s/writeln('YYY')/halt(1)/" < $file > $file.new
      grep -v "writeln('XXX')" < $file.new > $file
      rm $file.new
    fi

    if grep YYY output >/dev/null; then
      sed -e "s/writeln('XXX')/halt(1)/" < $file > $file.new
      grep -v "writeln('YYY')" < $file.new > $file
      rm $file.new
    fi

    if grep VVV output >/dev/null; then
      sed -e "s/writeln('COMPFAILV')/raise tobject.create/" < $file > $file.new
    else
      sed -e "s/writeln('VVV')/halt(1)/" < $file > $file.new
    fi
    mv $file.new $file

    if grep QQQ output >/dev/null; then
      sed -e "s/writeln('COMPFAILQ')/raise tobject.create/" < $file > $file.new
    else
      sed -e "s/writeln('QQQ')/halt(1)/" < $file > $file.new
    fi
    mv $file.new $file

    if ! grep "ifdef FPC_HAS_TYPE_EXTENDED" $file >/dev/null; then
      namenr=`echo $file | sed -e 's/tvarol//' -e 's/\.pp//'`
#      lines=`wc -l < $file`
#      lines=$(($lines - 5))
#      tail -$lines < $file >> $BIGTEST
      echo "  dotest${namenr};" >> $BIGTESTMAIN
      newname=`echo $file | sed -e 's/tvarol/ivarol/'`
      mv $file $newname
      echo '{$i' $newname '}' >> $BIGTEST
    fi
  fi
done

echo >> $BIGTEST
echo Begin >> $BIGTEST
cat $BIGTESTMAIN >> $BIGTEST
echo End. >> $BIGTEST
rm $BIGTESTMAIN
