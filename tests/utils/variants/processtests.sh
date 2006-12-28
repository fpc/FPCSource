#!/bin/bash

# Generates the tests in tests/test/cg/variants by
# a) generating the test programs
# b) compiling them with kylix and running them
# c) changing them based on the Kylix compilation and running result, so
#    they become self-checking
# Only tested under Linux with Kylix installed, might also work under cygwin

./genvartests
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
      mv $file.new $file
    fi

    if grep YYY output >/dev/null; then
      sed -e "s/writeln('XXX')/halt(1)/" < $file > $file.new
      mv $file.new $file
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

  fi
done
