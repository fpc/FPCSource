#!/bin/bash
echo "Compiling test framework";
fpc -glh dbtestframework.pas
if [ $? != 0 ]; then
  echo "Compilation failed";
  exit
fi
echo "Generating test list"
./dbtestframework --list 2>/dev/null | sed /TestSuites/d | tr -d '[:blank:]' > test-list.txt 
exit
for f in `cat test-list.txt`
do
  echo -n "Doing test $f"
  ./dbtestframework --suite=$f > $f-mem.txt 2>&1
  grep '^0 unfreed memory blocks' $f-mem.txt >/dev/null 2>&1
  EC=$?
  if [ $EC = 1 ]; then
    echo "Error:"
    echo "Memory leak in $f"
  else 
    if [ $EC = 0 ]; then
      echo "OK, removing log file."
      rm $f-mem.txt
    fi   
  fi
done 
NOTESTS=`cat test-list.txt | wc -l`
grep -L '^0 unfreed memory blocks' *-mem.txt > leaklist.txt
NOLEAKS=`cat leaklist.txt | wc -l`
echo "Failures:"
cat leaklist.txt
echo "$NOTESTS tests performed, $NOLEAKS tests have memleak"
# done
