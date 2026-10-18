#!/bin/sh
# Стрес-прогін: кожен набір тестів System.Threading запускається окремим
# процесом N разів з обмеженням часу. Вихід 0, лише якщо все пройшло.
# Використання: stress.sh [N=50] [таймаут_с=60] [раннер]
N="${1:-50}"
LIMIT="${2:-60}"
RUNNER="${3:-$(dirname "$0")/out/testcompat}"
SUITES="TTestTExceptionList TTestAggregateException TTestSparseArray TTestWorkStealingQueue
TTestObjectCache TTestObjectCaches TTestThreading TTestTask TTestParallel"
LOGDIR=$(mktemp -d)
total_bad=0
for s in $SUITES; do
  pass=0; fail=0; hang=0
  i=1
  while [ $i -le "$N" ]; do
    log="$LOGDIR/${s}_$i.txt"
    timeout "$LIMIT" "$RUNNER" --suite="$s" --format=plain > "$log" 2>&1
    rc=$?
    if [ $rc -eq 0 ]; then pass=$((pass+1)); rm -f "$log"
    elif [ $rc -eq 124 ]; then hang=$((hang+1))
    else fail=$((fail+1)); fi
    i=$((i+1))
  done
  total_bad=$((total_bad+fail+hang))
  printf '%-26s пройшло=%d впало=%d зависло=%d\n' "$s" $pass $fail $hang
done
if [ $total_bad -eq 0 ]; then rm -rf "$LOGDIR"; echo "УСЕ ГАРАЗД"; else echo "ПРОБЛЕМИ: логи в $LOGDIR"; fi
[ $total_bad -eq 0 ]
