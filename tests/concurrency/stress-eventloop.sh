#!/bin/sh
# Стрес-прогін тестів fcl-eventloop: кожен набір запускається окремим
# процесом N разів з обмеженням часу.
# Використання: stress-eventloop.sh [N=20] [таймаут_с=30] [раннер]
N="${1:-20}"
LIMIT="${2:-30}"
RUNNER="${3:-$(dirname "$0")/out-eventloop/testeventloop}"
SUITES="TTestEventLoopCore TTestPipes TTestSockets TTestEventLoopGroup TTestBufferPool TTestAsyncFiles"
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
  printf '%-22s пройшло=%d впало=%d зависло=%d\n' "$s" $pass $fail $hang
done
if [ $total_bad -eq 0 ]; then rm -rf "$LOGDIR"; echo "УСЕ ГАРАЗД: fcl-eventloop пройшов стрес-тест"; else echo "ПРОБЛЕМИ: логи в $LOGDIR"; fi
[ $total_bad -eq 0 ]
