The different directories are organized as follows:

webtbs...........Tests which should succeed compilation
  Digits in filename refer to bug database entry
webtbf...........Tests which should fail compilation
  Digits in filename refer to bug database entry
test.............Some manual tests / testsuites
tbs..............Manual database tests (success in compilation)
tbf..............Manual database tests (fail compile)
units............Unit helper for doing the tests
utils............Utilities for processing tests


At the top of the test source code, some options
can be used to determine how the tests will be
processed (if processed automatically via make):

OPT...............Compiler option required to compile
CPU...............CPU Test target (i386,m68k,etc)
VERSION...........Compiler required to execute/compiler test
RESULT............Exit code of execution of test expected
GRAPH.............Requires graph unit
FAIL..............Compilation must fail
RECOMPILE.........????
NORUN.............Do not execute test, only compile it
KNOWN.............Known bug, will not be logged as bug
INTERACTIVE.......Do not execute test, as it requires user
			intervention
NOTE..............Output note when compiling/executing test

To actually start the testsuite:
do a simple
make full This should create a log of all failed tests.

make rundigest scans the created log file and outputs some statistics
make rundigest USESQL=YES sends the results to an SQL database


Also remote execution of the testsuite is possible
Requirements:
- current build tree contains a cross compiled rtl/fcl
- the cross compiler is installed works without passing extra parameters
- the tests tree is somewhere on the remote machine e.g. /mnt/cf/fpc/tests
- some dir, e.g. i386-utils contains a dotest executable for the host system
- ssh must work without keyboard interaction or extra parameters
then a example make command could be
make DOTEST=i386-utils/dotest FPC=ppcarm "DOTESTOPT=-Y-XParm-linux- -Rroot@192.168.44.9 -P/mnt/cf/fpc/tests -T"
