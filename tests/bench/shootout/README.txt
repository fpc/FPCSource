This is a download of the shootout benchmark.

Shootout Website:
http://shootout.alioth.debian.org/

Old win32 site:
http://dada.perl.it/shootout/index.html

Directories:
Dir       Contents
---       --------
src       Pascal source files. bench.c contains sources of all (gcc) C equivalents.
obsolete  Tests not used anymore on shootout.alioth.debian.org
log       Logs from shootout tests on website.
io        Input and expected output files for some tests.


Files:
fpascal2.diff   Patch for shootout CVS checkout to add a second FPC release for
                comparison with 2 FPC versions

Running shootout tip:

If you want to run the tests for FPC and GCC don't forget to include a * in front of the name
otherwise it will not newer revisions (called benchmark.lang-#.lang) of some benchmarks

To run multiple languages use:

make SELECT_LANGS='{*fpascal,*fpascal2,*gcc}' clean plot

to run a single language don't use the { }:

make SELECT_LANGS='*fpascal' clean plot
