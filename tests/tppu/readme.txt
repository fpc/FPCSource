Test suite for running the compiler, building ppus, running again, checking what ppus were build.

How to use:
- Compile the compiler, for example via ../../compiler/ppcx64.lpi, creating ../../compiler/x86_64/pp
- Compile this testsuite testppu.lpi
- Run this testsuite:
  Set environment variable PP to the compiler and run all tests:
    PP=../../compiler/x86_64/pp ./testppu  

Or run a single test:

PP=../../compiler/x86_64/pp ./testppu --suite=TestImplInline1

