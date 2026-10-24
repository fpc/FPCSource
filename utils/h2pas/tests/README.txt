h2pas test suite
================

The tests run the h2pas executable on small C headers and check the
generated Pascal unit. Some tests also compile the generated unit.

Building and running
--------------------

  fpc testh2pas.lpr
  ./testh2pas --all --format=plain

or open testh2pas.lpi in Lazarus.

The h2pas executable is located as follows:

  1. the H2PAS environment variable;
  2. ../h2pas, as built by make in utils/h2pas;
  3. ../bin/<cpu>-<os>/h2pas, as built by fpmake.

The compiler for the compilation checks is the FPC environment variable,
else fpc on the PATH. Without a compiler those tests are ignored.

Each test runs h2pas in its own directory below the system temporary
directory; the directory is removed after the test.

Test units
----------

  tch2pasbase.pp     harness: runs h2pas, normalizes and inspects the output
  tcdeclarations.pp  function prototypes, function bodies, variables
  tctypemapping.pp   C base types, with and without -a and -C
  tcstructs.pp       structs, unions, bit fields
  tctypedefs.pp      typedefs and enumerations
  tcmacros.pp        #define constants and macros
  tcpreprocessor.pp  comments and preprocessor directives
  tcoptions.pp       command-line options
  tcprefixes.pp      the -t, -T and -p prefixes and their combinations
  tcerrorrecovery.pp conversion after syntax errors in the header

All tests are registered below the H2Pas suite; a single test class or a
list of them can be run with --suite:

  ./testh2pas --suite=H2Pas
  ./testh2pas --suite=TTestPointerPrefix,TTestTypePrefix

Output matching
---------------

Before matching, runs of whitespace are collapsed to one space, lines are
trimmed and empty lines are dropped. An expected fragment is a list of lines
that occur consecutively; the last line may be a prefix of an output line.
