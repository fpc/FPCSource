These tests are to thest the fppkg system.

The tests have the structure of unit-tests, but are not real unit-tests, altough
some tests may test the TpkgFPpkg-class in a unit-test way.

To be able to test fppkg, fpc-installations are made in in the templates-
directory. They can have several settings and options.

On each test one of these templates is synchronised to the current-test
directory. Then the test can do something to this current-test environment
without influencing the environment for other tests. And without having to
re-build the test-environment for each test.

To be able to build the test-templates, the tests need the full sources of fpc
and a start-compiler.

Usage is the same as for other unit-tests. Special options are:

 -f <path> or --fpcsrcpath=<path>
   The location of the full fpc-sources (fpcsrc)

 -t <path> or --testpath=<path>
   The location where the test-environment and templates has are build
   The default is the 'testroot' sub-directory of the directory where the
   executable resides.

 -p <path> or --packagespath=<path>
   The location of the packages which are used in the tests. This are the
   packages which are in the 'packages' directory that belongs to these tests.
   The default is the 'packages' sub-directory of the directory where the
   executable resides.

 -s <startcompiler> or --startcompiler=<startcompiler>
   The compiler that has to be used to compile fpc

 -T or --skipbuildtemplate
   Do not (re)-build the fpc-installation templates. To speed up the tests
   when it is not necessary to recompile fpc.

For example, to run the tests:

  fppkg_tests --all --fpcsrcpath=~/svn/fpc-trunk --startcompiler=ppcx64_3.0.0

Or to run the tests, but use a temporary directory for the test-setup:

  fppkg_tests --all --fpcsrcpath=~/svn/fpc-trunk --startcompiler=ppcx64_3.0.0 --testpath=/tmp/temptest

Or to run successive tests, without re-building the setup:

  fppkg_tests --all --testpath=/tmp/temptest --skipbuildtemplate
