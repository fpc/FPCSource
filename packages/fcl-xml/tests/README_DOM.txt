Testing FCL DOM implementation with official test suite from w3.org
-------------------------------------------------------------------

*** IMPORTANT: READ CAREFULLY!

IF YOU ARE ABOUT TO RUN THESE TESTS, CONSIDER DOING SO IN AN ENVIRONMENT
THAT YOU MAY ALLOW TO BE TRASHED.

As of writing this at 3 June 2008, FCL DOM memory model is
not compatible - at all - with the way that w3.org tests use. In
particular, tests acquire (and use) references to objects that DOM
implementation frees. Therefore, running the tests WILL result in heap
corruption, executing arbitrary code, and any other imaginable kind of
disaster. Be warned.

[Update: This was fixed in SVN revision 13196, dated 26 May 2009.]

*** End of notice
--------------------------------------------------------------------

The fcl-xml package support for testing its DOM implementation consists of
the following files:

1) testgen.pp  - an utility for generating Pascal source from XML descriptions.
2) api.xml     - database used by testgen.
3) domunit.pp  - FPCUnit extensions required at runtime.
4) extras.pp   - Additional tests, not present in w3.org testsuite.
5) extras2.pp  - Some tests that are present in the testsuite, but converted/modified
                 by hand because automatic conversion is not yet possible.
6) README_DOM.txt - this file.


To test the FCL DOM implementation, follow these steps:

1) Checkout the DOM test suite from w3.org CVS repository. The project name is
2001/DOM-Test-Suite. Only 'tests' subdirectory is needed, everything else
is irrelevant for our purposes.
Use the following commands:

  CVSROOT=:pserver:anonymous@dev.w3.org:/sources/public
  cvs login
  (enter the password anonymous when prompted)
  cvs checkout 2001/DOM-Test-Suite/tests

2) Compile the testgen utility. A simple

  fpc testgen.pp

should do it.

3) Use testgen to convert DOM test suites into Pascal code. Specify path to the
directory that contains 'alltests.xml' file, and the name of resulting FPC unit.
Testgen expects the API description file 'api.xml' present in its directory.
Successful conversion of the following test modules is possible:

Level 1 Core (527 tests):
  testgen 2001/DOM-Test-Suite/tests/level1/core core1.pp

Level 2 Core (282 tests):
  testgen 2001/DOM-Test-Suite/tests/level2/core core2.pp
  
Level 2 HTML (677 out of 685 tests, only conversion -- not runnable yet):
  testgen 2001/DOM-Test-Suite/tests/level2/html html2.pp

Level 3 Core (partial only, 131 out of 722 tests):
  testgen 2001/DOM-Test-Suite/tests/level3/core core3.pp
  
Level 3 XPath (63 tests, only conversion -- not runnable yet):
  testgen 2001/DOM-Test-Suite/tests/level3/xpath xpath3.pp

In the examples above, output names (core1.pp, etc.) carry no defined meaning, except
they are used to construct test names which appear in the test reports. The test name
is constructed from the unit name by removing the extension and adding a 'TTest' prefix.

Normally, tests that contain properties/methods unsupported by FCL DOM, or
other elements not yet known to testgen, will be skipped. The conversion may be forced
by using -f commandline switch, but in this case the resulting Pascal unit will likely
fail to compile.
 
4) Now, pick up your preferred fpcunit test runner, add the generated units to its
uses clause, and compile. You may as well add the suppied 'extras.pp' and 'extras2.pp'
units. During compilation, path to 'domunit.pp' should be added to the unit search
paths.

5) During runtime, tests must be able to read test files which are located
within CVS source tree ('files' subdirectory of each module directory). For this purpose,
each generated test class contains a function 'GetTestFilesURI' which returns the path
that was supplied to testgen. Therefore, either use an absolute path, or ensure that both
generated sources and the test executable are located in the same directory.
