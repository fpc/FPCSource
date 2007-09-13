This directory contains a framework to test several TDataset descendents.

The framework is based on the fpcunit unit-test system. The tests can be
executed using any fpcunit-testrunner. For example the console and graphical
fpcunit-test runners from Lazarus.
Simply add the test* units in this directory to the uses statement of the
test-runner and all tests will get registered and executed.

An simple test-runner (dbtestframework.pas) which generates XML-output is
included in this directory.

To test a TDataset descendent, a 'connector' is needed to test the database.
To add a new connector, add it to the uses-section in 'toolsunit.pas'. Several
connectors are available in the '*toolsunit.pas' files.

Which connector is currently used is dependent on the 'database.ini'
configuration file. Also some settings which are connector-dependent can be set
in that file. See 'database.ini.txt' for an example.

I hope this is enough information to get you started,

Joost van der Sluis (30-12-2006)




