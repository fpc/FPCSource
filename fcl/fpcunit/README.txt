This is a new rewrite of the fpcunit test reports to make 
fpcunit independent of the xml dom library but still provide
a good sistem of reports.
The reports are meant to be used to write the results of the tests
with the new fpc packaging system.
It produces latex and plain reports. Timing of the 
executed tests is provided at the testcase and testsuite level.
Summary of the results of the contained testcases (number of
run tests, number of errors, failures and ignored tests)
is provided for each test suite.
A modified console test runner (consoletestrunner.pp) 
is included that uses this new reporting classes.
Of course a separate xmltestreport class is included for those
that would like to use the xml test report through xslt to produce
more complex html reports.


The console test runner has to be used as always,
for example:

program runtests;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, 
  add here the units containing your testcases;
  
var
  App: TTestRunner;

begin
  App := TTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'FPCUnit Console runner.';
  App.Run;
  App.Free;
end.
