{ This unit contains the TTestRunner class, a base class for the a simple
  console test runner for fpcunit that can be used with FPC's testsuite. It only
  generates a plain test report, always runs all tests and sets the exitcode.

  Copyright (C) 2006 Vincent Snijders (original consoletestrunner.pas)
  Copyright (C) 2016 Sven Barth

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
}
unit simpletestrunner;

{$mode objfpc}{$H+}

interface

uses
  custapp, Classes, SysUtils, fpcunit, testregistry,
  fpcunitreport, plaintestreport;

const
  Version = '0.1';

type
  { TTestRunner }

  TTestRunner = class(TCustomApplication)
  protected
    procedure DoRun; override;
    function DoTestRun(ATest: TTest): Boolean;
  end;

implementation

function TTestRunner.DoTestRun(ATest: TTest): Boolean;
var
  ResultsWriter: TCustomResultsWriter;
  TestResult: TTestResult;
begin
  ResultsWriter := TPlainResultsWriter.Create(Nil);
  TestResult := TTestResult.Create;
  try
    TestResult.AddListener(ResultsWriter);
    ATest.Run(TestResult);
    ResultsWriter.WriteResult(TestResult);
    Result := (TestResult.NumberOfErrors = 0) and (TestResult.NumberOfFailures = 0);
  finally
    TestResult.Free;
    ResultsWriter.Free;
  end;
end;

procedure TTestRunner.DoRun;
begin
  if not DoTestRun(GetTestRegistry) then
    ExitCode := 1;
  Terminate;
end;

end.

