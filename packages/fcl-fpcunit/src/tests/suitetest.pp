{$mode objfpc}
{$h+}
{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2004 by Dean Zobec

    Port to Free Pascal of the JUnit framework.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit suitetest;

interface

uses
  fpcunit, testreport, testregistry;

type

  TNoTestCases = class(TTestCase)
  public
    procedure NoTestCase;
  end;

  {$M+}
  TNoTestCaseClass = class(TObject)
  published
    procedure TestSuccess;
  end;
  {$M-}

  TOneTestCase = class(TTestCase)
  public
    procedure NoTestCase;
  published
    procedure OnlyOneTestCase; virtual;
  end;

  TOverrideTestCase = class(TOneTestCase)
  published
    procedure OnlyOneTestCase; override;
  end;


  TInheritedTestCase = class(TOneTestCase)
  published
    procedure Test2;
  end;

  TSuiteTest = class(TTestCase)
  private
    FResult: TTestResult;
  protected
    procedure Setup; override;
    procedure Teardown; override;
  public
    class function Suite: TTestSuite;
  published
    procedure testNoTestCaseClass;
    procedure testNoTestCases;
    procedure testOneTestCase;
    procedure testInheritedTests;
    procedure testNotExistingTestCase;
    procedure testShadowedTests;
    procedure testAddTestSuiteFromClass;
    procedure testCreateTestSuiteFromArray;
  end;


implementation

procedure TNoTestCases.NoTestCase;
begin
end;

procedure TNoTestCaseClass.TestSuccess;
begin
end;

procedure TOneTestCase.NoTestCase;
begin
end;

procedure TOneTestCase.OnlyOneTestCase;
begin
end;

procedure TOverrideTestCase.OnlyOneTestCase;
begin
end;

procedure TInheritedTestCase.Test2;
begin
end;

procedure TSuiteTest.Setup;
begin
  FResult := TTestResult.Create;
end;

procedure TSuiteTest.Teardown;
begin
  FResult.Free;
end;

class function TSuiteTest.Suite: TTestSuite;
begin
  Result := TTestSuite.Create('TSuiteTest');
  Result.AddTest(TSuiteTest.CreateWithName('testNoTestCaseClass'));
  Result.AddTest(TSuiteTest.CreateWithName('testNoTestCases'));
  Result.AddTest(TSuiteTest.CreateWithName('testOneTestCase'));
  Result.AddTest(TSuiteTest.CreateWithName('testInheritedTests'));
  Result.AddTest(TSuiteTest.CreateWithName('testNotExistingTestCase'));
  Result.AddTest(TSuiteTest.CreateWithName('testShadowedTests'));
  Result.AddTest(TSuiteTest.CreateWithName('testAddTestSuiteFromClass'));
  Result.AddTest(TSuiteTest.CreateWithName('testCreateTestSuiteFromArray'));
end;

procedure TSuiteTest.testNoTestCaseClass;
var
  ts: TTestSuite;
begin
  ts := TTestSuite.Create(TNoTestCaseClass);
  ts.Run(FResult);
  ts.Free;
  AssertEquals(1, FResult.RunTests);
  AssertTrue(not FResult.WasSuccessful);
end;

procedure TSuiteTest.testNoTestCases;
var
  ts: TTestSuite;
begin
  ts := TTestSuite.Create(TNoTestCases);
  ts.Run(FResult);
  ts.Free;
  AssertTrue(FResult.RunTests = 1);
  AssertTrue(FResult.NumberOfFailures = 1);
  AssertTrue(not FResult.WasSuccessful);
end;

procedure TSuiteTest.testOneTestCase;
var
  ts: TTestSuite;
begin
  ts := TTestSuite.Create(TOneTestCase);
  ts.Run(FResult);
  ts.Free;
  AssertTrue(FResult.RunTests = 1);
  AssertTrue(FResult.NumberOfFailures = 0);
  AssertTrue(FResult.NumberOfErrors = 0);
  AssertTrue(FResult.WasSuccessful);
end;

procedure TSuiteTest.testInheritedTests;
var
  ts: TTestSuite;
begin
  ts := TTestSuite.Create(TInheritedTestCase);
  ts.Run(FResult);
  ts.Free;
  AssertEquals(2, FResult.RunTests);
  AssertTrue(FResult.WasSuccessful);
end;

procedure TSuiteTest.testNotExistingTestCase;
var
  t: TTestCase;
begin
  t := TSuiteTest.CreateWithName('notExistingMethod');
  t.Run(FResult);
  t.Free;
  AssertTrue(FResult.RunTests = 1);
  AssertTrue(FResult.NumberOfFailures = 1);
  AssertTrue(FResult.NumberOfErrors = 0);
end;

procedure TSuiteTest.testShadowedTests;
var
  ts: TTestSuite;
begin
  ts := TTestSuite.Create(TOverrideTestCase);
  ts.Run(FResult);
  ts.Free;
  AssertEquals(1, FResult.RunTests);
end;

procedure TSuiteTest.testAddTestSuiteFromClass;
var
  ts: TTestSuite;
begin
  ts := TTestSuite.Create;
  ts.AddTestSuiteFromClass(TOneTestCase);
  ts.Run(FResult);
  ts.Free;
  AssertEquals(1, FResult.RunTests);
end;

procedure TSuiteTest.testCreateTestSuiteFromArray;
var
  ts: TTestSuite;
begin
  ts := TTestSuite.Create([TOneTestCase, TInheritedTestCase]);
  try
    AssertEquals(3, ts.CountTestCases);
    AssertEquals(2, ts.Tests.Count);
    AssertEquals('TOneTestCase', ts[0].TestName);
    AssertEquals('TInheritedTestCase', ts[1].TestName);
  finally
    ts.Free;
  end;
end;

initialization

  RegisterTests([TSuiteTest]);

end.
