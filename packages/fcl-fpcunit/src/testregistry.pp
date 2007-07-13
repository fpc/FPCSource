{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2004 by Dean Zobec, Michael Van Canneyt

    Port to Free Pascal of the JUnit framework.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit testregistry;

{$mode objfpc}
{$h+}

interface

uses
  fpcunit, testdecorator;
  
type

  TTestDecoratorClass = class of TTestDecorator;


procedure RegisterTest(ATestClass: TTestCaseClass); overload;
procedure RegisterTest(ASuitePath: String; ATestClass: TTestCaseClass); overload;
procedure RegisterTest(ASuitePath: String; ATest: TTest); overload;

procedure RegisterTests(ATests: Array of TTestCaseClass);

procedure RegisterTestDecorator(ADecoratorClass: TTestDecoratorClass; ATestClass: TTestCaseClass);

function NumberOfRegisteredTests: longint;

function GetTestRegistry: TTestSuite;

implementation
uses
  Classes
  ;

var
  FTestRegistry: TTestSuite;

function GetTestRegistry: TTestSuite;
begin
  if not Assigned(FTestRegistry) then
    FTestRegistry := TTestSuite.Create;
  Result := FTestRegistry;
end;

procedure RegisterTestInSuite(ARootSuite: TTestSuite; APath: string; ATest: TTest);
var
  i: Integer;
  lTargetSuite: TTestSuite;
  lCurrentTest: TTest;
  lSuiteName: String;
  lPathRemainder: String;
  lDotPos: Integer;
  lTests: TFPList;
begin
  if APath = '' then
  begin
    // end recursion
    ARootSuite.AddTest(ATest);
  end
  else
  begin
    // Split the path on the dot (.)
    lDotPos := Pos('.', APath);
    if (lDotPos <= 0) then lDotPos := Pos('\', APath);
    if (lDotPos <= 0) then lDotPos := Pos('/', APath);
    if (lDotPos > 0) then
    begin
      lSuiteName := Copy(APath, 1, lDotPos - 1);
      lPathRemainder := Copy(APath, lDotPos + 1, length(APath) - lDotPos);
    end
    else
    begin
      lSuiteName := APath;
      lPathRemainder := '';
    end;

    // Check to see if the path already exists
    lTargetSuite := nil;
    lTests := ARootSuite.Tests;
    for i := 0 to lTests.Count -1 do
    begin
      lCurrentTest := TTest(lTests[i]);
      if lCurrentTest is TTestSuite then
      begin
        if (lCurrentTest.TestName = lSuiteName) then
        begin
          lTargetSuite := TTestSuite(lCurrentTest);
          break;
        end;
      end;  { if }
    end;  { for }

    if not Assigned(lTargetSuite) then
    begin
      lTargetSuite := TTestSuite.Create(lSuiteName);
      ARootSuite.AddTest(lTargetSuite);
    end;

    RegisterTestInSuite(lTargetSuite, lPathRemainder, ATest);
  end;  { if/else }
end;

procedure RegisterTest(ATestClass: TTestCaseClass);
begin
  GetTestRegistry.AddTestSuiteFromClass(ATestClass);
end;

procedure RegisterTest(ASuitePath: String; ATestClass: TTestCaseClass);
begin
  RegisterTestInSuite(GetTestRegistry, ASuitePath, TTestSuite.Create(ATestClass));
end;

procedure RegisterTest(ASuitePath: String; ATest: TTest);
begin
  RegisterTestInSuite(GetTestRegistry, ASuitePath, ATest);
end;

procedure RegisterTestDecorator(ADecoratorClass: TTestDecoratorClass; ATestClass: TTestCaseClass);
begin
  GetTestRegistry.AddTest(ADecoratorClass.Create(TTestSuite.Create(ATestClass)));
end;

procedure RegisterTests(ATests: Array of TTestCaseClass);
var
  i: integer;
begin
  for i := Low(ATests) to High(ATests) do
    if Assigned(ATests[i]) then
    begin
      RegisterTest(ATests[i]);
    end;
end;


function NumberOfRegisteredTests: longint;
begin
  Result := GetTestRegistry.CountTestCases;
end;

initialization
  FTestRegistry := nil;
finalization
  FTestRegistry.Free;
end.
