{$mode objfpc}
{$h+}
{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2004 by Dean Zobec, Michael Van Canneyt

    an example of a console test runner of FPCUnit tests.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit testreport;

interface

uses
  classes, SysUtils, fpcunit, testutils;

type

  { TXMLResultsWriter }

  TXMLResultsWriter = class(TNoRefCountObject, ITestListener)
  public
    procedure WriteHeader;
    procedure WriteResult(aResult: TTestResult);
  {ITestListener}
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
    procedure StartTestSuite(ATestSuite: TTestSuite);
    procedure EndTestSuite(ATestSuite: TTestSuite);
  end;

  { TPlainResultsWriter }

  TPlainResultsWriter = class(TNoRefCountObject, ITestListener)
  public
    procedure WriteHeader;
    procedure WriteResult(aResult: TTestResult);
  {ITestListener}
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
    procedure StartTestSuite(ATestSuite: TTestSuite);
    procedure EndTestSuite(ATestSuite: TTestSuite);
  end;

 {
  TLatexResultsWriter = class(TNoRefCountObject, ITestListener)
  public
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
    procedure StartTestSuite(ATestSuite: TTestSuite);
    procedure EndTestSuite(ATestSuite: TTestSuite);
  end;}

function TestSuiteAsXML(aSuite:TTestSuite; Indent : Integer): string;
function TestSuiteAsXML(aSuite: TTestSuite): string;
function TestSuiteAsLatex(aSuite:TTestSuite): string;
function TestSuiteAsPlain(aSuite:TTestSuite): string;
function GetSuiteAsXML(aSuite: TTestSuite): string;
function GetSuiteAsLatex(aSuite: TTestSuite): string;
function GetSuiteAsPlain(aSuite: TTestSuite): string;
function TestResultAsXML(aTestResult: TTestResult): string;
function TestResultAsPlain(aTestResult: TTestResult): string;

implementation

{TXMLResultsWriter}
procedure TXMLResultsWriter.WriteHeader;
begin
  writeln('<testresults>');
  writeln('<testlisting>');
end;

procedure TXMLResultsWriter.WriteResult(aResult: TTestResult);
begin
  writeln('</testlisting>');
  writeln(TestResultAsXML(aResult));
  writeln('</testresults>');
end;

procedure TXMLResultsWriter.AddFailure(ATest: TTest; AFailure: TTestFailure);
begin
  writeln('<failure ExceptionClassName="', AFailure.ExceptionClassName, '">');
  writeln('<message>', AFailure.ExceptionMessage, '</message>');
  writeln('</failure>');
end;

procedure TXMLResultsWriter.AddError(ATest: TTest; AError: TTestFailure);
begin
writeln('<error ExceptionClassName="', AError.ExceptionClassName, '">');
  writeln('<message>', AError.ExceptionMessage, '</message>');
  writeln('<sourceunit>', AError.SourceUnitName, '</sourceunit>');
  writeln('<methodname>', AError.FailedMethodName, '</methodname>');
  writeln('<linenumber>', AError.LineNumber, '</linenumber>');
  writeln('</error>');
end;

procedure TXMLResultsWriter.StartTest(ATest: TTest);
begin
  writeln('<test name="' , ATest.TestSuiteName + '.' + ATest.TestName, '">');
end;

procedure TXMLResultsWriter.EndTest(ATest: TTest);
begin
  writeln('</test>');
end;

procedure TXMLResultsWriter.StartTestSuite(ATestSuite: TTestSuite);
begin

end;

procedure TXMLResultsWriter.EndTestSuite(ATestSuite: TTestSuite);
begin

end;

{TPlainResultsWriter}
procedure TPlainResultsWriter.WriteHeader;
begin
end;

procedure TPlainResultsWriter.WriteResult(aResult: TTestResult);
begin
  writeln('', TestResultAsPlain(aResult));
end;

procedure TPlainResultsWriter.AddFailure(ATest: TTest; AFailure: TTestFailure);
begin
  writeln('', AFailure.ExceptionMessage);
end;

procedure TPlainResultsWriter.AddError(ATest: TTest; AError: TTestFailure);
begin
  writeln('  Error: ', AError.ExceptionClassName);
  writeln('    Exception:   ', AError.ExceptionMessage);
  writeln('    Source unit: ', AError.SourceUnitName);
  writeln('    Method name: ', AError.FailedMethodName);
  writeln('    Line number: ', AError.LineNumber);
end;

procedure TPlainResultsWriter.StartTest(ATest: TTest);
begin
  write('Test: ', ATest.TestSuiteName + '.' + ATest.TestName);
end;

procedure TPlainResultsWriter.EndTest(ATest: TTest);
begin
  writeln;
end;

procedure TPlainResultsWriter.StartTestSuite(ATestSuite: TTestSuite);
begin
  { example output }
//  Writeln('TestSuite: ' + ATestSuite.TestName);
end;

procedure TPlainResultsWriter.EndTestSuite(ATestSuite: TTestSuite);
begin
  { example output }
//  Writeln('TestSuite: ' + ATestSuite.TestName + ' - END ');
end;

function TestSuiteAsXML(aSuite:TTestSuite): string;

begin
  Result:=TestSuiteAsXML(ASuite,0);
end;

function TestSuiteAsXML(aSuite:TTestSuite; Indent : Integer): string;

var
  i: integer;
begin
  Result := StringOfChar(' ',Indent) + '<TestSuite name="' + ASuite.TestName + '">' + System.sLineBreak;
  Inc(Indent, 2);
  for i := 0 to aSuite.Tests.Count - 1 do
    if TTest(aSuite.Tests.Items[i]) is TTestSuite then
      Result := Result + TestSuiteAsXML(TTestSuite(aSuite.Tests.Items[i]),Indent)
    else
      if TTest(aSuite.Tests.Items[i]) is TTestCase then
        Result := Result + StringOfChar(' ',Indent) + '<test>' + TTestcase(aSuite.Tests.Items[i]).TestName + '</test>' + System.sLineBreak;
  Dec(Indent, 2);
  Result := Result + StringOfChar(' ',Indent) + '</TestSuite>' + System.sLineBreak;
end;


function TestSuiteAsLatex(aSuite:TTestSuite): string;
var
  i,j: integer;
  s: TTestSuite;
begin
  Result := '\flushleft' + System.sLineBreak;
  for i := 0 to aSuite.Tests.Count - 1 do
  begin
    s := TTestSuite(ASuite.Tests.Items[i]);
    Result := Result + s.TestSuiteName + System.sLineBreak;
    Result := Result + '\begin{itemize}'+ System.sLineBreak;
    for j := 0 to s.Tests.Count - 1 do
      if TTest(s.Tests.Items[j]) is TTestCase then
        Result := Result + '\item[-] ' + TTestcase(s.Tests.Items[j]).TestName  + System.sLineBreak;
    Result := Result +'\end{itemize}' + System.sLineBreak;
  end;
end;

function TestSuiteAsPlain(aSuite:TTestSuite): string;
var
  i,j: integer;
  s: TTestSuite;
begin
  for i := 0 to aSuite.Tests.Count - 1 do
    if TTest(aSuite.Tests.Items[i]) is TTestSuite then
      Result := Result + TestSuiteAsPlain(TTestSuite(aSuite.Tests.Items[i]))
    else
      if TTest(aSuite.Tests.Items[i]) is TTestCase then
        Result := Result + '  ' + ASuite.TestName+'.' + TTestcase(aSuite.Tests.Items[i]).TestName + System.sLineBreak;
end;

function GetSuiteAsXML(aSuite: TTestSuite): string;
begin
  if aSuite <> nil then
    begin
      if aSuite.TestName = '' then
        aSuite.TestName := 'Test Suite';
      Result := TestSuiteAsXML(aSuite)
    end
  else
    Result := '';
end;

function GetSuiteAsLatex(aSuite: TTestSuite): string;
begin
  if aSuite <> nil then
    begin
      Result := '\documentclass[a4paper,12pt]{article}' + System.sLineBreak;
      Result := Result + '\usepackage{array}' + System.sLineBreak;
      Result := Result + '\usepackage{mdwlist}' + System.sLineBreak + System.sLineBreak;
      Result := Result + '\begin{document}' + System.sLineBreak + System.sLineBreak;
      if aSuite.TestName = '' then
        aSuite.TestName := 'Test Suite';
      Result := Result + TestSuiteAsLatex(aSuite);
      Result := Result + '\end{document}';
    end
  else
    Result := '';
end;

function GetSuiteAsPlain(aSuite: TTestSuite): string;
begin
  Result := '';

  if aSuite <> nil then
    Result := 'TestSuites: ' + System.sLineBreak + TestSuiteAsPlain(aSuite);
end;

function TestResultAsXML(aTestResult: TTestResult): string;
var
  i: longint;
  f: TTestFailure;
begin
  with aTestResult do
  begin
    Result := '<NumberOfRunnedTests>' + intToStr(RunTests) + '</NumberOfRunnedTests>' + System.sLineBreak;
    Result := Result + '<NumberOfErrors>' + intToStr(NumberOfErrors) + '</NumberOfErrors>' + System.sLineBreak;
    Result := Result + '<NumberOfFailures>' + intToStr(NumberOfFailures) + '</NumberOfFailures>';
    if NumberOfErrors <> 0 then
    begin
      Result := Result + System.sLineBreak;
      Result := Result + '<ListOfErrors>';
      for i := 0 to Errors.Count - 1 do
      begin
        Result := Result + System.sLineBreak;
        Result := Result + '<Error>' + System.sLineBreak;
        f := TTestFailure(Errors.Items[i]);
        Result := Result + '  <Message>' + f.AsString + '</Message>' + System.sLineBreak;
        Result := Result + '  <ExceptionClass>' + f.ExceptionClassName + '</ExceptionClass>' + System.sLineBreak;
        Result := Result + '  <ExceptionMessage>' + f.ExceptionMessage + '</ExceptionMessage>' + System.sLineBreak;
        Result := Result + '  <SourceUnitName>' + f.SourceUnitName + '</SourceUnitName>' + System.sLineBreak;
        Result := Result + '  <LineNumber>' + IntToStr(f.LineNumber) + '</LineNumber>' + System.sLineBreak;
        Result := Result + '  <FailedMethodName>' + f.FailedMethodName + '</FailedMethodName>' + System.sLineBreak;
        Result := Result + '</Error>' + System.sLineBreak;
      end;
      Result := Result + '</ListOfErrors>';
    end;
    if NumberOfFailures <> 0 then
    begin
      Result := Result + System.sLineBreak;
      Result := Result + '<ListOfFailures>' + System.sLineBreak;
      for i := 0 to Failures.Count - 1 do
      begin
        Result := Result + '<Failure>' + System.sLineBreak;
        f := TTestFailure(Failures.Items[i]);
        Result := Result + '  <Message>' + f.AsString + '</Message>' + System.sLineBreak;
        Result := Result + '  <ExceptionClass>' + f.ExceptionClassName + '</ExceptionClass>' + System.sLineBreak;
        Result := Result + '  <ExceptionMessage>' + f.ExceptionMessage + '</ExceptionMessage>' + System.sLineBreak;
        Result := Result + '</Failure>' + System.sLineBreak;
      end;
      Result := Result + '</ListOfFailures>';
    end;
  end;
end;

function TestResultAsPlain(aTestResult: TTestResult): string;
var
  i: longint;
  f: TTestFailure;
begin
  with aTestResult do
  begin
    Result :=          'Number of run tests: ' + intToStr(RunTests) + System.sLineBreak;
    Result := Result + 'Number of errors:    ' + intToStr(NumberOfErrors) + System.sLineBreak;
    Result := Result + 'Number of failures:  ' + intToStr(NumberOfFailures);
    if NumberOfErrors <> 0 then
    begin
      Result := Result + System.sLineBreak;
      Result := Result + System.sLineBreak;
      Result := Result + 'List of errors:';
      for i := 0 to Errors.Count - 1 do
      begin
        Result := Result + System.sLineBreak;
        Result := Result + '  Error: ' + System.sLineBreak;
        f := TTestFailure(Errors.Items[i]);
        Result := Result + '    Message:           ' + f.AsString + System.sLineBreak;
        Result := Result + '    Exception class:   ' + f.ExceptionClassName + System.sLineBreak;
        Result := Result + '    Exception message: ' + f.ExceptionMessage + System.sLineBreak;
        Result := Result + '    Source unitname:   ' + f.SourceUnitName + System.sLineBreak;
        Result := Result + '    Line number:       ' + IntToStr(f.LineNumber) + System.sLineBreak;
        Result := Result + '    Failed methodname: ' + f.FailedMethodName + System.sLineBreak;
      end;
    end;
    if NumberOfFailures <> 0 then
    begin
      Result := Result + System.sLineBreak;
      Result := Result + System.sLineBreak;
      Result := Result + 'List of failures:' + System.sLineBreak;
      for i := 0 to Failures.Count - 1 do
      begin
        Result := Result + '  Failure: ' + System.sLineBreak;
        f := TTestFailure(Failures.Items[i]);
        Result := Result + '    Message:           ' + f.AsString + System.sLineBreak;
        Result := Result + '    Exception class:   ' + f.ExceptionClassName + System.sLineBreak;
        Result := Result + '    Exception message: ' + f.ExceptionMessage + System.sLineBreak;
      end;
    end;
  end;
  Result := Result + System.sLineBreak;
end;


end.
