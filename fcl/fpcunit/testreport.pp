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
  TXMLResultsWriter = class(TNoRefCountObject, ITestListener)
  public
    procedure WriteHeader;
    procedure WriteResult(aResult: TTestResult);
  {ITestListener}
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
  end;

 {
  TLatexResultsWriter = class(TNoRefCountObject, ITestListener)
  public
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
  end;}


function TestSuiteAsXML(aSuite: TTestSuite): string;
function TestSuiteAsLatex(aSuite:TTestSuite): string;
function GetSuiteAsXML(aSuite: TTestSuite): string;
function GetSuiteAsLatex(aSuite: TTestSuite): string;
function TestResultAsXML(aTestResult: TTestResult): string;

implementation


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

{TXMLResultsWriter}
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



function TestSuiteAsXML(aSuite:TTestSuite): string;
var
  i: integer;
begin
  Result := '<TestSuite name="' + ASuite.TestName + '">' + System.sLineBreak;
  for i := 0 to aSuite.Tests.Count - 1 do
    if TTest(aSuite.Tests.Items[i]) is TTestSuite then
      Result := Result + TestSuiteAsXML(TTestSuite(aSuite.Tests.Items[i]))
    else
      if TTest(aSuite.Tests.Items[i]) is TTestCase then
        Result := Result +'<test>' + TTestcase(aSuite.Tests.Items[i]).TestName + '</test>' + System.sLineBreak;
  Result := Result + '</TestSuite>' + System.sLineBreak;
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

end.
