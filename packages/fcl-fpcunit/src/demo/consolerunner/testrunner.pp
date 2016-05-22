{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2004 by Dean Zobec, Michael Van Canneyt

    an example of a console test runner of FPCUnit tests.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
program testrunner;

{$mode objfpc}
{$h+}

uses
  custapp, Classes, SysUtils, fpcunit, suiteconfig, testreport, testregistry;


const
  ShortOpts = 'alh';
  Longopts: Array[1..5] of String = (
    'all','list','format:','suite:','help');
  Version = 'Version 0.2';

Type

  { TSingleInstanceTest }

  TSingleInstanceTest = Class(TTestCase)
  Protected
    FCreateCount : Integer;
    Class function SingleInstanceForSuite : Boolean; override;
  Public
    Constructor Create; override;
    Destructor Destroy; override;
  Published
    Procedure TestWillSucceed;
    Procedure TestWillAlsoSucceed;
    Procedure TestWillFail;
  end;

type
  TTestRunner = Class(TCustomApplication)
  private
    FXMLResultsWriter: TXMLResultsWriter;
  protected
    procedure   DoRun ; Override;
    procedure   doTestRun(aTest: TTest); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

{ TSingleInstanceTest }

class function TSingleInstanceTest.SingleInstanceForSuite: Boolean;
begin
  Result:=True;
end;

constructor TSingleInstanceTest.Create;
begin
  Inc(FCreateCount);
  inherited Create;
end;

destructor TSingleInstanceTest.Destroy;
begin
  Dec(FCreateCount);
  inherited Destroy;
end;

procedure TSingleInstanceTest.TestWillSucceed;
begin
  AssertEquals('Created once',1,FCreateCount);
end;

procedure TSingleInstanceTest.TestWillAlsoSucceed;
begin
  AssertTrue('Created once',FCreateCount>0);
end;

procedure TSingleInstanceTest.TestWillFail;
begin
  AssertTrue('Created more than once',FCreateCount>1);
end;


constructor TTestRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXMLResultsWriter := TXMLResultsWriter.Create;
end;


destructor TTestRunner.Destroy;
begin
  FXMLResultsWriter.Free;
end;


procedure TTestRunner.doTestRun(aTest: TTest);
var
  testResult: TTestResult;
begin
  testResult := TTestResult.Create;
  try
    testResult.AddListener(FXMLResultsWriter);
    aTest.Run(testResult);
    FXMLResultsWriter.WriteResult(testResult);
  finally
    testResult.Free;
  end;
end;


procedure TTestRunner.DoRun;
var
  I : Integer;
  S : String;
begin
  S:=CheckOptions(ShortOpts,LongOpts);
  If (S<>'') then
    Writeln(S);
  if HasOption('h', 'help') or (ParamCount = 0) then
  begin
    writeln(Title);
    writeln(Version);
    writeln('Usage: ');
    writeln('-l or --list to show a list of registered tests');
    writeln('default format is xml, add --format=latex to output the list as latex source');
    writeln('-a or --all to run all the tests and show the results in xml format');
    writeln('The results can be redirected to an xml file,');
    writeln('for example: ./testrunner --all > results.xml');
    writeln('use --suite=MyTestSuiteName to run only the tests in a single test suite class');
  end
  else;
    if HasOption('l', 'list') then
    begin
      if HasOption('format') then
      begin
        if GetOptionValue('format') = 'latex' then
          writeln(GetSuiteAsLatex(GetTestRegistry))
        else
          writeln(GetSuiteAsXML(GetTestRegistry));
      end
      else
        writeln(GetSuiteAsXML(GetTestRegistry));
    end;
  if HasOption('a', 'all') then
  begin
    doTestRun(GetTestRegistry)
  end
  else
    if HasOption('suite') then
    begin
      S := '';
      S:=GetOptionValue('suite');
      if S = '' then
        for I := 0 to GetTestRegistry.Tests.count - 1 do
          writeln(GetTestRegistry[i].TestName)
      else
      for I := 0 to GetTestRegistry.Tests.count - 1 do
        if GetTestRegistry[i].TestName = S then
        begin
          doTestRun(GetTestRegistry[i]);
        end;
    end;
  Terminate;
end;


var
  App: TTestRunner;


begin
  RegisterTest(TSingleInstanceTest);
  App := TTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'FPCUnit Console Test Case runner.';
  App.Run;
  App.Free;
end.

