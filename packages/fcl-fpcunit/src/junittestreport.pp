{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2021 by Joost van der Sluis (CNOC)

    An example of an XML report writer for FpcUnit tests.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
  

  Purpose:
    This unit contains a XML/JUnit TestListener for use with the fpcUnit testing
    framework. It uses the XMLWrite unit (part of FPC) to generate
    the XML document.
    The output is compatible to the output that the Ant JUnit task produces.
    This format is used by a lot of third-party tools to examine test results.

    The XSD found at https://github.com/windyroad/JUnit-Schema is used as a
    guideline for the format.

  Notes:
    Specify 'null' as the filename if you don't want to output to file (e.g.
    used by the GUI test runner which instead reads the Document property).

}

{$IFNDEF FPC_DOTTEDUNITS}
unit junittestreport;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils,FpcUnit.Test, FpcUnit.Reports, Xml.Dom, Xml.Writer,
  {$ifdef Unix}UnixApi.Unix,{$endif}
  System.DateUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils,fpcunit, fpcunitreport, dom, XMLWrite,
  {$ifdef unix}unix,{$endif}
  DateUtils;
{$ENDIF FPC_DOTTEDUNITS}
  

type

  { TJUnitResultsWriter }

  TJUnitResultsWriter = class(TCustomResultsWriter)
  private
    FDoc: TXMLDocument;
    // When there are no suites, create an artificial one and add it to the root
    // of the XML-document.
    // JvdS: I do not know if this can ever happens, but the TXMLResultsWriter
    // has similar logic...
    FSingleSuite: TDOMElement;
    // When there are suites, create a list of suites and add it to the root
    // of the XML-document.
    FMultipleSuites: TDOMElement;
    FSuitePath: TFPList;
    FCurrentTest: TDOMElement;
    FTestSuiteCount: Integer;
    // The result (testsuites) is flattened, so we have to keep our own count.
    FTestCount: Integer;
    FFailureCount: Integer;
    FIgnoreCount: Integer;
    FErrorCount: Integer;
  protected
    function GetSingleSuiteElement: TDOMElement;
    function GetMultipleSuitesElement: TDOMElement;
    function GetCurrentElement: TDOMElement;
    procedure WriteTestHeader(ATest: TTest; ALevel: integer; ACount: integer); override;
    procedure WriteTestFooter(ATest: TTest; ALevel: integer; ATiming: TDateTime); override;
    procedure WriteSuiteHeader(ATestSuite: TTestSuite; ALevel: integer); override;
    procedure WriteSuiteFooter(ATestSuite: TTestSuite; ALevel: integer; 
      ATiming: TDateTime; ANumRuns: integer; ANumErrors: integer; 
      ANumFailures: integer; ANumIgnores: integer); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure WriteHeader; override;
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure); override;
    procedure AddError(ATest: TTest; AError: TTestFailure); override;
    procedure WriteResult(aResult: TTestResult); override;
    { A public property to the internal XML document }
    property Document: TXMLDocument read FDoc;
  end;

implementation

{ TJUnitResultsWriter }

function TJUnitResultsWriter.GetCurrentElement: TDOMElement;
begin
  if Assigned(FCurrentTest) then
    Result := FCurrentTest
  else if FSuitePath.Count > 0 then
  //test is included in a suite
    Result := TDOMElement(FSuitePath[FSuitePath.Count -1])
  else
  //no suite to append so append it to the single-suite element
    Result := GetSingleSuiteElement;
end;

procedure TJUnitResultsWriter.WriteTestHeader(ATest: TTest; ALevel: integer; ACount: integer);
var
  n: TDOMElement;
begin
  inherited;
  n := FDoc.CreateElement('testcase');
  n['name'] := ATest.TestName;
  n['classname'] := ATest.ClassName;
  if FSuitePath.Count > 0 then
    // test is included in a suite
    TDOMElement(FSuitePath[FSuitePath.Count -1]).AppendChild(n)
  else
    // no suite to append so append to the artificial suite
    GetSingleSuiteElement.AppendChild(n);
  FCurrentTest := n;
  Inc(FTestCount);
end;

procedure TJUnitResultsWriter.WriteTestFooter(ATest: TTest; ALevel: integer; ATiming: TDateTime);
begin
  inherited;
  if not SkipTiming then
    FCurrentTest['time'] := FloatToStrF(ATiming * SecsPerDay, ffFixed, 1, 3);
end;


procedure TJUnitResultsWriter.WriteSuiteHeader(ATestSuite: TTestSuite; ALevel: integer);
var
  n: TDOMElement;
  s: string;
begin
  inherited;
  n := FDoc.CreateElement('testsuite');
  FSuitePath.Add(n);
  n['name'] := ATestSuite.TestSuiteName;
  n['timestamp'] := DateToISO8601(NowUTC);
  {$ifdef unix}
  s := GetHostName;
  if s = '' then
    s := 'localhost';
  n['hostname'] := s;
  {$endif}
  n['id'] := IntToStr(FTestSuiteCount);
  Inc(FTestSuiteCount);
  GetMultipleSuitesElement.AppendChild(n);

  FTestCount := 0;
  FIgnoreCount := 0;
  FErrorCount := 0;
  FFailureCount := 0;
end;


procedure TJUnitResultsWriter.WriteSuiteFooter(ATestSuite: TTestSuite; ALevel: integer;
  ATiming: TDateTime; ANumRuns: integer; ANumErrors: integer; ANumFailures: integer;
  ANumIgnores: integer);
var
  n: TDOMElement;
begin
  inherited;

  n := TDomElement(FSuitePath[FSuitePath.Count -1]);
  if n.ChildNodes.Count = 0 then
    begin
    // some testsuites only contain child testsuites. Those are omitted from the XML.
    n.Free;
    end
  else
    begin
    n['tests'] := IntToStr(FTestCount);
    n['failures'] := IntToStr(FFailureCount);
    n['errors'] := IntToStr(FErrorCount);
    n['skipped'] := IntToStr(FIgnoreCount);
    if not SkipTiming then
      n['time'] := FloatToStrF(ATiming * SecsPerDay, ffFixed, 1, 3);
    end;
  FSuitePath.Delete(FSuitePath.Count -1);
end;

constructor TJUnitResultsWriter.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FDoc:= TXMLDocument.Create;
  FSuitePath := TFPList.Create;
end;

destructor  TJUnitResultsWriter.Destroy;
begin
  FSingleSuite := nil;
  FMultipleSuites := nil;
  FSuitePath.Free;
  FDoc.Free;
  inherited Destroy;
end;


procedure TJUnitResultsWriter.WriteHeader;
begin
  inherited;
  FTestSuiteCount := 0;
end;

procedure TJUnitResultsWriter.AddFailure(ATest: TTest; AFailure: TTestFailure);
var
  CurrentElement: TDOMElement;
begin
  inherited;
  CurrentElement := GetCurrentElement;
  if AFailure.IsIgnoredTest then
    begin
    CurrentElement.AppendChild(FDoc.CreateElement('skipped'));
    Inc(FIgnoreCount);
    end
  else
    begin
    CurrentElement := CurrentElement.AppendChild(FDoc.CreateElement('failure')) as TDOMElement;

    CurrentElement.AppendChild(FDoc.CreateElement('message')).AppendChild
      (FDoc.CreateTextNode(AFailure.ExceptionMessage));
    CurrentElement.AppendChild(FDoc.CreateElement('name')).AppendChild
      (FDoc.CreateTextNode(AFailure.ExceptionClassName));
    CurrentElement.AppendChild(FDoc.CreateTextNode(AFailure.AsString));
    Inc(FFailureCount);
    end;
end;

procedure TJUnitResultsWriter.AddError(ATest: TTest; AError: TTestFailure);
var
  CurrentElement: TDOMElement;
begin
  inherited;
  CurrentElement := GetCurrentElement;
  CurrentElement := CurrentElement.AppendChild(FDoc.CreateElement('error')) as TDOMElement;

  CurrentElement.AppendChild(FDoc.CreateElement('message')).AppendChild
    (FDoc.CreateTextNode(AError.ExceptionMessage));
  CurrentElement.AppendChild(FDoc.CreateElement('name')).AppendChild
    (FDoc.CreateTextNode(AError.ExceptionClassName));
  CurrentElement.AppendChild(FDoc.CreateTextNode(AError.AsString));
  Inc(FErrorCount);
end;

procedure TJUnitResultsWriter.WriteResult(aResult: TTestResult);
var
  f: text;
begin
  // This is so that the GUI Test Runner doesn't output text as well.
  if FileName <> 'null' then
  begin
    system.Assign(f, FileName);
    rewrite(f);
    WriteXMLFile(FDoc, f);
    close(f);
  end;
end;

function TJUnitResultsWriter.GetSingleSuiteElement: TDOMElement;
begin
  if not Assigned(FSingleSuite) then
    begin
    FSingleSuite := FDoc.CreateElement('testsuite');
    FSingleSuite['timestamp'] := DateToISO8601(NowUTC);
    FDoc.AppendChild(FSingleSuite);
    end;
  Result := FSingleSuite;
end;

function TJUnitResultsWriter.GetMultipleSuitesElement: TDOMElement;
begin
  if not Assigned(FMultipleSuites) then
    begin
    FMultipleSuites := FDoc.CreateElement('testsuites');
    FDoc.AppendChild(FMultipleSuites);
    end;
  Result := FMultipleSuites;
end;

end.

