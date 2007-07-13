{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2006 by Dean Zobec, Graeme Geldenhuys

    an example of XML report for FPCUnit tests.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
  

  Purpose:
    This unit contains a XML TestListener for use with the fpcUnit testing
    framework.  It uses the XMLWrite unit, which is part of FPC, to generate
    the XML document. The benefit of using the XMLWrite unit, is that the
    data generated is valid XML, with resevered characters correctly escaped.
    This allows the XML document to be further processed with XSLT etc without
    any issues.

}

unit xmltestreport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fpcunit, fpcunitreport, testutils, dom, XMLWrite;
  

type

  { TXMLResultsWriter }

  TXMLResultsWriter = class(TCustomResultsWriter)
  private
    FDoc: TXMLDocument;
    FResults, FListing: TDOMNode;
    FSuitePath: TFPList;
    FCurrentTest: TDOMElement;
  protected
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
    procedure WriteFooter; override;
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure); override;
    procedure AddError(ATest: TTest; AError: TTestFailure); override;
    procedure StartTest(ATest: TTest); override;
    procedure EndTest(ATest: TTest); override;
    procedure WriteResult(aResult: TTestResult); override;
    { A public property to the internal XML document }
    property Document: TXMLDocument read FDoc;
  end;

function GetSuiteAsXML(aSuite: TTestSuite): string;
function TestSuiteAsXML(n: TDOMElement; FDoc: TXMLDocument; aSuite:TTestSuite): string;

implementation

function GetSuiteAsXML(aSuite: TTestSuite): string;
var
  FDoc: TXMLDocument;
  n: TDOMElement;
  stream : TStringStream;
begin
  Result := '';

  if aSuite <> nil then
  begin
    FDoc:= TXMLDocument.Create;

    n := FDoc.CreateElement('TestSuites');
    FDoc.AppendChild(n);

    TestSuiteAsXML(n, FDoc, aSuite);

    stream := TStringStream.Create('');
    WriteXMLFile(FDoc, stream);
    writeln(stream.DataString);
    stream.Free;
  end;
end;

function TestSuiteAsXML(n: TDOMElement; FDoc: TXMLDocument; aSuite:TTestSuite): string;
var
  i: integer;
begin
  for i := 0 to Pred(aSuite.Tests.Count) do
    if TTest(aSuite.Tests.Items[i]) is TTestSuite then
      TestSuiteAsXML(n, FDoc, TTestSuite(aSuite.Tests.Items[i]))
    else
      if TTest(aSuite.Tests.Items[i]) is TTestCase then
        n.AppendChild(FDoc.CreateTextNode(TTestcase(aSuite.Tests.Items[i]).TestName + ' '));
end;


{ TXMLResultsWriter }

procedure TXMLResultsWriter.WriteTestHeader(ATest: TTest; ALevel: integer; ACount: integer);
var
  n: TDOMElement;
begin
  inherited;
  n := FDoc.CreateElement('Test');
  n['Name'] := ATest.TestName;
  n['Result'] := 'OK';
  if FSuitePath.Count > 0 then
  //test is included in a suite
    TDOMElement(FSuitePath[FSuitePath.Count -1]).AppendChild(n)
  else
  //no suite to append so append directly to the listing node
    FListing.AppendChild(n);
  FCurrentTest := n;
end;

procedure TXMLResultsWriter.WriteTestFooter(ATest: TTest; ALevel: integer; ATiming: TDateTime);
begin
  inherited;
  FCurrentTest['ElapsedTime'] := FormatDateTime('hh:nn:ss.zzz', ATiming);
end;


procedure TXMLResultsWriter.WriteSuiteHeader(ATestSuite: TTestSuite; ALevel: integer);
var
  n: TDOMElement;
begin
  inherited;
  n := FDoc.CreateElement('TestSuite');
  FSuitePath.Add(n); 
  n['Name'] := ATestSuite.TestName;
  if FSuitePath.Count = 1 then
    FListing.AppendChild(n)
  else
    TDOMElement(FSuitePath[FSuitePath.Count -2]).AppendChild(n);
end;


procedure TXMLResultsWriter.WriteSuiteFooter(ATestSuite: TTestSuite; ALevel: integer; 
  ATiming: TDateTime; ANumRuns: integer; ANumErrors: integer; ANumFailures: integer;
  ANumIgnores: integer);
var
  n: TDOMElement;
begin
  inherited;
  n := TDomElement(FSuitePath[FSuitePath.Count -1]);
  n['ElapsedTime'] := FormatDateTime('hh:nn:ss.zzz', ATiming);
  n['NumberOfRunTests'] := IntToStr(ANumRuns);
  n['NumberOfErrors'] := IntToStr(ANumErrors);
  n['NumberOfFailures'] := IntToStr(ANumFailures);
  n['NumberOfIgnoredTests'] := IntToStr(ANumIgnores);
  FSuitePath.Delete(FSuitePath.Count -1);
end;

constructor TXMLResultsWriter.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FDoc:= TXMLDocument.Create;
  FSuitePath := TFPList.Create;
  FResults := nil;
  FListing := nil;
end;

destructor  TXMLResultsWriter.Destroy;
begin
  FResults := nil;
  FListing := nil;
  FSuitePath.Free;
  FDoc.Free;
  inherited Destroy;
end;


procedure TXMLResultsWriter.WriteHeader;
begin
  inherited;
  FResults := FDoc.CreateElement('TestResults');
  FResults.AppendChild(FDoc.CreateComment(' Generated using FPCUnit on '
    + FormatDateTime('yyyy-mm-dd hh:mm:ss', Now) ));
  FDoc.AppendChild(FResults);
  FListing := FDoc.CreateElement('TestListing');
  FResults.AppendChild(FListing);
end;


procedure TXMLResultsWriter.WriteFooter;
begin
  inherited;
end;

procedure TXMLResultsWriter.AddFailure(ATest: TTest; AFailure: TTestFailure);
begin
  inherited;
  if AFailure.IsIgnoredTest then
    FCurrentTest['Result'] := 'Ignored'
  else  
    FCurrentTest['Result'] := 'Failed';
    FCurrentTest.AppendChild(FDoc.CreateElement('Message')).AppendChild
      (FDoc.CreateTextNode(AFailure.AsString));
    FCurrentTest.AppendChild(FDoc.CreateElement('ExceptionClass')).AppendChild
      (FDoc.CreateTextNode(AFailure.ExceptionClassName));
    FCurrentTest.AppendChild(FDoc.CreateElement('ExceptionMessage')).AppendChild
      (FDoc.CreateTextNode(AFailure.ExceptionMessage));
end;

procedure TXMLResultsWriter.AddError(ATest: TTest; AError: TTestFailure);
begin
  inherited;
  FCurrentTest['Result'] := 'Error';
  FCurrentTest.AppendChild(FDoc.CreateElement('Message')).AppendChild
    (FDoc.CreateTextNode(AError.AsString));
  FCurrentTest.AppendChild(FDoc.CreateElement('ExceptionClass')).AppendChild
    (FDoc.CreateTextNode(AError.ExceptionClassName));
  FCurrentTest.AppendChild(FDoc.CreateElement('ExceptionMessage')).AppendChild
    (FDoc.CreateTextNode(AError.ExceptionMessage));
  FCurrentTest.AppendChild(FDoc.CreateElement('SourceUnitName')).AppendChild
    (FDoc.CreateTextNode(AError.SourceUnitName));
  FCurrentTest.AppendChild(FDoc.CreateElement('LineNumber')).AppendChild
    (FDoc.CreateTextNode(IntToStr(AError.LineNumber)));
  FCurrentTest.AppendChild(FDoc.CreateElement('FailedMethodName')).AppendChild
    (FDoc.CreateTextNode(AError.FailedMethodName));
end;


procedure TXMLResultsWriter.StartTest(ATest: TTest);
begin
  inherited;
end;


procedure TXMLResultsWriter.EndTest(ATest: TTest);
begin
  inherited;
end;

procedure TXMLResultsWriter.WriteResult(aResult: TTestResult);
var
  n, lResults: TDOMNode;
  f: text;
begin
  lResults := FDoc.FindNode('TestResults');
  n := FDoc.CreateElement('NumberOfRunTests');
  n.AppendChild(FDoc.CreateTextNode(IntToStr(aResult.RunTests)));
  lResults.AppendChild(n);

  n := FDoc.CreateElement('NumberOfErrors');
  n.AppendChild(FDoc.CreateTextNode(IntToStr(aResult.NumberOfErrors)));
  lResults.AppendChild(n);

  n := FDoc.CreateElement('NumberOfFailures');
  n.AppendChild(FDoc.CreateTextNode(IntToStr(aResult.NumberOfFailures)));
  lResults.AppendChild(n);
  
  n := FDoc.CreateElement('NumberOfIgnoredTests');
  n.AppendChild(FDoc.CreateTextNode(IntToStr(aResult.NumberOfIgnoredTests)));
  lResults.AppendChild(n);

  n := FDoc.CreateElement('TotalElapsedTime');
  n.AppendChild(FDoc.CreateTextNode(FormatDateTime('hh:nn:ss.zzz', 
    Now - aResult.StartingTime)));
  lResults.AppendChild(n);     

  { Summary of ISO 8601  http://www.cl.cam.ac.uk/~mgk25/iso-time.html }
  n := FDoc.CreateElement('DateTimeRan');
  n.AppendChild(FDoc.CreateTextNode(FormatDateTime('yyyy-mm-dd hh:mm:ss', Now)));
  lResults.AppendChild(n);

  // This is so that the GUI Test Runner doesn't output text as well.
  if FileName <> 'null' then
  begin
    system.Assign(f, FileName);
    rewrite(f);
    WriteXMLFile(FDoc, f);
    close(f);
  end;
end;

end.



