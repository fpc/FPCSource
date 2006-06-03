program dbtestframework;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

{$include settings.inc}

{$APPTYPE CONSOLE}

uses
  SysUtils,
  fpcunit,testregistry,
  testbasics,
{$ifdef SQLDB_AVAILABLE}
  testsqlfieldtypes,
{$ENDIF}
{$IFDEF DBF_AVAILABLE}
  testdbbasics,
{$ENDIF}
  testreport;
  
var
  FXMLResultsWriter: TXMLResultsWriter;
  testResult: TTestResult;
begin
  testResult := TTestResult.Create;
  FXMLResultsWriter := TXMLResultsWriter.Create;
  try
    testResult.AddListener(FXMLResultsWriter);
    FXMLResultsWriter.WriteHeader;
    GetTestRegistry.Run(testResult);
    FXMLResultsWriter.WriteResult(testResult);
  finally
    testResult.Free;
    FXMLResultsWriter.Free;
  end;
end.
