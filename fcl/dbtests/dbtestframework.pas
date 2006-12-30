program dbtestframework;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

{$APPTYPE CONSOLE}

uses
  SysUtils,
  fpcunit, testregistry, testreport,
  toolsunit,
  testbasics, testsqlfieldtypes, testdbbasics;
  
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
