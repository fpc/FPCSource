unit utcCSVDocument;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, punit, csvdocument;

procedure RegisterTests;

implementation

const
  TestFileName = 'test.csv';
  ColCount = 3;
  RowCount = 4;

type
  TRow = array[0..ColCount-1] of String;
  TCells = array[0..RowCount-1] of TRow;

const
  Cells : TCells = (
    ('a','b','c'),
    ('1','"one"','1.1'),
    ('2','"two"','2.2'),
    ('3','"three"','3.3')
  );

var
  FDoc: TCSVDocument;

procedure RemoveTestFile;
begin
  if FileExists(TestFileName) then
    DeleteFile(TestFileName);
end;

function StripQuotes(S: String): String;
var
  L: integer;
begin
  Result := S;
  L := Length(Result);
  if (L > 1) and (Result[1] = '"') and (Result[L] = '"') then
    Result := Copy(Result, 2, L - 2);
end;

procedure CreateTestFile;
var
  L: TStringList;
  R, C: Integer;
  S: String;
begin
  L := TStringList.Create;
  try
    for R := 0 to RowCount - 1 do
    begin
      S := '';
      for C := 0 to ColCount - 1 do
      begin
        if S <> '' then
          S := S + ',';
        S := S + Cells[R, C];
      end;
      L.Add(S);
    end;
    L.SaveToFile(TestFileName);
  finally
    L.Free;
  end;
end;

procedure TestTheFile;
var
  R, C: Integer;
begin
  AssertEquals('Row count', RowCount, FDoc.RowCount);
  for R := 0 to RowCount - 1 do
    for C := 0 to ColCount - 1 do
    begin
      AssertEquals('Col[' + IntToStr(R) + '] count', ColCount, FDoc.ColCount[R]);
      AssertEquals(Format('Cell[%d,%d]', [C, R]), StripQuotes(Cells[R, C]), FDoc.Cells[C, R]);
    end;
end;

function Setup: TTestString;
begin
  Result := '';
  FDoc := TCSVDocument.Create;
end;

function TearDown: TTestString;
begin
  Result := '';
  RemoveTestFile;
  FreeAndNil(FDoc);
end;

function TCSVDocument_TestEmpty: TTestString;
begin
  Result := '';
  AssertNotNull('Have document', FDoc);
end;

function TCSVDocument_TestRead: TTestString;
begin
  Result := '';
  CreateTestFile;
  FDoc.LoadFromFile(TestFileName);
  TestTheFile;
end;

procedure RegisterTests;
begin
  AddSuite('TCSVDocumentTests', @Setup, @TearDown);
  AddTest('TestEmpty', @TCSVDocument_TestEmpty, 'TCSVDocumentTests');
  AddTest('TestRead', @TCSVDocument_TestRead, 'TCSVDocumentTests');
end;

end.
