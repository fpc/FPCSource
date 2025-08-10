unit utcCSVReadWrite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, punit, csvreadwrite;

procedure RegisterTests;

implementation

var
  FData: TStrings;
  FParser: TCSVParser;

procedure HaveNext(ARow, ACol: integer; AValue: String);
var
  CN: String;
begin
  CN := Format('Cell(row: %d, col: %d)', [ARow, ACol]);
  AssertTrue('Have ' + CN, FParser.ParseNextCell);
  AssertEquals(CN + ': Row matches', ARow, FParser.CurrentRow);
  AssertEquals(CN + ': Col matches', ACol, FParser.CurrentCol);
  AssertEquals(CN + ': Value', AValue, FParser.CurrentCellText);
end;

procedure AssertLine(ARow: Integer; const AValues: array of String);
var
  I: Integer;
begin
  for I := 0 to High(AValues) do
    HaveNext(ARow, I, AValues[I]);
end;

function Setup: TTestString;
begin
  Result := '';
  if Assigned(FParser) then
    FParser.Free;
  FParser := TCSVParser.Create;
  if Assigned(FData) then
    FData.Free;
  FData := TStringList.Create;
end;

function TearDown: TTestString;
begin
  Result := '';
  FreeAndNil(FData);
  FreeAndNil(FParser);
end;

function TCSVReadWrite_TestEmpty: TTestString;
begin
  Result := '';
  AssertNotNull('Have parser', FParser);
end;

function TCSVReadWrite_TestNormalLine: TTestString;
begin
  Result := '';
  FParser.SetSource('this,is,a,normal,line');
  AssertLine(0, ['this', 'is', 'a', 'normal', 'line']);
end;

function TCSVReadWrite_TestQuotedLine: TTestString;
begin
  Result := '';
  FParser.SetSource('"this","is","a","quoted","line"');
  AssertLine(0, ['this', 'is', 'a', 'quoted', 'line']);
end;

function TCSVReadWrite_TestInlineQuotedLine: TTestString;
begin
  Result := '';
  FParser.SetSource('"this","line",has,mixed" quoting"');
  AssertLine(0, ['this', 'line', 'has', 'mixed quoting']);
end;

function TCSVReadWrite_TestQuotedNewLine: TTestString;
begin
  Result := '';
  FParser.SetSource('"this","line",has,"an embedded' + LineEnding + 'newline"');
  AssertLine(0, ['this', 'line', 'has', 'an embedded' + LineEnding + 'newline']);
end;

function TCSVReadWrite_Test2Lines: TTestString;
begin
  Result := '';
 
  FParser.SetSource('"this","line",has,an embedded' + LineEnding + 'newline');
  AssertLine(0, ['this', 'line', 'has', 'an embedded']);
  AssertLine(1, ['newline']);
end;

function TCSVReadWrite_TestEscapedQuotes: TTestString;
begin
  Result := '';
  FParser.SetSource('"this","line",has,"an embedded "" quote"');
  AssertLine(0, ['this', 'line', 'has', 'an embedded " quote']);
end;

procedure RegisterTests;
begin
  AddSuite('TCSVReadWriteTests', @Setup, @TearDown,Nil,True);
  AddTest('TestEmpty', @TCSVReadWrite_TestEmpty, 'TCSVReadWriteTests');
  AddTest('TestNormalLine', @TCSVReadWrite_TestNormalLine, 'TCSVReadWriteTests');
  AddTest('TestQuotedLine', @TCSVReadWrite_TestQuotedLine, 'TCSVReadWriteTests');
  AddTest('TestInlineQuotedLine', @TCSVReadWrite_TestInlineQuotedLine, 'TCSVReadWriteTests');
  AddTest('TestQuotedNewLine', @TCSVReadWrite_TestQuotedNewLine, 'TCSVReadWriteTests');
  AddTest('Test2Lines', @TCSVReadWrite_Test2Lines, 'TCSVReadWriteTests');
  AddTest('TestEscapedQuotes', @TCSVReadWrite_TestEscapedQuotes, 'TCSVReadWriteTests');
end;

end.
