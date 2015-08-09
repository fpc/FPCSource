unit tcsdfdata;
// Tests specific functionality of SdfDataSet (multiline etc)
//                             and FixedFormatDataSet

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Fpcunit, TestRegistry,
  dateutils,sdfdata,ToolsUnit;

type

  { TTestSdfSpecific }

  TTestSdfSpecific = class(TTestCase)
  private
    TestDataset: TSdfDataset;
    function TestFileName(const FileName: string=''): string;
  protected
    procedure Setup; override;
    procedure Teardown; override;
  published
    procedure TestEmptyFileHeader;
    procedure TestEmptyFileNoHeader;
    procedure TestSingleLineHeader;
    procedure TestSingleLineNoHeader;
    procedure TestOutput;
    procedure TestDelimitedTextOutput;
    procedure TestEmptyFieldHeader;
    Procedure TestEmptyFieldNoHeader;
    procedure TestEmptyFieldContents;
    Procedure TestEmptyFieldHeaderStripTrailingDelimiters;
    Procedure TestStripTrailingDelimiters;
  end;

  { TTestFixedFormatSpecific }

  TTestFixedFormatSpecific = class(TTestCase)
  private
    TestDataset: TFixedFormatDataset;
    function TestFileName(const FileName: string=''): string;
    procedure CreateTestFile;
  protected
    procedure Setup; override;
    procedure Teardown; override;
  published
    procedure TestTrimSpace;
    procedure TestNoTrimSpace;
  end;

implementation

function Ttestsdfspecific.TestFileName(const FileName: string): string;
const
  DefaultTestFileName = 'test.csv';
begin
  if FileName = '' then
    Result := DefaultTestFileName
  else
    Result := FileName;

  if dbname <> '' then
    begin
    ForceDirectories(dbname);
    Result := IncludeTrailingPathDelimiter(dbname) + Result;
    end;

  if FileExists(Result) then DeleteFile(Result);
end;

procedure Ttestsdfspecific.TestEmptyFileHeader;
// An empty file should return 0 records even if it has a header
begin
  // with Schema, with Header line
  TestDataset.FirstLineAsSchema := True;
  TestDataset.FileName := TestFileName('empty.csv');
  TestDataset.Open;

  TestDataset.Last;
  TestDataset.First;  
  AssertEquals('Number of records in test dataset', 0, TestDataset.RecordCount);
  TestDataset.Close;
end;

procedure Ttestsdfspecific.TestEmptyFileNoHeader;
// An empty file should return 0 records even if it has a header
begin
  // with Schema, without Header line
  TestDataset.FirstLineAsSchema := False;
  TestDataset.FileName := TestFileName('empty.csv');
  TestDataset.Open;

  TestDataset.Last;
  TestDataset.First;  
  AssertEquals('Number of records in test dataset', 0, TestDataset.RecordCount);
  TestDataset.Close;
end;

procedure Ttestsdfspecific.TestSingleLineHeader;
// A file with a single data line and header should return 1 records
var
  FileStrings: TStringList;
begin
  // with Schema, with Header line, which differs from Schema
  TestDataset.FirstLineAsSchema := True;
  TestDataset.FileName := TestFileName('singleh.csv');

  FileStrings:=TStringList.Create;
  try
    FileStrings.Add('ID,NAME,BIRTHDAY,GENDER'); // 4 fields override 3 fields in Schema
    FileStrings.Add('1,SimpleName,31-12-1976,M');
    FileStrings.SaveToFile(TestDataset.FileName);
  finally
    FileStrings.Free;
  end;

  TestDataset.Open;
  AssertEquals('FieldDefs.Count', 4, TestDataset.FieldDefs.Count);
  AssertEquals('1', TestDataset.Fields[0].AsString); // just after Open

  TestDataset.Last;
  TestDataset.First;
  AssertEquals('RecNo', 1, TestDataset.RecNo);
  AssertEquals('RecordCount', 1, TestDataset.RecordCount);
  TestDataset.Close;
  AssertEquals('RecordCount after Close', 0, TestDataset.RecordCount);
end;

procedure Ttestsdfspecific.TestSingleLineNoHeader;
// A file with a single data line, no header should return 1 records
var
  FileStrings: TStringList;
begin
  // with Schema, without Header line
  TestDataset.FirstLineAsSchema := False;
  TestDataset.FileName := TestFileName('singleh.csv');

  FileStrings:=TStringList.Create;
  try
    FileStrings.Add('1,SimpleName,31-12-1976');
    FileStrings.SaveToFile(TestDataset.FileName);
  finally
    FileStrings.Free;
  end;

  TestDataset.Open;
  AssertEquals('FieldDefs.Count', 3, TestDataset.FieldDefs.Count);
  AssertEquals('1', TestDataset.Fields[0].AsString);

  TestDataset.Last;
  TestDataset.First;
  AssertEquals('RecNo', 1, TestDataset.RecNo);
  AssertEquals('RecordCount', 1, TestDataset.RecordCount);
  TestDataset.Close;
  AssertEquals('RecordCount after Close', 0, TestDataset.RecordCount);
end;

procedure Ttestsdfspecific.TestOutput;
// Basic assignment test: assign some difficult data to records and
// see if the RecordCount is correct.
const
  NAME: array[1..4] of string = (
    'J"T"',                             // Data with quotes
    'Hello, goodbye',                   // Data with delimiter
    '  Just a line with spaces     ',   // Regular data
    'Delimiter,"and";quote'             // Data with delimiter and quote
  );
var
  i: integer;
begin
  // with Schema, with Header line
  TestDataset.Schema[1] := 'NAME=30';
  TestDataset.FileName := TestFileName('output.csv');
  TestDataset.Open;

  // Fill test data
  TestDataset.Append;
  TestDataset.FieldByName('ID').AsInteger := 1;
  TestDataset.FieldByName('NAME').AsString := NAME[1];
  TestDataset.FieldByName('BIRTHDAY').AsDateTime := ScanDateTime('yyyymmdd', '19761231', 1);
  TestDataset.Post;

  TestDataset.Append;
  TestDataset.FieldByName('ID').AsInteger := 2;
  TestDataset.FieldByName('NAME').AsString := NAME[2];
  TestDataset.FieldByName('BIRTHDAY').AsDateTime := ScanDateTime('yyyymmdd', '19761231', 1);
  TestDataset.Post;

  TestDataset.Append;
  TestDataset.FieldByName('ID').AsInteger := 4;
  TestDataset.FieldByName('NAME').AsString := NAME[4];
  TestDataset.FieldByName('BIRTHDAY').AsDateTime := ScanDateTime('yyyymmdd', '19761231', 1);
  TestDataset.Post;

  TestDataset.Insert;
  TestDataset.FieldByName('ID').AsInteger := 3;
  TestDataset.FieldByName('NAME').AsString := NAME[3];
  TestDataset.FieldByName('BIRTHDAY').AsDateTime := ScanDateTime('yyyymmdd', '19761231', 1);
  TestDataset.Post;

  // test sequential order of records
  TestDataset.First;
  for i:=1 to 4 do begin
    AssertEquals('RecNo', i, TestDataset.RecNo);
    AssertEquals(i, TestDataset.FieldByName('ID').AsInteger);
    TestDataset.Next;
  end;
  // set/test RecNo
  for i:=1 to 4 do begin
    TestDataset.RecNo := i;
    AssertEquals('RecNo', i, TestDataset.RecNo);
    AssertEquals(i, TestDataset.FieldByName('ID').AsInteger);
  end;
  AssertEquals('RecordCount', 4, TestDataset.RecordCount);
  TestDataset.Close;
  AssertEquals('RecordCount after Close', 0, TestDataset.RecordCount);

  // reopen, retest
  TestDataset.Open;
  for i:=1 to 4 do begin
    AssertEquals(NAME[i], TestDataset.FieldByName('NAME').AsString);
    TestDataset.Next;
  end;
  AssertTrue('Eof', TestDataset.Eof);
end;

procedure Ttestsdfspecific.TestDelimitedTextOutput;
// Test if saving and loading data keeps the original values.
// Mainly check if writing & reading embedded quotes and CRLF works.
const
  Value1='Delimiter,"and";quote';
  Value2='J"T"';
  Value3='Just a long line';
  Value4='Just a quoted long line';
  Value5='multi'+#13+#10+'line';
  Value6='Delimiter,and;done';
  Value7='Some "random" quotes';
Var
  F : Text;
  i : integer;
begin
  // with Schema, with Header line
  TestDataset.Close;
  TestDataset.AllowMultiLine := True;
  TestDataset.FirstLineAsSchema := True;
  TestDataset.FileName := TestFileName('delim.csv');
  Assign(F, TestDataset.FileName);
  Rewrite(F);
  Writeln(F,'Field1,Field2,Field3,Field4,Field5,Field6,Field7');
  for i:=1 to 3 do
  begin
    Writeln(F,'"Delimiter,""and"";quote","J""T""",Just a long line,"Just a quoted long line","multi');
    Writeln(F,'line","Delimiter,and;done","Some ""random"" quotes"');
  end;
  Close(F);
  // Load our dataset
  TestDataset.Open;
  AssertEquals('FieldDefs.Count', 7, TestDataset.FieldDefs.Count);
  AssertEquals('RecordCount', 3, TestDataset.RecordCount);
  TestDataset.First;
  for i:=1 to 3 do
  begin
    AssertEquals('Field1', Value1, TestDataSet.Fields[0].AsString);
    AssertEquals('Field2', Value2, TestDataSet.Fields[1].AsString);
    AssertEquals('Field3', Value3, TestDataSet.Fields[2].AsString);
    AssertEquals('Field4', Value4, TestDataSet.Fields[3].AsString);
    AssertEquals('Field5', Value5, TestDataSet.Fields[4].AsString);
    AssertEquals('Field6', Value6, TestDataSet.Fields[5].AsString);
    AssertEquals('Field7' ,Value7, TestDataSet.Fields[6].AsString);
    TestDataSet.Next;
  end;
end;

procedure Ttestsdfspecific.TestEmptyFieldContents;
Var
  F : Text;
begin
  // with empty Field name in Header line
  TestDataset.FirstLineAsSchema := True;
  TestDataset.Delimiter := ';';
  TestDataset.FileName := TestFileName();

  Assign(F, TestDataset.FileName);
  Rewrite(F);
  Writeln(F,'1;2;3;;5');
  Writeln(F,'11;12;13;;15');
  Close(F);

  TestDataset.Open;
  AssertEquals('FieldDefs.Count',5,TestDataset.FieldDefs.Count);
  AssertEquals('RecordCount',1,TestDataset.RecordCount);
end;

procedure Ttestsdfspecific.TestEmptyFieldHeader;
Var
  F : Text;
begin
  // with empty Field name in Header line
  TestDataset.Delimiter := ';';
  TestDataset.FirstLineAsSchema := True;
  TestDataset.FileName := TestFileName();

  Assign(F, TestDataset.FileName);
  Rewrite(F);
  Writeln(F,'1;2;3;;5');
  Close(F);

  TestDataset.Open;
  AssertEquals('FieldDefs.Count',5,TestDataset.FieldDefs.Count);
  AssertEquals('RecordCount', 0, TestDataset.RecordCount);
end;

procedure Ttestsdfspecific.TestEmptyFieldNoHeader;

Var
  F : Text;
  S1,S2 : String;

begin
  // without Schema, without Header line
  TestDataset.Schema.Clear;
  TestDataset.FirstLineAsSchema := False;
  TestDataset.Delimiter := ';';
  TestDataset.FileName := TestFileName();

  Assign(F, TestDataset.FileName);
  Rewrite(F);
  Writeln(F,'value1;value2;;;');
  Close(F);

  with TestDataset do begin
    Open;
    AssertEquals('FieldDefs.Count', 5, FieldDefs.Count);
    AssertEquals('RecordCount', 1, RecordCount);
    // #1 record
    Edit;
    Fields[0].AsString := 'Value1';
    Post;
    AssertEquals('Fields[4]', '', Fields[4].AsString);
    // #2 record
    Append;
    Fields[1].AsString := 'Value2';
    Fields[2].AsString := 'Value"'; // embedded double quote
    Post;
    Close;
  end;

  Assign(F, TestDataset.FileName);
  Reset(F);
  ReadLn(F,S1);
  ReadLn(F,S2);
  Close(F);
  AssertEquals('Value1;value2;;;',S1);
  AssertEquals(';Value2;"Value""";;',S2);
end;

procedure Ttestsdfspecific.TestEmptyFieldHeaderStripTrailingDelimiters;
Var
  F : Text;
  S : String;

begin
  // without Schema, without Header line
  TestDataset.Schema.Clear;
  TestDataset.FirstLineAsSchema := False;
  TestDataset.Delimiter := ';';
  TestDataset.StripTrailingDelimiters := True;
  TestDataset.FileName := TestFileName();

  Assign(F, TestDataset.FileName);
  Rewrite(F);
  Writeln(F,'value1;value2;;;');
  Close(F);

  TestDataset.Open;
  AssertEquals('FieldDefs.Count',2,TestDataset.FieldDefs.Count);
  TestDataset.Edit;
  TestDataset.Fields[0].AsString:='Value1';
  TestDataset.Post;
  TestDataset.Close;

  Assign(F, TestDataset.FileName);
  Reset(F);
  ReadLn(F,S);
  Close(F);
  AssertEquals('No data lost','Value1;value2',S);
end;

procedure Ttestsdfspecific.TestStripTrailingDelimiters;
Var
  F : Text;
  S1,S2 : String;

begin
  // without Schema, with Header line
  TestDataset.Schema.Clear;
  TestDataset.FirstLineAsSchema := True;
  TestDataset.Delimiter := ';';
  TestDataset.StripTrailingDelimiters := True;
  TestDataset.FileName := TestFileName();;

  Assign(F, TestDataset.FileName);
  Rewrite(F);
  Writeln(F,'value1;value2;;;');
  Writeln(F,'value1;value2;;;');
  Close(F);

  TestDataset.Open;
  AssertEquals('FieldDefs.Count',2,TestDataset.FieldDefs.Count);
  TestDataset.Edit;
  TestDataset.Fields[0].AsString:='Value1';
  TestDataset.Post;
  TestDataset.Close;

  Assign(F, TestDataset.FileName);
  Reset(F);
  ReadLn(F,S1);
  ReadLn(F,S2);
  Close(F);
  AssertEquals('Headers lost','value1;value2;;;',S1); // should striping affect also header line ?
  AssertEquals('Data lost','Value1;value2',S2);
end;


procedure Ttestsdfspecific.Setup;

begin
  TestDataset := TSDFDataset.Create(nil);
  TestDataset.Delimiter := ',';
  TestDataset.FileMustExist := False;
  TestDataset.FirstLineAsSchema := True;
  TestDataset.TrimSpace := False;
  TestDataset.AllowMultiLine := False;
  TestDataset.Schema.Add('ID');
  TestDataset.Schema.Add('NAME');
  TestDataset.Schema.Add('BIRTHDAY');
end;

procedure Ttestsdfspecific.Teardown;
begin
  try
    TestDataset.Close;
  except
    //swallow
  end;

  TestDataset.Free;
  try
    //DeleteFile(FCSVFileName);
  except
    //swallow
  end;
end;


{ TTestFixedFormatSpecific }

procedure TTestFixedFormatSpecific.Setup;
begin
  TestDataset := TFixedFormatDataset.Create(nil);
  TestDataset.FileMustExist := False;
  TestDataset.Schema.Add('ID=1');
  TestDataset.Schema.Add('NAME=10');
  TestDataset.Schema.Add('BIRTHDAY=10');
end;

procedure TTestFixedFormatSpecific.Teardown;
begin
  TestDataSet.Close;
  TestDataSet.Free;
end;

function TTestFixedFormatSpecific.TestFileName(const FileName: string): string;
const
  DefaultTestFileName = 'test.sdf';
begin
  if FileName = '' then
    Result := DefaultTestFileName
  else
    Result := FileName;

  if dbname <> '' then
    begin
    ForceDirectories(dbname);
    Result := IncludeTrailingPathDelimiter(dbname) + Result;
    end;

  if FileExists(Result) then DeleteFile(Result);
end;

procedure TTestFixedFormatSpecific.CreateTestFile;
var
  FileStrings: TStringList;
begin
  FileStrings:=TStringList.Create;
  try
    FileStrings.Add('1John      2000-01-01');
    FileStrings.Add('2Christiana2001-02-02');
    FileStrings.SaveToFile(TestDataset.FileName);
  finally
    FileStrings.Free;
  end;
end;

procedure TTestFixedFormatSpecific.TestTrimSpace;
begin
  TestDataset.FileName := TestFileName();
  CreateTestFile;

  with TestDataset do begin
    Open;
    AssertEquals('FieldDefs.Count', 3, FieldDefs.Count);
    AssertEquals('1', Fields[0].AsString); // just after Open

    Last;
    First;
    AssertEquals('RecNo', 1, RecNo);
    AssertEquals('RecordCount', 2, RecordCount);
    AssertEquals('1', Fields[0].AsString);
    AssertEquals('John', Fields[1].AsString);
    Next;
    AssertEquals('2', Fields[0].AsString);
    AssertEquals('Christiana', Fields[1].AsString);
    Edit;
    Fields[1].AsString := 'Chris';
    Post;
    AssertEquals('Chris', Fields[1].AsString);
    Close; // save changes
    AssertEquals('RecordCount after Close', 0, RecordCount);
    Open;
    Next;
    AssertEquals('Chris', Fields[1].AsString);
  end;
end;

procedure TTestFixedFormatSpecific.TestNoTrimSpace;
begin
  TestDataset.FileName := TestFileName();
  CreateTestFile;

  with TestDataset do begin
    TrimSpace := False;
    Open;
    AssertEquals('1', Fields[0].AsString);
    AssertEquals('John      ', Fields[1].AsString);
    Next;
    AssertEquals('2', Fields[0].AsString);
    AssertEquals('Christiana', Fields[1].AsString);
    Edit;
    Fields[1].AsString := 'Chris';
    Post;
    AssertEquals('Chris     ', Fields[1].AsString);
    Close; // save changes
    Open;
    Next;
    AssertEquals('Chris     ', Fields[1].AsString);
  end;
end;

initialization
  // Only run these tests if we are running
  // sdf tests. After all, running these when testing
  // e.g. SQL RDBMS doesn't make sense.
  if uppercase(dbconnectorname)='SDFDS' then
    begin
    RegisterTest(TTestSdfSpecific);
    RegisterTest(TTestFixedFormatSpecific);
    end;
end.

