unit tcsdfdata;
// Tests specific functionality of sdfdataset (multiline etc)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Fpcunit, TestRegistry,
  dateutils,sdfdata,ToolsUnit;

type

  { Ttestsdfspecific }

  Ttestsdfspecific = class(Ttestcase)
  private
    function TestFileName(const FileName: string=''): string;
  protected
    TestDataset: TSDFDataset;
    procedure Setup; override;
    procedure Teardown; override;
  published
    procedure TestEmptyFileHeader;
    procedure TestEmptyFileNoHeader;
    procedure TestSingleLineHeader;
    procedure TestSingleLineNoHeader;
    procedure TestOutput;
    {
    November 2012: this test tests again sdf;
    however sdfdataset should comply with RFC4180 CSV, see issue #22980
    todo: rewrite test to RFC4180
    procedure TestInputOurFormat;
    }
    procedure TestDelimitedTextOutput;
    procedure TestEmptyFieldHeader;
    Procedure TestEmptyFieldNoHeader;
    procedure TestEmptyFieldContents;
    Procedure TestEmptyFieldHeaderStripTrailingDelimiters;
    Procedure TestStripTrailingDelimiters;
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
var
  i: integer;
begin
  // with Schema, with Header line
  TestDataset.FileName := TestFileName('output.csv');
  TestDataset.Open;
  // Fill test data
  TestDataset.Append;
  TestDataset.FieldByName('ID').AsInteger := 1;
  // Data with quotes
  TestDataset.FieldByName('NAME').AsString := 'J"T"';
  TestDataset.FieldByName('BIRTHDAY').AsDateTime := ScanDateTime('yyyymmdd', '19761231', 1);
  TestDataset.Post;

  TestDataset.Append;
  TestDataset.FieldByName('ID').AsInteger := 2;
  // Data with delimiter
  TestDataset.FieldByName('NAME').AsString := 'Hello'+TestDataset.Delimiter+' goodbye';
  TestDataset.FieldByName('BIRTHDAY').AsDateTime := ScanDateTime('yyyymmdd', '19761231', 1);
  TestDataset.Post;

  TestDataset.Append;
  TestDataset.FieldByName('ID').AsInteger := 4;
  //Data with delimiter and quote (to test 19376)
  TestDataset.FieldByName('NAME').AsString := 'Delimiter,"and";quote';
  TestDataset.FieldByName('BIRTHDAY').AsDateTime := ScanDateTime('yyyymmdd', '19761231', 1);
  TestDataset.Post;

  TestDataset.Insert;
  TestDataset.FieldByName('ID').AsInteger := 3;
  // Regular data
  TestDataset.FieldByName('NAME').AsString := 'Just a long line of text without anything special';
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
end;

{
procedure Ttestsdfspecific.TestInputOurFormat;
// Test if input works as expected: output is written according to specs and read in.
// Mainly check if reading quotes is according to Delphi sdf specs and works.
// See test results from bug 19610 for evidence that the strings below should work.
// If this works, we can switch to this and be RFC 4180 compliant and Delphi compliant.
const
  OutputFileName='input.csv';
  //Value1 is the on disk format; it should translate to Expected1
  Value1='"Delimiter,""and"";quote"';
  Expected1='Delimiter,"and";quote';
  Value2='"J""T"""';
  Expected2='J"T"';
  Value3='Just a long line';
  Expected3='Just a long line';
  //Note: Delphi can read this, see evidence in bug 19610 (the "quoted and space" value)
  Value4='"Just a quoted long line"';
  Expected4='Just a quoted long line';
  // Delphi can read multiline, see evidence in bug 19610 (the multiline entry)
  Value5='"quoted_multi'+#13+#10+'line"';
  Expected5='quoted_multi'+#13+#10+'line';
  Value6='"Delimiter,and;quoted"';
  Expected6='Delimiter,and;quoted';
  Value7='"A random""quote"';
  Expected7='A random"quote';
var
  FileStrings: TStringList;
begin
  TestDataset.Close;
  TestDataset.AllowMultiLine:=true;
  if FileExists(OutputFilename) then DeleteFile(OutputFileName);
  FileStrings:=TStringList.Create;
  try
    FileStrings.Add('ID,NAME,BIRTHDAY');
    FileStrings.Add('1,'+Value1+',31-12-1976');
    FileStrings.Add('2,'+Value2+',31-12-1976');
    FileStrings.Add('3,'+Value3+',31-12-1976');
    FileStrings.Add('4,'+Value4+',31-12-1976');
    FileStrings.Add('5,'+Value5+',31-12-1976');
    FileStrings.Add('6,'+Value6+',31-12-1976');
    FileStrings.Add('7,'+Value7+',31-12-1976');
    FileStrings.SaveToFile(OutputFileName);
  finally
    FileStrings.Free;
  end;

  // Load our dataset
  TestDataset.FileName:=OutputFileName;
  TestDataset.Open;
  TestDataset.First;
  AssertEquals(Expected1, TestDataSet.FieldByName('NAME').AsString);
  TestDataSet.Next;
  AssertEquals(Expected2, TestDataSet.FieldByName('NAME').AsString);
  TestDataSet.Next;
  AssertEquals(Expected3, TestDataSet.FieldByName('NAME').AsString);
  TestDataSet.Next;
  AssertEquals(Expected4, TestDataSet.FieldByName('NAME').AsString);
  TestDataSet.Next;
  AssertEquals(Expected5, TestDataSet.FieldByName('NAME').AsString);
  TestDataSet.Next;
  AssertEquals(Expected6, TestDataSet.FieldByName('NAME').AsString);
  TestDataSet.Next;
  AssertEquals(Expected7, TestDataSet.FieldByName('NAME').AsString);
end;
}

procedure Ttestsdfspecific.TestDelimitedTextOutput;
// Test if saving and loading data keeps the original values.

// Mainly check if writing & reading quotes works.
// to do: more fully test RFC4180
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
begin
  // with Schema, with Header line
  TestDataset.Close;
  TestDataset.AllowMultiLine := True;
  TestDataset.FirstLineAsSchema := True;
  TestDataset.FileName := TestFileName('delim.csv');
  Assign(F, TestDataset.FileName);
  Rewrite(F);
  Writeln(F,'Field1,Field2,Field3,Field4,Field5,Field6,Field7');
  Writeln(F,'"Delimiter,""and"";quote","J""T""",Just a long line,"Just a quoted long line","multi');
  Writeln(F,'line","Delimiter,and;done","Some ""random"" quotes"');
  Close(F);
  // Load our dataset
  TestDataset.Open;
//  AssertEquals('Field count',7,TEstDataset.Fielddefs.Count);
//  AssertEquals('Record count',1,TEstDataset.RecordCount);
  TestDataset.First;
  AssertEquals('Field1',Value1, TestDataSet.Fields[0].AsString);
  AssertEquals('Field2',Value2, TestDataSet.Fields[1].AsString);
  AssertEquals('Field3',Value3, TestDataSet.Fields[2].AsString);
  AssertEquals('Field4',Value4, TestDataSet.Fields[3].AsString);
  AssertEquals('Field5',Value5, TestDataSet.Fields[4].AsString);
  AssertEquals('Field6',Value6, TestDataSet.Fields[5].AsString);
  AssertEquals('Field7',Value7, TestDataSet.Fields[6].AsString);
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
  S : String;

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

  TestDataset.Open;
  AssertEquals('FieldDefs.Count',5,TestDataset.FieldDefs.Count);
  AssertEquals('RecordCount', 1, TestDataset.RecordCount);
  TestDataset.Edit;
  TestDataset.Fields[0].AsString:='Value1';
  TestDataset.Post;
  TestDataset.Close;

  Assign(F, TestDataset.FileName);
  Reset(F);
  ReadLn(F,S);
  Close(F);
  AssertEquals('No data lost','Value1;value2;;;',S);
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

initialization
  // Only run these tests if we are running
  // sdf tests. After all, running these when testing
  // e.g. SQL RDBMS doesn't make sense.
  if uppercase(dbconnectorname)='SDFDS' then
    begin
    Registertest(Ttestsdfspecific);
    end;
end.

