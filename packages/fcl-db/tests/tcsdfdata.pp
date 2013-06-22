unit tcsdfdata;
// Tests specific functionality of sdfdataset (multiline etc)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Fpcunit, Testutils, Testregistry, testdecorator,
  dateutils,sdfdata,ToolsUnit;

type

  { Ttestsdfspecific }

  Ttestsdfspecific = class(Ttestcase)
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
  end;

implementation

procedure Ttestsdfspecific.TestEmptyFileHeader;
// An empty file should return 0 records even if it has a header
const
  InputFilename='empty.csv';
begin
  TestDataSet.Close;

  if FileExists(InputFilename) then DeleteFile(InputFilename);
  TestDataset.FileMustExist:=false;
  TestDataset.FirstLineAsSchema := True;  
  TestDataset.FileName:=InputFilename;
  TestDataset.Open;

  TestDataset.Last;
  TestDataset.First;  
  AssertEquals('Number of records in test dataset', 0, TestDataset.RecordCount);
  TestDataset.Close;
end;

procedure Ttestsdfspecific.TestEmptyFileNoHeader;
// An empty file should return 0 records even if it has a header
const
  InputFilename='empty.csv';
begin
  TestDataSet.Close;

  if FileExists(InputFilename) then DeleteFile(InputFilename);
  TestDataset.FileMustExist:=false;
  TestDataset.FirstLineAsSchema := false;  
  TestDataset.FileName:=InputFilename;
  TestDataset.Open;

  TestDataset.Last;
  TestDataset.First;  
  AssertEquals('Number of records in test dataset', 0, TestDataset.RecordCount);
  TestDataset.Close;
end;

procedure Ttestsdfspecific.TestSingleLineHeader;
// A file with a single data line and header should return 1 records
const
  InputFilename='singleh.csv';
var
  FileStrings: TStringList;
begin
  TestDataSet.Close;

  if FileExists(InputFilename) then DeleteFile(InputFilename);
  FileStrings:=TStringList.Create;
  try
    FileStrings.Add('ID,NAME,BIRTHDAY');
    FileStrings.Add('1,SimpleName,31-12-1976');
    FileStrings.SaveToFile(InputFileName);
  finally
    FileStrings.Free;
  end;

  TestDataset.FileMustExist:=false;
  TestDataset.FirstLineAsSchema := true;
  TestDataset.FileName:=InputFilename;
  TestDataset.Open;

  TestDataset.Last;
  TestDataset.First;
  AssertEquals('Number of records in test dataset', 1, TestDataset.RecordCount);
  TestDataset.Close;
end;

procedure Ttestsdfspecific.TestSingleLineNoHeader;
// A file with a single data line, no header should return 1 records
const
  InputFilename='single.csv';
var
  FileStrings: TStringList;
begin
  TestDataSet.Close;

  if FileExists(InputFilename) then DeleteFile(InputFilename);
  FileStrings:=TStringList.Create;
  try
    FileStrings.Add('1,SimpleName,31-12-1976');
    FileStrings.SaveToFile(InputFileName);
  finally
    FileStrings.Free;
  end;

  TestDataset.FileMustExist:=false;
  TestDataset.FirstLineAsSchema := false;
  TestDataset.FileName:=InputFilename;
  TestDataset.Open;

  TestDataset.Last;
  TestDataset.First;
  AssertEquals('Number of records in test dataset', 1, TestDataset.RecordCount);
  TestDataset.Close;
end;

procedure Ttestsdfspecific.TestOutput;
// Basic assignment test: assign some difficult data to records and
// see if the recordcount is correct.
const
  OutputFilename='output.csv';
begin
  TestDataSet.Close;

  if FileExists(OutputFilename) then DeleteFile(OutputFileName);
  TestDataset.FileName:=OutputFileName;
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
  TestDataset.FieldByName('ID').AsInteger := 3;
  //Data with delimiter and quote (to test 19376)
  TestDataset.FieldByName('NAME').AsString := 'Delimiter,"and";quote';
  TestDataset.FieldByName('BIRTHDAY').AsDateTime := ScanDateTime('yyyymmdd', '19761231', 1);
  TestDataset.Post;


  TestDataset.Append;
  TestDataset.FieldByName('ID').AsInteger := 4;
  // Regular data
  TestDataset.FieldByName('NAME').AsString := 'Just a long line of text without anything special';
  TestDataset.FieldByName('BIRTHDAY').AsDateTime := ScanDateTime('yyyymmdd', '19761231', 1);
  TestDataset.Post;

  TestDataset.Last;
  TestDataset.First;
  // This fails - seems it sees the header as a record, too?
  AssertEquals('Number of records in test dataset', 4, TestDataset.RecordCount);
  TestDataset.Close;
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
  OutputFileName='delim.csv';
  Value1='Delimiter,"and";quote';
  Value2='J"T"';
  Value3='Just a long line';
  Value4='Just a quoted long line';
  Value5='multi'+#13+#10+'line';
  Value6='Delimiter,and;done';
  Value7='Some "random" quotes';
var
  FileStrings: TStringList;
  OneRecord: TStringList;
begin
  TestDataset.Close;
  TestDataset.AllowMultiLine:=true;
  if FileExists(OutputFileName) then DeleteFile(OutputFileName);
  FileStrings:=TStringList.Create;
  OneRecord:=TStringList.Create;
  try
    FileStrings.Add('Field1,Field2,Field3,Field4,Field5,Field6,Field7');
    OneRecord.Add(Value1);
    OneRecord.Add(Value2);
    OneRecord.Add(Value3);
    OneRecord.Add(Value4);
    OneRecord.Add(Value5);
    OneRecord.Add(Value6);
    OneRecord.Add(Value7);
    OneRecord.Delimiter:=',';
    OneRecord.QuoteChar:='"';
    OneRecord.StrictDelimiter:=true;
    FileStrings.Add(OneRecord.DelimitedText);
    FileStrings.SaveToFile(OutputFileName);
  finally
    FileStrings.Free;
    OneRecord.Free;
  end;

  // Load our dataset
  TestDataset.FileName:=OutputFileName;
  TestDataset.Open;
  TestDataset.First;
  AssertEquals(Value1, TestDataSet.Fields[0].AsString);
  AssertEquals(Value2, TestDataSet.Fields[1].AsString);
  AssertEquals(Value3, TestDataSet.Fields[2].AsString);
  AssertEquals(Value4, TestDataSet.Fields[3].AsString);
  AssertEquals(Value5, TestDataSet.Fields[4].AsString);
  AssertEquals(Value6, TestDataSet.Fields[5].AsString);
  AssertEquals(Value7, TestDataSet.Fields[6].AsString);
end;


procedure Ttestsdfspecific.Setup;

begin
  TestDataset := TSDFDataset.Create(nil);
  TestDataset.Delimiter := ',';
  TestDataset.FileMustExist:=false;
  TestDataset.FirstLineAsSchema := True;
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

