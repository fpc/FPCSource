unit tccsvdataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, db, SysUtils, fpcunit, testutils, testregistry, csvdataset;

type

  { TTestCSVDataset }

  TTestCSVDataset= class(TTestCase)
  private
    FCSVDataset: TCSVDataset;
    // Load CSVDataset from CSV stream containing lines
    Procedure LoadFromLines(Const Lines: Array of string);
    // Save CSVDataset to CSV stream, transform to lines
    Procedure SaveToLines(Const Lines: TStrings);
    // Save CSVDataset to CSV stream, transform to lines, compare with given lines
    Procedure AssertLines(Const Lines: Array of string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Property CSVDataset : TCSVDataset Read FCSVDataset;
  published
    procedure TestEmpty;
    procedure TestDefaults;
    Procedure TestLoadEmptyDefault;
    Procedure TestLoadEmptyFirstLineAsNames;
    Procedure TestLoad2fieldsFirstLineAsNames;
    Procedure TestLoad2fields;
    Procedure TestLoad2Records2fields;
    Procedure TestSaveEmptyDefault;
    Procedure TestSaveEmptyFirstLineAsNames;
    Procedure TestSaveOneRecordDefault;
    Procedure TestSaveOneRecordFirstLineAsNames;
    Procedure TestSaveTwoRecordsDefault;
    Procedure TestSaveTwoRecordsFirstLineAsNames;
    Procedure TestSaveOneRecord2FieldsDefault;
    Procedure TestSaveOneRecord2FieldsFirstLineAsNames;
    Procedure TestLoadPriorFieldDefs;
    Procedure TestLoadPriorFieldDefsNoFieldNames;
    Procedure TestLoadPriorFieldDefsNoFieldNamesWrongCount;
    Procedure TestLoadPriorFieldDefsFieldNamesWrongCount;
    Procedure TestLoadPriorFieldDefsFieldNamesWrongNames;
  end;

implementation

procedure TTestCSVDataset.TestEmpty;
begin
  AssertNotNull('Have CSV dataset',CSVDataset);
  AssertFalse('Not open',CSVDataset.Active);
  AssertEquals('No fielddefs',0,CSVDataset.FieldDefs.Count);
  AssertEquals('Name','DS',CSVDataset.Name);
end;

procedure TTestCSVDataset.TestDefaults;
begin
  With CSVDataset.CSVOptions do
    begin
    AssertEquals('DefaultFieldLength',255,DefaultFieldLength);
    AssertEquals('FirstLineAsFieldNames',False,FirstLineAsFieldNames);
    AssertEquals('Delimiter',',',Delimiter);
    AssertEquals('QuoteChar','"',QuoteChar);
    AssertEquals('LineEnding',sLineBreak,LineEnding);
    AssertEquals('IgnoreOuterWhitespace',False,IgnoreOuterWhitespace);
    AssertEquals('QuoteOuterWhitespace',True,QuoteOuterWhitespace);
    AssertEquals('EqualColCountPerRow',True,EqualColCountPerRow);
    end;
end;

Procedure TTestCSVDataset.LoadFromLines(Const Lines : Array of string);

Var
  L : TStringList;
  s : TStream;
begin
  S:=Nil;
  L:=TStringList.Create;
  try
    L.AddStrings(Lines);
    S:=TStringStream.Create(L.Text);
    CSVDataset.LoadFromCSVStream(S);
  finally
    S.Free;
    L.Free;
  end;
end;

Procedure TTestCSVDataset.SaveToLines(Const Lines: TStrings);

Var
  S : TStringStream;

begin
  S:=TStringStream.Create('');
  try
    CSVDataset.SaveToCSVStream(S);
    Lines.Text:=S.DataString;
    {
    Writeln('----');
    Writeln(S.DataString);
    Writeln('----');
    }
  finally
    S.Free;
  end;
end;

Procedure TTestCSVDataset.AssertLines(Const Lines: Array of string);

Var
  L : TStrings;
  I : Integer;
begin
  L:=TStringList.Create;
  try
    SaveToLines(L);
    AssertEquals('Number of lines',Length(Lines),L.Count);
    For I:=0 to L.Count-1 do
      AssertEquals('Correct line '+IntToStr(0),Lines[I],L[i]);
  finally
    L.Free;
  end;
end;

Procedure TTestCSVDataset.TestLoadEmptyDefault;
begin
  LoadFromLines(['a']);
  AssertEquals('Active',True,CSVDataset.Active);
  AssertEquals('field count',1,CSVDataset.FieldDefs.Count);
  AssertEquals('field name','Column1',CSVDataset.FieldDefs[0].Name);
  AssertEquals('field size',CSVDataset.CSVOptions.DefaultFieldLength,CSVDataset.FieldDefs[0].Size);
  AssertEquals('Not Empty',False,CSVDataset.EOF and CSVDataset.BOF);
  AssertEquals('field contents','a',CSVDataset.Fields[0].AsString);
end;

Procedure TTestCSVDataset.TestLoadEmptyFirstLineAsNames;

begin
  CSVDataset.CSVOptions.FirstLineAsFieldNames:=True;
  CSVDataset.CSVOptions.DefaultFieldLength:=128;
  LoadFromLines(['a']);
  AssertEquals('Active',True,CSVDataset.Active);
  AssertEquals('field count',1,CSVDataset.FieldDefs.Count);
  AssertEquals('field name','a',CSVDataset.FieldDefs[0].Name);
  AssertEquals('field size',CSVDataset.CSVOptions.DefaultFieldLength,CSVDataset.FieldDefs[0].Size);
  AssertEquals('Empty',True,CSVDataset.EOF and CSVDataset.BOF);
end;

Procedure TTestCSVDataset.TestLoad2fieldsFirstLineAsNames;
begin
  CSVDataset.CSVOptions.FirstLineAsFieldNames:=True;
  CSVDataset.CSVOptions.DefaultFieldLength:=128;
  LoadFromLines(['a,b']);
  AssertEquals('Active',True,CSVDataset.Active);
  AssertEquals('field count',2,CSVDataset.FieldDefs.Count);
  AssertEquals('field 0 name','a',CSVDataset.FieldDefs[0].Name);
  AssertEquals('field 0 size',CSVDataset.CSVOptions.DefaultFieldLength,CSVDataset.FieldDefs[0].Size);
  AssertEquals('field 1 name','b',CSVDataset.FieldDefs[1].Name);
  AssertEquals('field 1 size',CSVDataset.CSVOptions.DefaultFieldLength,CSVDataset.FieldDefs[1].Size);
  AssertEquals('Empty',True,CSVDataset.EOF and CSVDataset.BOF);
end;

Procedure TTestCSVDataset.TestLoad2fields;

begin
  CSVDataset.CSVOptions.DefaultFieldLength:=128;
  LoadFromLines(['a,b']);
  AssertEquals('Active',True,CSVDataset.Active);
  AssertEquals('field count',2,CSVDataset.FieldDefs.Count);
  AssertEquals('field 0 name','Column1',CSVDataset.FieldDefs[0].Name);
  AssertEquals('field 0 size',CSVDataset.CSVOptions.DefaultFieldLength,CSVDataset.FieldDefs[0].Size);
  AssertEquals('field 1 name','Column2',CSVDataset.FieldDefs[1].Name);
  AssertEquals('field 1 size',CSVDataset.CSVOptions.DefaultFieldLength,CSVDataset.FieldDefs[1].Size);
  AssertEquals('Empty',False,CSVDataset.EOF and CSVDataset.BOF);
  AssertEquals('Not Empty',False,CSVDataset.EOF and CSVDataset.BOF);
  AssertEquals('field 0 contents','a',CSVDataset.Fields[0].AsString);
  AssertEquals('field 1 contents','b',CSVDataset.Fields[1].AsString);
end;

Procedure TTestCSVDataset.TestLoad2Records2fields;
begin
  CSVDataset.CSVOptions.DefaultFieldLength:=128;
  LoadFromLines(['a,b','c,d']);
  AssertEquals('Active',True,CSVDataset.Active);
  AssertEquals('field count',2,CSVDataset.FieldDefs.Count);
  AssertEquals('field 0 name','Column1',CSVDataset.FieldDefs[0].Name);
  AssertEquals('field 0 size',CSVDataset.CSVOptions.DefaultFieldLength,CSVDataset.FieldDefs[0].Size);
  AssertEquals('field 1 name','Column2',CSVDataset.FieldDefs[1].Name);
  AssertEquals('field 1 size',CSVDataset.CSVOptions.DefaultFieldLength,CSVDataset.FieldDefs[1].Size);
  AssertEquals('Empty',False,CSVDataset.EOF and CSVDataset.BOF);
  AssertEquals('Not Empty',False,CSVDataset.EOF and CSVDataset.BOF);
  AssertEquals('field 0 contents','a',CSVDataset.Fields[0].AsString);
  AssertEquals('field 1 contents','b',CSVDataset.Fields[1].AsString);
  CSVDataset.Next;
  AssertEquals('not At EOF',False,CSVDataset.EOF);
  AssertEquals('field 0 contents','c',CSVDataset.Fields[0].AsString);
  AssertEquals('field 1 contents','d',CSVDataset.Fields[1].AsString);
  CSVDataset.Next;
  AssertEquals('At EOF',True,CSVDataset.EOF);
end;

Procedure TTestCSVDataset.TestSaveEmptyDefault;
begin
  CSVDataset.FieldDefs.Add('a',ftString);
  CSVDataset.CreateDataset;
  AssertLines([]);
end;

Procedure TTestCSVDataset.TestSaveEmptyFirstLineAsNames;
begin
  CSVDataset.CSVOptions.FirstLineAsFieldNames:=True;
  CSVDataset.FieldDefs.Add('a',ftString);
  CSVDataset.CreateDataset;
  AssertLines(['a']);
end;

Procedure TTestCSVDataset.TestSaveOneRecordDefault;
begin
//  CSVDataset.CSVOptions.FirstLineAsFieldNames:=True;
  CSVDataset.FieldDefs.Add('a',ftString,20);
  CSVDataset.CreateDataset;
  CSVDataset.Append;
  CSVDataset.Fields[0].AsString:='b';
  CSVDataset.Post;
  AssertLines(['b']);
end;

Procedure TTestCSVDataset.TestSaveOneRecordFirstLineAsNames;
begin
  CSVDataset.CSVOptions.FirstLineAsFieldNames:=True;
  CSVDataset.FieldDefs.Add('a',ftString,20);
  CSVDataset.CreateDataset;
  CSVDataset.Append;
  CSVDataset.Fields[0].AsString:='b';
  CSVDataset.Post;
  AssertLines(['a','b']);
end;

Procedure TTestCSVDataset.TestSaveTwoRecordsDefault;
begin
  CSVDataset.FieldDefs.Add('a',ftString,20);
  CSVDataset.CreateDataset;
  CSVDataset.Append;
  CSVDataset.Fields[0].AsString:='b';
  CSVDataset.Post;
  CSVDataset.Append;
  CSVDataset.Fields[0].AsString:='c';
  CSVDataset.Post;
  AssertLines(['b','c']);
end;

Procedure TTestCSVDataset.TestSaveTwoRecordsFirstLineAsNames;
begin
  CSVDataset.CSVOptions.FirstLineAsFieldNames:=True;
  CSVDataset.FieldDefs.Add('a',ftString,20);
  CSVDataset.CreateDataset;
  CSVDataset.Append;
  CSVDataset.Fields[0].AsString:='b';
  CSVDataset.Post;
  CSVDataset.Append;
  CSVDataset.Fields[0].AsString:='c';
  CSVDataset.Post;
  AssertLines(['a','b','c']);
end;

Procedure TTestCSVDataset.TestSaveOneRecord2FieldsDefault;
begin
  CSVDataset.FieldDefs.Add('a',ftString,20);
  CSVDataset.FieldDefs.Add('b',ftString,20);
  CSVDataset.CreateDataset;
  CSVDataset.Append;
  CSVDataset.Fields[0].AsString:='c';
  CSVDataset.Fields[1].AsString:='d';
  CSVDataset.Post;
  AssertLines(['c,d']);
end;

Procedure TTestCSVDataset.TestSaveOneRecord2FieldsFirstLineAsNames;
begin
  CSVDataset.CSVOptions.FirstLineAsFieldNames:=True;
  CSVDataset.FieldDefs.Add('a',ftString,20);
  CSVDataset.FieldDefs.Add('b',ftString,20);
  CSVDataset.CreateDataset;
  CSVDataset.Append;
  CSVDataset.Fields[0].AsString:='c';
  CSVDataset.Fields[1].AsString:='d';
  CSVDataset.Post;
  AssertLines(['a,b','c,d']);
end;

Procedure TTestCSVDataset.TestLoadPriorFieldDefs;
begin
  CSVDataset.CSVOptions.FirstLineAsFieldNames:=True;
  CSVDataset.FieldDefs.Add('a',ftString,20);
  CSVDataset.FieldDefs.Add('b',ftInteger,4);
  LoadFromLines(['a,b','1,2']);
  AssertEquals('field count',2,CSVDataset.FieldDefs.Count);
  AssertEquals('field 0 name','a',CSVDataset.FieldDefs[0].Name);
  AssertEquals('field 0 size',20,CSVDataset.FieldDefs[0].Size);
  AssertEquals('field 1 name','b',CSVDataset.FieldDefs[1].Name);
  AssertEquals('field 1 size',4,CSVDataset.FieldDefs[1].Size);
  AssertEquals('field 1 typee',Ord(ftInteger),Ord(CSVDataset.FieldDefs[1].DataType));
  AssertEquals('Not Empty',False,CSVDataset.EOF and CSVDataset.BOF);
  AssertEquals('field 0 contents','1',CSVDataset.Fields[0].AsString);
  AssertEquals('field 1 contents',2,CSVDataset.Fields[1].AsInteger);
end;

Procedure TTestCSVDataset.TestLoadPriorFieldDefsNoFieldNames;
begin
  CSVDataset.FieldDefs.Add('a',ftString,20);
  CSVDataset.FieldDefs.Add('b',ftInteger,4);
  LoadFromLines(['1,2']);
  AssertEquals('field count',2,CSVDataset.FieldDefs.Count);
  AssertEquals('field 0 name','a',CSVDataset.FieldDefs[0].Name);
  AssertEquals('field 0 size',20,CSVDataset.FieldDefs[0].Size);
  AssertEquals('field 1 name','b',CSVDataset.FieldDefs[1].Name);
  AssertEquals('field 1 size',4,CSVDataset.FieldDefs[1].Size);
  AssertEquals('field 1 typee',Ord(ftInteger),Ord(CSVDataset.FieldDefs[1].DataType));
  AssertEquals('Not Empty',False,CSVDataset.EOF and CSVDataset.BOF);
  AssertEquals('field 0 contents','1',CSVDataset.Fields[0].AsString);
  AssertEquals('field 1 contents',2,CSVDataset.Fields[1].AsInteger);
end;

Procedure TTestCSVDataset.TestLoadPriorFieldDefsNoFieldNamesWrongCount;

Var
  OK : Boolean;
begin
  CSVDataset.FieldDefs.Add('a',ftString,20);
  CSVDataset.FieldDefs.Add('b',ftInteger,4);
  try
    LoadFromLines(['1']);
    OK:=False;
  except
    OK:=true;
  end;
  if not OK then
    Fail('Expected exception, but none raised');
end;

Procedure TTestCSVDataset.TestLoadPriorFieldDefsFieldNamesWrongCount;

const
  EM = 'DS : CSV File Field count (1) does not match dataset field count (2).';
Var
  OK : String;
begin
  CSVDataset.CSVOptions.FirstLineAsFieldNames:=True;
  CSVDataset.FieldDefs.Add('a',ftString,20);
  CSVDataset.FieldDefs.Add('b',ftInteger,4);
  try
    LoadFromLines(['A']);
    OK:='Expected exception, but none raised';
  except
    on E : Exception do
      if  (E.Message<>EM) then
        OK:=ComparisonMsg(EM,E.Message);
  end;
  if (OK<>'') then
    Fail(OK);
end;

Procedure TTestCSVDataset.TestLoadPriorFieldDefsFieldNamesWrongNames;
const
  EM = 'DS : CSV File field 1: name "c" does not match dataset field name "b".';
Var
  OK : String;
begin
  CSVDataset.CSVOptions.FirstLineAsFieldNames:=True;
  CSVDataset.FieldDefs.Add('a',ftString,20);
  CSVDataset.FieldDefs.Add('b',ftInteger,4);
  try
    LoadFromLines(['a,c']);
    OK:='No exception raised';
  except
    on E : Exception do
      if  (E.Message<>EM) then
        OK:=ComparisonMsg(EM,E.Message)
  end;
  if (OK<>'') then
    Fail(OK);
end;

procedure TTestCSVDataset.SetUp;
begin
  FCSVDataset:=TCSVDataset.Create(Nil);
  FCSVDataset.Name:='DS';
end;

procedure TTestCSVDataset.TearDown;
begin
  FreeAndNil(FCSVDataset);
end;

Initialization

  RegisterTest(TTestCSVDataset);
end.

