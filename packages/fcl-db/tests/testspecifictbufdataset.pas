unit TestSpecificTBufDataset;

{
  Unit tests which are specific to stand-alone TBufDataset-datasets. (So not
  for derived datasets like TQuery )
}

{$IFDEF FPC}
  {$mode Delphi}{$H+}
{$ENDIF}

interface

uses
{$IFDEF FPC}
  fpcunit, testregistry, BufDataset,
{$ELSE FPC}
  TestFramework,
{$ENDIF FPC}
  Classes, SysUtils, db, ToolsUnit;

type

  { TTestSpecificTBufDataset }

  { TMyBufDataset }

  TMyBufDataset = Class(TBufDataset)
  protected
    procedure LoadBlobIntoBuffer(FieldDef: TFieldDef; ABlobBuf: PBufBlobField); override;
  end;


  TTestSpecificTBufDataset = class(TDBBasicsTestCase)
  private
    FAfterScrollCount:integer;
    FBeforeScrollCount:integer;
    FDataset : TBufDataset;
    procedure DoAfterScrollCount(DataSet: TDataSet);
    procedure DoBeforeScrollCount(DataSet: TDataSet);
    procedure TestDataset(ABufDataset: TBufDataset; AutoInc: boolean = false);
    function GetAutoIncDataset: TBufDataset;
    procedure IntTestAutoIncFieldStreaming(XML: boolean);
    // Returns a dataset with FieldDefs and persistent fields for ID and NAME, not yet linked to each other.
    function GetDefsAndFieldsDataset: TBufDataset;
    // Returns an open dataset with a UTF-8 ftString(5) field NAME holding one Cyrillic word.
    function GetCodePageDataset: TBufDataset;
    procedure IntTestCodePageStreaming(aFormat: TDataPacketFormat);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CreateDatasetFromFielddefs;
    procedure CreateDatasetFromFields;
    // Opens a dataset that has both FieldDefs and persistent fields, once with Open and once with CreateDataset.
    procedure CreateDatasetFromFielddefsAndFields;
    procedure TestOpeningNonExistingDataset;
    procedure TestCreationDatasetWithCalcFields;
    procedure TestAutoIncField;
    procedure TestAutoIncFieldStreaming;
    procedure TestAutoIncFieldStreamingXML;
    Procedure TestLocateScrollEventCount;
    Procedure TestLookupScrollEventCount;
    procedure TestLookupEmpty;
    Procedure TestRecordCount;
    Procedure TestClear;
    procedure TestCopyFromDataset; //is copied dataset identical to original?
    procedure TestCopyFromDatasetMoved; //move record then copy. Is copy identical? Has record position changed?
    procedure TestNoRefreshForInMemory; // Test that refresh with in-memory data raises an error.
    procedure TestRefreshWithIndexOK; // Test that refresh works when indexes are defined
    procedure TestCancelUpdatesOnRefresh; // Test that refresh works when changes are present and cancelupdatesonrefresh is true.
    procedure TestCodePageFieldDefClass;
    procedure TestCodePageValue;
    procedure TestCodePageTruncate;
    procedure TestCodePageStreamingBIN;
    procedure TestCodePageStreamingXML;
    procedure TestCodePageStreamVersion;
    procedure TestCodePageComponentStreaming;
    // IndexDefs.Add on an open dataset, then select the index, append and close (bug 37915)
    procedure TestIndexDefsAddWhileOpen;
    // IndexDefs.Add on an open dataset beyond MaxIndexesCount
    procedure TestIndexDefsAddWhileOpenMaxIndexes;
    // Case-insensitive filters on non-ASCII strings, also with DefaultSystemCodePage CP_UTF8 (bug 36512)
    procedure TestFilterCaseInsensitiveNonASCII;
    // IndexFieldNames with DESC fields, assigned repeatedly and alternated (bug 38884)
    procedure TestIndexFieldNamesDescRepeated;
  end;

implementation

uses
{$ifdef fpc}
//
{$endif fpc}
  variants,
  FmtBCD;

{ TMyBufDataset }

procedure TMyBufDataset.LoadBlobIntoBuffer(FieldDef: TFieldDef; ABlobBuf: PBufBlobField);
begin
  Raise ENotImplemented.Create('LoadBlobIntoBuffer not implemented');
end;

{ TTestSpecificTBufDataset }

procedure TTestSpecificTBufDataset.TestDataset(ABufDataset: TBufDataset;
  AutoInc: boolean);
var
  i  : integer;
begin
  for i := 1 to 10 do
    begin
    ABufDataset.Append;
    if not AutoInc then
      ABufDataset.FieldByName('ID').AsInteger := i;
    ABufDataset.FieldByName('NAME').AsString := 'TestName' + inttostr(i);
    ABufDataset.Post;
    end;
  ABufDataset.first;
  for i := 1 to 10 do
    begin
    CheckEquals(i,ABufDataset.fieldbyname('ID').asinteger);
    CheckEquals('TestName' + inttostr(i),ABufDataset.fieldbyname('NAME').AsString);
    ABufDataset.next;
    end;
  CheckTrue(ABufDataset.EOF);
end;

procedure TTestSpecificTBufDataset.DoAfterScrollCount(DataSet: TDataSet);
begin
  Inc(FAfterScrollCount);
end;

procedure TTestSpecificTBufDataset.DoBeforeScrollCount(DataSet: TDataSet);
begin
  Inc(FBeforeScrollCount);
end;

function TTestSpecificTBufDataset.GetAutoIncDataset: TBufDataset;
var
  ds : TBufDataset;
  f: TField;
begin
  ds := TMyBufDataset.Create(nil);
  F := TAutoIncField.Create(ds);
  F.FieldName:='ID';
  F.DataSet:=ds;
  F := TStringField.Create(ds);
  F.FieldName:='NAME';
  F.DataSet:=ds;
  F.Size:=50;
  DS.CreateDataset;

  TestDataset(ds,True);
  result := ds;
end;

procedure TTestSpecificTBufDataset.IntTestAutoIncFieldStreaming(XML: boolean);
var
  ds : TBufDataset;
  fn: string;
begin
  ds := GetAutoIncDataset;
  fn := GetTempFileName;
  if xml then
    ds.SaveToFile(fn,dfXML)
  else
    ds.SaveToFile(fn);
  DS.Close;
  ds.Free;

  ds := TMyBufDataset.Create(nil);
  ds.LoadFromFile(fn);
  ds.Last;
  CheckEquals(10,ds.FieldByName('Id').AsInteger);
  ds.Append;
  ds.FieldByName('NAME').asstring := 'Test';
  ds.Post;
  CheckEquals(11,ds.FieldByName('Id').AsInteger);
  ds.Free;

  DeleteFile(fn);
end;

procedure TTestSpecificTBufDataset.SetUp;
begin
  FAfterScrollCount:=0;
  FBeforeScrollCount:=0;
  DBConnector.StartTest(TestName);
end;

procedure TTestSpecificTBufDataset.TearDown;
begin
  DBConnector.StopTest(TestName);
end;

procedure TTestSpecificTBufDataset.CreateDatasetFromFielddefs;
var ds : TBufDataset;
begin
  ds := TMyBufDataset.Create(nil);
  DS.FieldDefs.Add('ID',ftInteger);
  DS.FieldDefs.Add('NAME',ftString,50);
  DS.CreateDataset;
  TestDataset(ds);
  DS.Close;
  DS.CreateDataset;
  TestDataset(ds);
end;

procedure TTestSpecificTBufDataset.CreateDatasetFromFields;
var ds : TBufDataset;
    f: TField;
begin
  ds := TMyBufDataset.Create(nil);
  F := TIntegerField.Create(ds);
  F.FieldName:='ID';
  F.DataSet:=ds;
  F := TStringField.Create(ds);
  F.FieldName:='NAME';
  F.DataSet:=ds;
  F.Size:=50;
  DS.CreateDataset;
  TestDataset(ds);
  DS.Close;
  DS.CreateDataset;
  TestDataset(ds);
end;

function TTestSpecificTBufDataset.GetDefsAndFieldsDataset: TBufDataset;

var
  lField: TField;

begin
  Result := TMyBufDataset.Create(nil);
  Result.FieldDefs.Add('ID', ftInteger);
  Result.FieldDefs.Add('NAME', ftString, 50);
  lField := TIntegerField.Create(Result);
  lField.FieldName := 'ID';
  lField.DataSet := Result;
  lField := TStringField.Create(Result);
  lField.FieldName := 'NAME';
  lField.Size := 50;
  lField.DataSet := Result;
end;


procedure TTestSpecificTBufDataset.CreateDatasetFromFielddefsAndFields;

// Test for issue 41931

var
  lDS: TBufDataset;
  lUseCreate: Boolean;

begin
  for lUseCreate := False to True do
    begin
    lDS := GetDefsAndFieldsDataset;
    try
      if lUseCreate then
        lDS.CreateDataset
      else
        lDS.Open;
      AssertEquals('ID field number, CreateDataset=' + BoolToStr(lUseCreate, True), 1, lDS.FieldByName('ID').FieldNo);
      AssertEquals('NAME field number, CreateDataset=' + BoolToStr(lUseCreate, True), 2, lDS.FieldByName('NAME').FieldNo);
      TestDataset(lDS);
      lDS.Close;
    finally
      lDS.Free;
    end;
    end;
end;


procedure TTestSpecificTBufDataset.TestOpeningNonExistingDataset;
var ds : TBufDataset;
    f: TField;
begin
  ds := TMyBufDataset.Create(nil);
  F := TIntegerField.Create(ds);
  F.FieldName:='ID';
  F.DataSet:=ds;

  CheckException(ds.Open,EDatabaseError);
  ds.Free;

  ds := TMyBufDataset.Create(nil);
  DS.FieldDefs.Add('ID',ftInteger);

  CheckException(ds.Open,EDatabaseError);
  ds.Free;
end;

procedure TTestSpecificTBufDataset.TestCreationDatasetWithCalcFields;
var ds : TBufDataset;
    f: TField;
    i: integer;
begin
  ds := TMyBufDataset.Create(nil);
  try
    F := TIntegerField.Create(ds);
    F.FieldName:='ID';
    F.Required:=True;
    F.DataSet:=ds;
    F := TStringField.Create(ds);
    F.FieldName:='NAME';
    F.Required:=False;
    F.DataSet:=ds;
    F.Size:=50;

    F := TStringField.Create(ds);
    F.FieldKind:=fkCalculated;
    F.FieldName:='NAME_CALC';
    F.DataSet:=ds;
    F.Size:=50;

    F := TStringField.Create(ds);
    F.FieldKind:=fkLookup;
    F.FieldName:='NAME_LKP';
    F.LookupDataSet:=DBConnector.GetNDataset(5);
    F.KeyFields:='ID';
    F.LookupKeyFields:='ID';
    F.LookupResultField:='NAME';
    F.DataSet:=ds;
    F.Size:=50;

    DS.CreateDataset;

    TestDataset(ds);

    CheckTrue(ds.FieldDefs[0].Required, 'Required');
    CheckFalse(ds.FieldDefs[1].Required, 'not Required');
    for i := 0 to ds.FieldDefs.Count-1 do
      begin
      CheckNotEquals(ds.FieldDefs[i].Name,'NAME_CALC');
      CheckNotEquals(ds.FieldDefs[i].Name,'NAME_LKP');
      end;
    DS.Close;
  finally
    ds.Free;
  end;
end;

procedure TTestSpecificTBufDataset.TestAutoIncField;
var
  ds : TBufDataset;
begin
  ds := GetAutoIncDataset;
  DS.Close;
  ds.Free;
end;

procedure TTestSpecificTBufDataset.TestAutoIncFieldStreaming;
begin
  IntTestAutoIncFieldStreaming(false);
end;

procedure TTestSpecificTBufDataset.TestAutoIncFieldStreamingXML;
begin
  IntTestAutoIncFieldStreaming(true);
end;

procedure TTestSpecificTBufDataset.TestLocateScrollEventCount;

begin
  with DBConnector.GetNDataset(10) as TBufDataset do
    begin
    Open;
    AfterScroll:=DoAfterScrollCount;
    BeforeScroll:=DoBeforeScrollCount;
    Locate('ID',5,[]);
    AssertEquals('Current record OK',5,FieldByName('ID').AsInteger);
    AssertEquals('After scroll count',1,FAfterScrollCount);
    AssertEquals('After scroll count',1,FBeforeScrollCount);
    end;
end;


procedure TTestSpecificTBufDataset.TestLookupEmpty;

// Test for issue 36086

Var
  V : Variant;

begin
  with DBConnector.GetNDataset(0) as TBufDataset do
    begin
    Open;
    V:=Lookup('ID',5,'NAME');
    AssertTrue('Null',Null=V);
    end;
end;

procedure TTestSpecificTBufDataset.TestLookupScrollEventCount;

Var
  V : Variant;
  S : String;
  ID : Integer;

begin
  with DBConnector.GetNDataset(10) as TBufDataset do
    begin
    Open;
    ID:=FieldByName('ID').AsInteger;
    AfterScroll:=DoAfterScrollCount;
    BeforeScroll:=DoBeforeScrollCount;
    V:=Lookup('ID',5,'NAME');
    AssertTrue('Not null',Null<>V);
    S:=V;
    AssertEquals('Result','TestName5',S);
    AssertEquals('After scroll count',0,FAfterScrollCount);
    AssertEquals('After scroll count',0,FBeforeScrollCount);
    AssertEquals('Current record unchanged',ID,FieldByName('ID').AsInteger);
    end;
end;

procedure TTestSpecificTBufDataset.TestRecordCount;
var
  BDS:TBufDataSet;

begin
  BDS:=TMyBufDataset.Create(nil);
  BDS.FieldDefs.Add('ID',ftLargeint);
  BDS.CreateDataSet;
  BDS.AppendRecord([1]);
  BDS.AppendRecord([2]);
  BDS.AppendRecord([3]);
  BDS.Close;
  AssertEquals('IsEmpty: ',True,BDS.IsEmpty);
  AssertEquals('RecordCount: ',0,BDS.RecordCount);
end;

procedure TTestSpecificTBufDataset.TestClear;

const
  testValuesCount=3;
var
  i: integer;
begin
  with DBConnector.GetNDataset(10) as TBufDataset do
    begin
    Open;
    Clear;
    AssertTrue('Dataset Closed',Not Active);
    AssertEquals('No fields',0,Fields.Count);
    AssertEquals('No fielddefs',0,FieldDefs.Count);
    // test after FieldDefs are Cleared, if internal structures are updated properly
    // create other FieldDefs
    FieldDefs.Add('Fs', ftString, 20);
    FieldDefs.Add('Fi', ftInteger);
    FieldDefs.Add('Fi2', ftInteger);
    // use only Open without CreateTable
    CreateDataset;
    AssertTrue('Empty dataset',IsEmpty);
    // add some data
    for i:=1 to testValuesCount do
      AppendRecord([TestStringValues[i], TestIntValues[i], TestIntValues[i]]);
    // check data
    AssertEquals('Record count',testValuesCount, RecordCount);
    First;
    for i:=1 to testValuesCount do
    begin
      AssertEquals('Field FS, Record '+InttoStr(i),TestStringValues[i], FieldByName('Fs').AsString);
      AssertEquals('Field Fi2, Record '+InttoStr(i),TestIntValues[i], FieldByName('Fi2').AsInteger);
      Next;
    end;
    CheckTrue(Eof);
  end;
end;

procedure TTestSpecificTBufDataset.TestCopyFromDataset;
var bufds1, bufds2: TBufDataset;
begin
  bufds1:=DBConnector.GetFieldDataset as TBufDataset;
  bufds2:=DBConnector.GetNDataset(0) as TBufDataset;

  bufds1.Open;
  bufds2.CopyFromDataset(bufds1);
  CheckFieldDatasetValues(bufds2);
end;

procedure TTestSpecificTBufDataset.TestCopyFromDatasetMoved;
var
  bufds1, bufds2: TBufDataset;
  CurrentID,NewID: integer;
begin
  bufds1:=DBConnector.GetFieldDataset as TBufDataset;
  bufds2:=DBConnector.GetNDataset(0) as TBufDataset;

  bufds1.Open;
  bufds1.Next; //this should not influence the copydataset step.
  CurrentID:=bufds1.FieldByName('ID').AsInteger;
  bufds2.CopyFromDataset(bufds1);
  CheckFieldDatasetValues(bufds2);
  NewID:=bufds1.FieldByName('ID').AsInteger;
  AssertEquals('Mismatch between ID field contents - the record has moved.',CurrentID,NewID);
end;

procedure TTestSpecificTBufDataset.TestRefreshWithIndexOK;

var
  Dataset: TBufDataset;
  FN : String;

begin
  Dataset := TBufDataset.Create(nil);
  try
    FN:=GetTempFileName;
    if FileExists(GetTempFileName) then
      AssertTrue('Delete existing db file',DeleteFile(FN));
    Dataset.FileName := FN;
    Dataset.FieldDefs.Add('LastName', ftString, 20);
    Dataset.FieldDefs.Add('FirstName', ftString, 20);
    Dataset.FieldDefs.Add('ID', ftString, 4);
    Dataset.CreateDataset;

    Dataset.Open;
    Dataset.AppendRecord(['Jenkins', 'John', '0003']);
    Dataset.AppendRecord(['Brooks', 'Jenny', '0001']);
    Dataset.AppendRecord(['Adams', 'Paul', '0002']);
    Dataset.Close;

    Dataset.Open;
    Dataset.IndexFieldNames := 'ID';
    Dataset.CancelChangesOnRefresh:=True;
    AssertNoException('Refresh OK',Dataset.Refresh);
  finally
    Dataset.Close;
    Dataset.Free;
  end;
end;

procedure  TTestSpecificTBufDataset.TestCancelUpdatesOnRefresh;
var
  Dataset: TBufDataset;
  FN : String;

begin
  Dataset := TBufDataset.Create(nil);
  try
    FN:=GetTempFileName;
    if FileExists(GetTempFileName) then
      AssertTrue('Delete existing db file',DeleteFile(FN));
    Dataset.FileName := FN;
    Dataset.FieldDefs.Clear;
    Dataset.FieldDefs.Add('LastName', ftString, 20);
    Dataset.FieldDefs.Add('FirstName', ftString, 20);
    Dataset.FieldDefs.Add('ID', ftString, 4);
    Dataset.CreateDataset;
    Dataset.Open;
    Dataset.IndexFieldNames := 'ID';
    Dataset.AppendRecord(['Jenkins', 'John', '0003']);
    Dataset.AppendRecord(['Brooks', 'Jenny', '0001']);
    Dataset.AppendRecord(['Adams', 'Paul', '0002']);
    Dataset.Close;
    Dataset.Open;
    Dataset.AppendRecord(['Jenkins2', 'John2', '0004']);

    AssertException('Refresh raises error',EDatabaseError,Dataset.Refresh);
    Dataset.CancelChangesOnRefresh:=True;
    AssertNoException('Refresh raises no error if CancelChangesOnRefresh is set',Dataset.Refresh);
  finally
    Dataset.Close;
    Dataset.Free;
  end;
end;

procedure TTestSpecificTBufDataset.TestNoRefreshForInMemory;
var
  Dataset: TBufDataset;
begin
  Dataset := TBufDataset.Create(nil);
  try
    Dataset.FieldDefs.Clear;
    Dataset.FieldDefs.Add('LastName', ftString, 20);
    Dataset.FieldDefs.Add('FirstName', ftString, 20);
    Dataset.FieldDefs.Add('ID', ftString, 4);
    Dataset.CreateDataset;

    Dataset.Open;
    Dataset.AppendRecord(['Jenkins', 'John', '0003']);
    Dataset.AppendRecord(['Brooks', 'Jenny', '0001']);
    Dataset.AppendRecord(['Adams', 'Paul', '0002']);

    Dataset.Open;
    Dataset.IndexFieldNames := 'ID';
    AssertException('Refresh raises error',EDatabaseError,Dataset.Refresh);
  finally
    Dataset.Close;
    Dataset.Free;
  end;
end;

const
  SCyrillicWord : UnicodeString = #$0421#$043B#$043E#$0432#$043E;

function TTestSpecificTBufDataset.GetCodePageDataset: TBufDataset;

begin
  Result := TBufDataset.Create(nil);
  try
    Result.FieldDefs.Add('NAME', ftString, 5);
    TBufFieldDef(Result.FieldDefs[0]).Codepage := CP_UTF8;
    Result.CreateDataset;
    Result.Append;
    Result.FieldByName('NAME').AsUTF8String := UTF8Encode(SCyrillicWord);
    Result.Post;
  except
    Result.Free;
    raise;
  end;
end;


procedure TTestSpecificTBufDataset.IntTestCodePageStreaming(aFormat: TDataPacketFormat);

var
  lDS: TBufDataset;
  lStream: TMemoryStream;

begin
  lStream := TMemoryStream.Create;
  try
    lDS := GetCodePageDataset;
    try
      lDS.SaveToStream(lStream, aFormat);
    finally
      lDS.Free;
    end;
    lStream.Position := 0;
    lDS := TBufDataset.Create(nil);
    try
      lDS.LoadFromStream(lStream, aFormat);
      AssertEquals('Code page of the loaded field def', Integer(CP_UTF8), Integer(lDS.FieldDefs[0].Codepage));
      AssertEquals('Size of the loaded field def', 5, lDS.FieldDefs[0].Size);
      AssertEquals('Loaded value', SCyrillicWord, UTF8Decode(lDS.FieldByName('NAME').AsUTF8String));
    finally
      lDS.Free;
    end;
  finally
    lStream.Free;
  end;
end;


procedure TTestSpecificTBufDataset.TestCodePageFieldDefClass;

var
  lDS: TBufDataset;

begin
  lDS := TBufDataset.Create(nil);
  try
    lDS.FieldDefs.Add('NAME', ftString, 5);
    AssertTrue('FieldDefs is a TBufFieldDefs', lDS.FieldDefs is TBufFieldDefs);
    AssertTrue('Added field def is a TBufFieldDef', lDS.FieldDefs[0] is TBufFieldDef);
    AssertTrue('AddFieldDef returns a TBufFieldDef', lDS.FieldDefs.AddFieldDef is TBufFieldDef);
    AssertEquals('Default code page', Integer(CP_ACP), Integer(lDS.FieldDefs[0].Codepage));
    TBufFieldDef(lDS.FieldDefs[0]).Codepage := CP_UTF8;
    AssertEquals('Code page after setting it', Integer(CP_UTF8), Integer(lDS.FieldDefs[0].Codepage));
    AssertEquals('Bytes per character of a UTF-8 def', 4, lDS.FieldDefs[0].CharSize);
  finally
    lDS.Free;
  end;
end;


procedure TTestSpecificTBufDataset.TestCodePageValue;

var
  lDS: TBufDataset;

begin
  lDS := GetCodePageDataset;
  try
    AssertEquals('Code page of the field', Integer(CP_UTF8), Integer(TStringField(lDS.FieldByName('NAME')).CodePage));
    AssertEquals('Data size of a UTF-8 ftString(5) field', 21, lDS.FieldByName('NAME').DataSize);
    AssertEquals('Value after posting', SCyrillicWord, UTF8Decode(lDS.FieldByName('NAME').AsUTF8String));
    lDS.First;
    AssertEquals('Value read from the record buffer', SCyrillicWord, UTF8Decode(lDS.FieldByName('NAME').AsUTF8String));
  finally
    lDS.Free;
  end;
end;


procedure TTestSpecificTBufDataset.TestCodePageTruncate;

const
  cSmiley : UnicodeString = #$D83D#$DE00;

var
  lDS: TBufDataset;

begin
  lDS := GetCodePageDataset;
  try
    lDS.Edit;
    lDS.FieldByName('NAME').AsUTF8String := UTF8Encode(SCyrillicWord + 'ab');
    lDS.Post;
    AssertEquals('Two-byte characters cut after Size characters', SCyrillicWord, UTF8Decode(lDS.FieldByName('NAME').AsUTF8String));
    lDS.Edit;
    lDS.FieldByName('NAME').AsUTF8String := UTF8Encode('a' + cSmiley + cSmiley + cSmiley + cSmiley + cSmiley);
    lDS.Post;
    AssertEquals('Four-byte characters cut after Size characters', UTF8Encode('a' + cSmiley + cSmiley + cSmiley + cSmiley),
      lDS.FieldByName('NAME').AsUTF8String);
    lDS.Edit;
    lDS.FieldByName('NAME').AsUTF8String := 'abcdefgh';
    lDS.Post;
    AssertEquals('ASCII characters cut after Size characters', 'abcde', lDS.FieldByName('NAME').AsUTF8String);
  finally
    lDS.Free;
  end;
end;


procedure TTestSpecificTBufDataset.TestCodePageStreamingBIN;

begin
  IntTestCodePageStreaming(dfBinary);
end;


procedure TTestSpecificTBufDataset.TestCodePageStreamingXML;

begin
  IntTestCodePageStreaming(dfXML);
end;


procedure TTestSpecificTBufDataset.TestCodePageStreamVersion;

var
  lDS: TBufDataset;
  lStream: TMemoryStream;

begin
  lStream := TMemoryStream.Create;
  try
    lDS := TBufDataset.Create(nil);
    try
      lDS.FieldDefs.Add('NAME', ftString, 5);
      lDS.CreateDataset;
      lDS.SaveToStream(lStream, dfBinary);
    finally
      lDS.Free;
    end;
    AssertEquals('Format version without code pages', 30, PByte(lStream.Memory)[13]);
    lStream.Clear;
    lDS := GetCodePageDataset;
    try
      lDS.SaveToStream(lStream, dfBinary);
    finally
      lDS.Free;
    end;
    AssertEquals('Format version with code pages', 31, PByte(lStream.Memory)[13]);
  finally
    lStream.Free;
  end;
end;


procedure TTestSpecificTBufDataset.TestCodePageComponentStreaming;

var
  lDS, lCopy: TBufDataset;
  lStream: TMemoryStream;
  lBytes: AnsiString;

begin
  lCopy := nil;
  lStream := TMemoryStream.Create;
  lDS := TBufDataset.Create(nil);
  try
    lDS.FieldDefs.Add('PLAIN', ftString, 5);
    lStream.WriteComponent(lDS);
    SetString(lBytes, PAnsiChar(lStream.Memory), lStream.Size);
    AssertEquals('Default code page is not written', 0, Pos('Codepage', lBytes));
    lStream.Clear;
    lDS.FieldDefs.Add('NAME', ftString, 5);
    TBufFieldDef(lDS.FieldDefs[1]).Codepage := CP_UTF8;
    lStream.WriteComponent(lDS);
    lStream.Position := 0;
    lCopy := TBufDataset.Create(nil);
    lStream.ReadComponent(lCopy);
    AssertEquals('Number of streamed field defs', 2, lCopy.FieldDefs.Count);
    AssertEquals('Code page of a default def', Integer(CP_ACP), Integer(lCopy.FieldDefs[0].Codepage));
    AssertEquals('Code page of a UTF-8 def', Integer(CP_UTF8), Integer(lCopy.FieldDefs[1].Codepage));
    lCopy.CreateDataset;
    lCopy.Append;
    lCopy.FieldByName('NAME').AsUTF8String := UTF8Encode(SCyrillicWord);
    lCopy.Post;
    AssertEquals('Value in the streamed dataset', SCyrillicWord, UTF8Decode(lCopy.FieldByName('NAME').AsUTF8String));
  finally
    lCopy.Free;
    lDS.Free;
    lStream.Free;
  end;
end;

procedure TTestSpecificTBufDataset.TestIndexDefsAddWhileOpen;

var
  lDS: TBufDataset;

  procedure CheckOrder(const aMsg: string; const aIDs: array of Integer);

  var
    i: Integer;

  begin
    lDS.First;
    for i:=0 to High(aIDs) do
      begin
      AssertFalse(aMsg+': Eof before record '+IntToStr(i), lDS.Eof);
      AssertEquals(aMsg+': ID of record '+IntToStr(i), aIDs[i], lDS.FieldByName('ID').AsInteger);
      lDS.Next;
      end;
    AssertTrue(aMsg+': Eof after the last record', lDS.Eof);
  end;

begin
  lDS:=TBufDataset.Create(nil);
  try
    lDS.FieldDefs.Add('ID', ftInteger);
    lDS.FieldDefs.Add('NAME', ftString, 20);
    lDS.CreateDataset;
    lDS.AppendRecord([3, 'c']);
    lDS.AppendRecord([1, 'a']);
    lDS.IndexDefs.Add('BYID', 'ID', []);
    lDS.AppendRecord([2, 'b']);
    CheckOrder('Default order before selecting the index', [3, 1, 2]);
    lDS.IndexName:='BYID';
    AssertEquals('IndexName', 'BYID', lDS.IndexName);
    CheckOrder('Order of the index added while open', [1, 2, 3]);
    lDS.AppendRecord([0, 'z']);
    CheckOrder('Order after appending to the index', [0, 1, 2, 3]);
    lDS.IndexName:='';
    CheckOrder('Default order after deselecting the index', [3, 1, 2, 0]);
    lDS.Close;
    AssertEquals('Index definitions after Close', 1, lDS.IndexDefs.Count);
    lDS.CreateDataset;
    lDS.AppendRecord([5, 'e']);
    lDS.AppendRecord([4, 'd']);
    lDS.IndexName:='BYID';
    CheckOrder('Order of the index after reopening', [4, 5]);
    lDS.IndexDefs.Add('BYNAME', 'NAME', []);
    lDS.Close;
    AssertEquals('Index definitions after closing with an unused index', 2, lDS.IndexDefs.Count);
  finally
    lDS.Free;
  end;
end;

procedure TTestSpecificTBufDataset.TestIndexDefsAddWhileOpenMaxIndexes;

var
  lDS: TBufDataset;
  lRaised: Boolean;
  i, lLast: Integer;

begin
  lDS:=TBufDataset.Create(nil);
  try
    lDS.FieldDefs.Add('ID', ftInteger);
    lDS.CreateDataset;
    lDS.AppendRecord([2]);
    lDS.AppendRecord([1]);
    lLast:=lDS.MaxIndexesCount;
    for i:=lDS.IndexDefs.Count to lLast do
      lDS.IndexDefs.Add('IDX'+IntToStr(i), 'ID', []);
    lDS.IndexName:='IDX'+IntToStr(lLast-1);
    AssertEquals('Index within MaxIndexesCount selected', 'IDX'+IntToStr(lLast-1), lDS.IndexName);
    lRaised:=False;
    try
      lDS.IndexName:='IDX'+IntToStr(lLast);
    except
      on EDatabaseError do
        lRaised:=True;
    end;
    AssertTrue('Selecting an index beyond MaxIndexesCount raises EDatabaseError', lRaised);
    AssertEquals('IndexName after the failed selection', 'IDX'+IntToStr(lLast-1), lDS.IndexName);
    lDS.AppendRecord([3]);
    AssertEquals('Record count', 3, lDS.RecordCount);
    lDS.First;
    AssertEquals('First ID in the selected index', 1, lDS.FieldByName('ID').AsInteger);
    lDS.Close;
  finally
    lDS.Free;
  end;
end;

procedure TTestSpecificTBufDataset.TestFilterCaseInsensitiveNonASCII;

var
  lSavedCodePage: TSystemCodePage;

  // Converts UTF-16 text to a string in the default system code page
  function Native(const aText: UnicodeString): string;

  begin
    Result:=string(aText);
  end;

  procedure CheckFilters(const aCodePageName: string);

  const
    cLowerN = UnicodeString(#$00F1);
    cUpperN = UnicodeString(#$00D1);

  var
    lDS: TBufDataset;

    procedure CheckCount(const aFilter: UnicodeString; aExpected: Integer);

    var
      lCount: Integer;

    begin
      lDS.Filtered:=False;
      lDS.Filter:=Native(aFilter);
      lDS.Filtered:=True;
      lCount:=0;
      lDS.First;
      while not lDS.Eof do
        begin
        Inc(lCount);
        lDS.Next;
        end;
      AssertEquals(aCodePageName+': records for filter '+UTF8Encode(aFilter), aExpected, lCount);
    end;

  begin
    lDS:=TBufDataset.Create(nil);
    try
      lDS.FieldDefs.Add('N', ftString, 20);
      lDS.CreateDataset;
      lDS.AppendRecord([Native(cLowerN)]);
      lDS.AppendRecord([Native(cUpperN)]);
      lDS.AppendRecord([Native('a')]);
      lDS.AppendRecord([Native('a'+cLowerN+'b')]);
      lDS.AppendRecord([Native('A'+cUpperN+'B')]);
      lDS.First;
      AssertEquals(aCodePageName+': stored value', UTF8Encode(cLowerN), UTF8Encode(UnicodeString(lDS.FieldByName('N').AsString)));
      lDS.FilterOptions:=[foCaseInsensitive];
      CheckCount('N='''+cLowerN+'''', 2);
      CheckCount('N='''+cUpperN+'''', 2);
      CheckCount('N<>'''+cLowerN+'''', 3);
      CheckCount('N='''+cLowerN+'*''', 2);
      CheckCount('N=''*'+cUpperN+'''', 2);
      CheckCount('N=''*'+cLowerN+'*''', 4);
      CheckCount('N=''*'+cUpperN+'b''', 2);
      CheckCount('N=''a'+cUpperN+'*''', 2);
      lDS.FilterOptions:=[];
      CheckCount('N='''+cLowerN+'''', 1);
      lDS.Close;
    finally
      lDS.Free;
    end;
  end;

begin
  lSavedCodePage:=DefaultSystemCodePage;
  try
    CheckFilters('System code page '+IntToStr(lSavedCodePage));
    DefaultSystemCodePage:=CP_UTF8;
    CheckFilters('CP_UTF8');
  finally
    DefaultSystemCodePage:=lSavedCodePage;
  end;
end;

procedure TTestSpecificTBufDataset.TestIndexFieldNamesDescRepeated;

const
  cTitleLastNameDesc = 'Mr Zed,Mr Smith,Mr Brown,Ms Young,Ms Adams';
  cTitleLastName = 'Mr Brown,Mr Smith,Mr Zed,Ms Adams,Ms Young';

var
  lDS: TBufDataset;

  // Returns the records as a comma-separated list of 'Title LastName'
  function Order: string;

  begin
    Result:='';
    lDS.First;
    while not lDS.Eof do
      begin
      if Result<>'' then
        Result:=Result+',';
      Result:=Result+lDS.FieldByName('TITLE').AsString+' '+lDS.FieldByName('LASTNAME').AsString;
      lDS.Next;
      end;
  end;

  // Returns the titles as a comma-separated list
  function TitleOrder: string;

  begin
    Result:='';
    lDS.First;
    while not lDS.Eof do
      begin
      if Result<>'' then
        Result:=Result+',';
      Result:=Result+lDS.FieldByName('TITLE').AsString;
      lDS.Next;
      end;
  end;

begin
  lDS:=TBufDataset.Create(nil);
  try
    lDS.FieldDefs.Add('TITLE', ftString, 10);
    lDS.FieldDefs.Add('LASTNAME', ftString, 20);
    lDS.CreateDataset;
    lDS.AppendRecord(['Mr', 'Smith']);
    lDS.AppendRecord(['Ms', 'Adams']);
    lDS.AppendRecord(['Mr', 'Brown']);
    lDS.AppendRecord(['Ms', 'Young']);
    lDS.AppendRecord(['Mr', 'Zed']);
    lDS.IndexFieldNames:='TITLE; LASTNAME DESC';
    AssertEquals('First assignment of TITLE; LASTNAME DESC', cTitleLastNameDesc, Order);
    lDS.IndexFieldNames:='TITLE; LASTNAME DESC';
    AssertEquals('Second assignment of TITLE; LASTNAME DESC', cTitleLastNameDesc, Order);
    lDS.IndexFieldNames:='TITLE DESC';
    AssertEquals('TITLE DESC', 'Ms,Ms,Mr,Mr,Mr', TitleOrder);
    lDS.IndexFieldNames:='TITLE; LASTNAME';
    AssertEquals('TITLE; LASTNAME after TITLE DESC', cTitleLastName, Order);
    lDS.IndexFieldNames:='TITLE; LASTNAME DESC';
    AssertEquals('TITLE; LASTNAME DESC after TITLE; LASTNAME', cTitleLastNameDesc, Order);
    lDS.IndexFieldNames:='TITLE;LASTNAME DESC';
    AssertEquals('TITLE;LASTNAME DESC without space', cTitleLastNameDesc, Order);
    lDS.IndexFieldNames:='TITLE; LASTNAME';
    AssertEquals('TITLE; LASTNAME after TITLE;LASTNAME DESC', cTitleLastName, Order);
  finally
    lDS.Free;
  end;
end;


initialization
{$ifdef fpc}

  if uppercase(dbconnectorname)='BUFDATASET' then
    begin
    RegisterTestDecorator(TDBBasicsTestSetup, TTestSpecificTBufDataset);
    end;
{$endif fpc}
end.
