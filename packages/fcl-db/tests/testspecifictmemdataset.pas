unit TestSpecificTMemDataSet;

{
  Unit tests which are specific to TMemDataset
}

{$mode objfpc}{$H+}
{$codepage UTF8}

interface

uses
  Classes, SysUtils,
  testregistry,
  ToolsUnit;

type

  { TTestSpecificTMemDataset }

  TTestSpecificTMemDataset = class(TDBBasicsTestCase)
  private
  protected
  published
    procedure TestClear;
    procedure TestFileName;
    procedure TestCopyFromDataset; //is copied dataset identical to original?
    procedure TestCopyFromDatasetMoved; //move record then copy. Is copy identical? Has record position changed?
    Procedure TestLocateUTF8;
  end;

implementation

uses
  MemDS, db;

const
  Test_FileName='test.dat';

{ TTestSpecificTMemDataset }

procedure TTestSpecificTMemDataset.TestClear;
const
  testValuesCount=3;
var
  i: integer;
begin
  with DBConnector.GetNDataset(10) as TMemDataset do
  begin
    Open;
    Clear;
    // test after FieldDefs are Cleared, if internal structures are updated properly
    // create other FieldDefs
    FieldDefs.Add('Fs', ftString, 20);
    FieldDefs.Add('Fi', ftInteger);
    FieldDefs.Add('Fi2', ftInteger);
    // use only Open without CreateTable
    Open;
    CheckTrue(IsEmpty);
    CheckEquals(0, DataSize);
    // add some data
    for i:=1 to testValuesCount do
      AppendRecord([TestStringValues[i], TestIntValues[i], TestIntValues[i]]);
    // check data
    CheckEquals(testValuesCount, RecordCount);
    First;
    for i:=1 to testValuesCount do
    begin
      CheckEquals(TestStringValues[i], FieldByName('Fs').AsString);
      CheckEquals(TestIntValues[i], FieldByName('Fi2').AsInteger);
      Next;
    end;
    CheckTrue(Eof);
  end;
end;

procedure TTestSpecificTMemDataset.TestFileName;
var memds1, memds2: TMemDataset;
begin
  memds1:=DBConnector.GetFieldDataset as TMemDataset;
  memds2:=DBConnector.GetNDataset(0) as TMemDataset;

  memds1.Open;
  memds1.SaveToFile(Test_FileName);
  memds1.Close;
  memds1.Clear;

  memds1.FileName:=Test_FileName;
  memds1.Open;
  CheckFieldDatasetValues(memds1);

  // try read same file into different dataset, testing if FieldDefs are updated properly
  memds2.FileName:=Test_FileName;
  memds2.Open;
  CheckFieldDatasetValues(memds2);
  DeleteFile(memds2.FileName);
end;

procedure TTestSpecificTMemDataset.TestCopyFromDataset;
var memds1, memds2: TMemDataset;
    i: integer;
begin
  memds1:=DBConnector.GetFieldDataset as TMemDataset;
  memds2:=DBConnector.GetNDataset(0) as TMemDataset;

  memds1.Open;
  // insert 1st row with all NULL values
  memds1.Insert; memds1.Post;
  memds2.CopyFromDataset(memds1);
  // check if 1st row has all NULL values
  for i:=0 to memds2.FieldCount-1 do CheckTrue(memds2.Fields[i].IsNull, 'IsNull');
  memds2.Delete;
  CheckFieldDatasetValues(memds2);
end;

procedure TTestSpecificTMemDataset.TestCopyFromDatasetMoved;
var
  memds1, memds2: TMemDataset;
  CurrentID,NewID: integer;
begin
  memds1:=DBConnector.GetFieldDataset as TMemDataset;
  memds2:=DBConnector.GetNDataset(0) as TMemDataset;

  memds1.Open;
  memds1.Next; //this should not influence the copydataset step.
  CurrentID:=memds1.FieldByName('ID').AsInteger;
  memds2.CopyFromDataset(memds1);
  CheckFieldDatasetValues(memds2);
  NewID:=memds1.FieldByName('ID').AsInteger;
  CheckEquals(CurrentID,NewID,'Mismatch between ID field contents - the record has moved.');
end;

procedure TTestSpecificTMemDataset.TestLocateUTF8;
Var
  MemDataset1: TMemDataset;
  S : UTF8String;
begin
  MemDataset1:=TMemDataset.Create(Nil);
  With MemDataset1 do
    try
    FieldDefs.Add('first',ftString,40,0,true,False,1,cp_UTF8);
    FieldDefs.Add('second',ftString,40,0,true,False,2,cp_ACP);
    CreateTable;
    Active:=True;
    Append;
    Fields[0].AsUTF8String:='♯abcd';
    Fields[1].AsString:='native';
    Post;
    Append;
    Fields[0].AsUTF8String:='défaut';
    Fields[1].AsString:='morenative';
    Post;
    First;
    While not eof do
      begin
      S:=fields[0].AsUTF8String;
      Writeln(S);
      next;
      end;
    First;
    AssertTrue('UTF8 1 ok',Locate('first','♯abcd',[]));
    AssertTrue('UTF8 2 ok',Locate('first','défaut',[]));
    AssertTrue('ANSI 1 ok',Locate('second','native',[]));
    AssertTrue('ANSI 1 ok',Locate('second','morenative',[]));
  finally
    Free;
  end;
end;


initialization

  if uppercase(dbconnectorname)='MEMDS' then
    begin
    RegisterTestDecorator(TDBBasicsTestSetup, TTestSpecificTMemDataset);
    end;

end.
