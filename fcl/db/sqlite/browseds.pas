program browseds;
{$Mode ObjFpc}
{$define DEBUGHEAP}
uses
{$ifdef DEBUGHEAP}
  Heaptrc,
{$endif}
{$ifdef Linux}
  cmem,
{$endif}
  crt,sysutils,sqliteds,db;

var
  dsTest:TSQliteDataset;
  I:Integer;

procedure DumpFieldData (F : TField);
begin
  With F Do
    begin
    Writeln ('Field     : ',FieldName);
    Writeln ('Data type : ',FieldTypeNames[DataType]);
    Writeln ('As String : ',AsString);
    Case Datatype of
      ftSmallint, ftInteger, ftWord : Writeln ('As Longint : ',AsLongint);
      ftBoolean : Writeln ('As Boolean : ',AsBoolean);
      ftFloat : Writeln ('As Float : ',AsFloat);
      ftDate, ftTime, ftDateTime : Writeln ('As DateTime : ',AsDateTime);
    end;
    end;
  writeln('------------------');
  //Readkey;
end;

begin
  {$ifdef DEBUGHEAP}
  SetHeapTraceOutput('heaplog.txt');
  {$endif}
  dsTest:=TsqliteDataset.Create(nil);
  with dsTest do
  Begin
    FileName:='New.db';
    TableName:='NewTable';
    Sql:= 'SELECT _ROWID_,* FROM NewTable';
    Open;
    WriteLn('RecordCount: ',RecordCount);
    First;
    while not Eof do
    begin
      writeln(':::: Press a key to see data from record ',RecNo,' ::::');
      Readkey;
      For I:=0 to FieldCount-1 do
        DumpFieldData(Fields[I]);
      Next;
    end;
    Close;
    Destroy;
  end;
  Exit;
end.
