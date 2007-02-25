program testds;
{$Mode ObjFpc}
{$define DEBUGHEAP}
uses
{$ifdef DEBUGHEAP}
  Heaptrc,
{$endif}
{$ifdef Linux}
  cmem,
{$endif}
  crt,sysutils,db,SqliteDS;

var
  dsTest:TSQliteDataset;
  I:Integer;

Procedure DumpField(F : Tfield);

begin
  With F do
    begin
    Writeln ('FieldName : ',FieldName);
    Writeln ('FieldNo   : ',FieldNo);
    Writeln ('Index     : ',Index);
    Writeln ('DataSize  : ',DataSize);
    Writeln ('Size      : ',Size);
    Writeln ('DataType  : ',FieldTypeNames[DataType]);
    Writeln ('Class     : ',ClassName);
    Writeln ('Required  : ',required);
    Writeln ('ReadOnly  : ',ReadOnly);
    Writeln ('Visible   : ',Visible);
    end;
  writeln('-------- Press a key to continue ----------');
  readkey;
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
    SQL:='SELECT _ROWID_,* from NewTable';
    Open;
    //writeln('SqliteReturnString after Open: ',SqliteReturnString);
    //readkey;
    Writeln ('Fields count : ',FieldCount);
    WriteLn('============DumpFields ============');
    For I:=0 to FieldCount-1 do
      DumpField(Fields[i]);
    writeln('Push a key to test -Edit-');
    readkey;
    clrscr;
    WriteLn('Old Code:',FieldbyName('Code').AsInteger);
    WriteLn('Old Name:',FieldbyName('Name').AsString);
    FieldbyName('Code').AsInteger:=12345;
    FieldbyName('Name').AsString:='Record Edited in TestDs.pas';
    WriteLn('New Code:',FieldbyName('Code').AsInteger);
    WriteLn('New Name:',FieldbyName('Name').AsString);
    writeln('Push a key to test -Append-');
    readkey;
    clrscr;
    Append;
    FieldbyName('Code').AsInteger:=22222;
    FieldbyName('Name').AsString:='This will be deleted';
    Post;
    WriteLn('First Record Appended - Code:',FieldbyName('Code').AsInteger);
    WriteLn('First Record Appended - Name:',FieldbyName('Name').AsString);
    Append;
    FieldbyName('Code').AsInteger:=3333;
    FieldbyName('Name').AsString:='This will stay';
    Post;
    WriteLn('Second Record Appended - Code:',FieldbyName('Code').AsInteger);
    WriteLn('Second Record Appended - Name:',FieldbyName('Name').AsString);
    writeln('Push a key to test -Delete-');
    readkey;
    clrscr;
    Prior;
    WriteLn('Current record:');
    Writeln('RowId:',Fields[0].AsInteger);
    WriteLn('Code: ',FieldbyName('Code').AsInteger);
    WriteLn('Name: ',FieldbyName('Name').AsString);
    if FieldbyName('Code').AsInteger = 22222 then
      Writeln('This record should be deleted');
    Delete;
    WriteLn('After Delete:');
    Writeln('RowId:',Fields[0].AsInteger);
    WriteLn('Code: ',FieldbyName('Code').AsInteger);
    WriteLn('Name: ',FieldbyName('Name').AsString);

    WriteLn('Try to find record with code = 22222');
    First;
    While Not Eof do
    begin
      if FieldbyName('Code').AsInteger = 22222 then
        Writeln('Record Found: It Should Not Occur')
      else
        Writeln('Record NOT Found: It''s OK');
      Next;
    end;
    readkey;
    ApplyUpdates;
    writeln('SqliteReturnString after ApplyUpdates: ',SqliteReturnString);
    Close;
    Destroy;
  end;
end.
