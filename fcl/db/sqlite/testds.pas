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

var dsTest:TSQliteDataset;
 I,Counter:Integer;

Procedure DumpFieldDef(F : TfieldDef);

begin
  With F do
    begin
    Writeln ('Name              : ',Name);
    Writeln ('FieldNo           : ',FieldNo);
    Writeln ('Size              : ',Size);
    Writeln ('FieldClass        : ',FieldClass.ClassName);
    Writeln ('Required          : ',required);
    Writeln ('Precision         : ',Precision);
    Writeln ('DataType          : ',FieldTypeNames[DataType]);
    Writeln ('InternalCalcField : ',Internalcalcfield);
    end;
  writeln('------------------'); 
  readkey; 
end;

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
  writeln('------------------');  
end;

Procedure DumpFieldData (F : TField);

begin
  With F Do
    begin
    Writeln ('Field     : ',FieldName);
    Writeln ('Data type : ',FieldTypeNames[DataType]);
    Writeln ('As String : ',Asstring);
    Case Datatype of
      ftSmallint, ftInteger, ftWord : Writeln ('As longint : ',AsLongint);
      ftBoolean : Writeln ('As Boolean : ',AsBoolean);
      ftFloat : Writeln ('As Float : ',AsFloat);
      ftDate, ftTime, ftDateTime : Writeln ('As DateTime : ',DateTimeToStr(AsDateTime));
    end;
    end;
  writeln('------------------');  
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
	  writeln('SqliteReturnString after Open: ',SqliteReturnString);
	  readkey;
	  Writeln ('Fielddefs count : ',FieldDefs.Count);
    WriteLn('==============DumpFieldDef==========');
    For I:=0 to FieldDefs.Count-1 do
      DumpFieldDef(FieldDefs.Items[i]);
    //readkey;  
    Writeln ('Fields count : ',FieldCount);
    WriteLn('============DumpFields ============');
    For I:=0 to FieldCount-1 do
      DumpField(Fields[i]);
    WriteLn('============DumpFieldData==========');
    While Not Eof do
    begin  
      For I:=0 to FieldCount-1 do
        DumpFieldData(Fields[I]);
      Next;  
    end;
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
	  Post;
	  FieldbyName('Code').AsInteger:=22222;
	  FieldbyName('Name').AsString:='This will be deleted';
	  WriteLn('First Record Appended - Code:',FieldbyName('Code').AsInteger);
	  WriteLn('First Record Appended - Name:',FieldbyName('Name').AsString);
	  Append;
	  Post;
	  FieldbyName('Code').AsInteger:=3333;
	  FieldbyName('Name').AsString:='This will stay';
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
	      Writeln('Record Found: It Should Not Occur');
      Next;  
	  end;
	  readkey; 
	  ApplyUpdates;
	  writeln('SqliteReturnString after ApplyUpdates: ',SqliteReturnString);
	  Close;
	  Destroy;
	end;
end.