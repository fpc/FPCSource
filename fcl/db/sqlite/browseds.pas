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
  crt,sysutils,SqliteDS,db;

var 
  dsTest:TSQliteDataset;
  Counter:Integer;

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
    First;
    while not Eof do
    begin
      WriteLn('ROWID: ',FieldByName('_ROWID_').AsInteger);
      WriteLn('Code: ',FieldByName('Code').AsInteger);
	    WriteLn('Name: ',FieldByName('Name').AsString);
	    Next;
    end;  
	  Close;
	  Destroy;
	end;
	Readkey;
	exit;
end.