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
  crt,sysutils,SqliteDS;

var 
  dsTest:TSQliteDataset;
  Counter:Integer;

begin 
	//clrscr;
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
	  Append;
	  FieldByName('Code').AsInteger:=100;
	  FieldByName('Name').AsString:='Luiz';
	  Post;
	  Append;
	  FieldByName('Code').AsInteger:=101;
	  FieldByName('Name').AsString:='Américo';
	  Post;
	  Append;
	  FieldByName('Code').AsInteger:=102;
	  FieldByName('Name').AsString:='Ana';
	  Post;
	  Append;
	  FieldByName('Code').AsInteger:=103;
	  FieldByName('Name').AsString:='Luiza';
	  Post;
	  ApplyUpdates;
	  writeln('Last sqlite reurn: ',SqliteReturnString);
	  Close;
	  Destroy;
	end;
	exit;
end.