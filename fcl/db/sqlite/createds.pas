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
	  if FileExists(FileName) then
	    DeleteFile(FileName);
	  TableName:='NewTable';
	  with FieldDefs do
	  begin
	    Add('Code',ftInteger,0,False);
	    Add('Name',ftString,0,False); 
	  end; 
	  CreateDataSet(True);
	  Destroy;
	end;
	exit;
end.