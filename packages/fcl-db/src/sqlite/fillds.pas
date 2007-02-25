program fillds;
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
    Append;
    FieldByName('Code').AsInteger:=100;
    FieldByName('Name').AsString:='Luiz';
    FieldByName('Bool').AsBoolean:= True;
    FieldByName('Float').AsFloat:=2;
    FieldByName('DateTime').AsDateTime:=Now;
    FieldByName('Time').AsDateTime:=Time;
    FieldByName('Date').AsDateTime:=Date;
    Post;
    Append;
    FieldByName('Code').AsInteger:=101;
    FieldByName('Name').AsString:='Américo';
    FieldByName('Bool').AsBoolean:= True;
    FieldByName('Float').AsFloat:=1.1;
    FieldByName('DateTime').AsDateTime:=Now;
    FieldByName('Time').AsDateTime:=Time;
    FieldByName('Date').AsDateTime:=Date;
    Post;
    Append;
    FieldByName('Code').AsInteger:=102;
    FieldByName('Name').AsString:='Ana';
    FieldByName('Bool').AsBoolean:= False;
    FieldByName('Float').AsFloat:=5.0E-324;
    FieldByName('DateTime').AsDateTime:=Now;
    FieldByName('Time').AsDateTime:=Time;
    FieldByName('Date').AsDateTime:=Date;
    Post;
    Append;
    FieldByName('Code').AsInteger:=103;
    FieldByName('Name').AsString:='Luiza';
    FieldByName('Bool').AsBoolean:= False;
    FieldByName('Float').AsFloat:=1.7E308;
    FieldByName('DateTime').AsDateTime:=Now;
    FieldByName('Time').AsDateTime:=Time;
    FieldByName('Date').AsDateTime:=Date;
    Post;
    ApplyUpdates;
    writeln('Last sqlite return: ',SqliteReturnString);
    Close;
    Destroy;
  end;
  exit;
end.
