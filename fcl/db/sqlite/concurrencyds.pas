program concurrencyds;
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
  dsOne,dsTwo:TSQliteDataset;

begin
  {$ifdef DEBUGHEAP}
  SetHeapTraceOutput('heaplog.txt');
  {$endif}
  dsOne:=TsqliteDataset.Create(nil);
  dsTwo:=TsqliteDataset.Create(nil);
  dsOne.FileName:='New.db';
  dsTwo.FileName:='New.db';
  dsOne.TableName:='NewTable';
  dsTwo.TableName:='NewTable';
  dsOne.Sql:= 'SELECT Code FROM NewTable';
  dsTwo.Sql:= 'SELECT Name FROM NewTable';
  dsOne.Open;
  dsTwo.Open;
  writeln('Sqlite Return after opening dsTwo: ',dsTwo.SqliteReturnString);
  dsOne.First;
  dsTwo.First;
  WriteLn('Code: ',dsOne.FieldByName('Code').AsInteger);
  WriteLn('Name: ',dsTwo.FieldByName('Name').AsString);
  dsOne.Next;
  dsTwo.Next;
  WriteLn('Code: ',dsOne.FieldByName('Code').AsInteger);
  WriteLn('Name: ',dsTwo.FieldByName('Name').AsString);
  dsOne.Close;
  dsTwo.Close;
  dsOne.Destroy;
  dsTwo.Destroy;
  Readkey;
  exit;
end.
