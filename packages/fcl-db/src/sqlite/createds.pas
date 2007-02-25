program createds;
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

begin
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
      Clear;
      Add('Code',ftInteger,0,False);
      Add('Name',ftString,0,False);
      Add('Bool',ftBoolean,0,False);
      Add('Float',ftFloat,0,False);
      Add('Word',ftWord,0,False);
      Add('DateTime',ftDateTime,0,False);
      Add('Date',ftDate,0,False);
      Add('Time',ftTime,0,False);
    end;
    CreateTable;
    writeln('SqliteReturnString after CreateTable: ',SqliteReturnString);
    Destroy;
  end;
  exit;
end.
