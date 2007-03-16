program createds;

{$Mode ObjFpc}
{$H+}
{$define DEBUGHEAP}

//To test the sqlite3 version replace sqliteds by sqlite3ds
//  and TSqliteDataset by TSqlite3Dataset

uses 
{$ifdef DEBUGHEAP}
  Heaptrc,
{$endif}
{$ifdef Linux}
  cmem,
{$endif}
  sqliteds,
  sysutils,db,inifiles;

const
  SQLITEDS_TESTS_INI_FILE = 'sqlitedstests.ini';
  DEFAULT_TABLENAME = 'tabletest';
  DEFAULT_FILENAME = 'test.db';
  
var 
  dsTest:TSqliteDataset;
  ini: TIniFile;

begin 
  {$ifdef DEBUGHEAP}
  SetHeapTraceOutput(ExtractFileName(ParamStr(0))+'.heap.log');
  {$endif}
  dsTest:=TSqliteDataset.Create(nil);
  with dsTest do
  begin
    //Load Database properties from a inifile
    ini:=TIniFile.Create(SQLITEDS_TESTS_INI_FILE);
    FileName:=ini.ReadString('testinfo','filename',DEFAULT_FILENAME);
    TableName:=ini.ReadString('testinfo','tablename',DEFAULT_TABLENAME);
    ini.Destroy;
    //Ensure the file does not exist yet
    if FileExists(FileName) then
      DeleteFile(FileName);
    //Create a table with all available field types
    with FieldDefs do
    begin
      Clear;
      Add('Integer',ftInteger);
      Add('String',ftString);
      Add('Boolean',ftBoolean);
      Add('Float',ftFloat);
      Add('Word',ftWord);
      Add('DateTime',ftDateTime);
      Add('Date',ftDate);
      Add('Time',ftTime);
      Add('AutoInc',ftAutoInc);
      Add('Memo',ftMemo);
      Add('LargeInt',ftLargeint);
      Add('Currency',ftCurrency);
    end; 
    CreateTable;
    writeln('ReturnString after CreateTable: ',ReturnString);
    Destroy;
  end;
end.
