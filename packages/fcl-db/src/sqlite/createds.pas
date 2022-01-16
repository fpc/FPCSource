program createds;

{$Mode ObjFpc}
{$H+}
{$define DEBUGHEAP}

uses 
{$ifdef DEBUGHEAP}
  Heaptrc,
{$endif}
{$ifdef Linux}
  cmem,
{$endif}
  sqlite3ds,
  sysutils,db,inifiles;

const
  SQLITEDS_TESTS_INI_FILE = 'sqlitedstests.ini';
  DEFAULT_TABLENAME = 'tabletest';
  DEFAULT_FILENAME = 'test.db';
  
var 
  dsTest: TSqlite3Dataset;
  ini: TIniFile;

begin 
  {$ifdef DEBUGHEAP}
  SetHeapTraceOutput(ExtractFileName(ParamStr(0))+'.heap.log');
  {$endif}
  dsTest:=TSqlite3Dataset.Create(nil);
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
    if CreateTable then
	begin
	  WriteLn('Table created successfully');
	  if not TableExists then
	    WriteLn('TableExists check failed with error: ', ReturnString);
	end  
	else
      WriteLn('Error creating table');
    WriteLn('ReturnString after CreateTable: ',ReturnString);
    Destroy;
  end;
end.
