{$ifdef FPC} {$mode delphi}  {$endif}
unit tregistry2;

interface

procedure DoRegTest2;

implementation

uses Windows, SysUtils, Classes, registry;

const
  STestRegPath = 'Software\FPC-RegTest';
  
procedure TestFailed(ErrCode: integer);
begin
  raise Exception.Create('Test FAILED. Error code: ' + IntToStr(ErrCode));
end;

procedure ClearReg;
begin
  with TRegistry.Create do
    try
      DeleteKey(STestRegPath + '\1');
      DeleteKey(STestRegPath);
    finally
      Free;
    end;
end;

procedure DoRegTest2;
var
  reg: TRegistry;
  ri: TRegIniFile;
  rini: TRegistryIniFile;
  sl: TStringList;
begin
  ClearReg;
  reg:=TRegistry.Create;
  try
    if not reg.OpenKey(STestRegPath, True) then
      TestFailed(1);
    if reg.CurrentPath <> STestRegPath then
      TestFailed(2);
    reg.WriteString('Item1', '1');
    if not reg.OpenKey('\' + STestRegPath + '\1', True) then
      TestFailed(3);
    reg.WriteString('Item2', '2');
    if reg.CurrentPath <> STestRegPath + '\1' then
      TestFailed(5);
    reg.CloseKey;
    if reg.CurrentPath <> '' then
      TestFailed(6);

    ri:=TRegIniFile.Create(STestRegPath);
    with ri do
    try
      if ReadString('', 'Item1', '') <> '1' then
        TestFailed(10);
      if ReadString('1', 'Item2', '') <> '2' then
        TestFailed(11);
      if ReadString('', 'Item1', '') <> '1' then
        TestFailed(12);
      if not ValueExists('Item1') then
        TestFailed(13);

      WriteInteger('1', 'Item3', 3);

      sl:=TStringList.Create;
      try
        ReadSectionValues('1', sl);
        if sl.Count <> 2 then
          TestFailed(14);
        if sl.Values['Item2'] <> '2' then
          TestFailed(15);
        if sl.Values['Item3'] <> '3' then
          TestFailed(16);
      finally
        sl.Free;
      end;

      WriteInteger('', 'Item4', 4);
      if  GetDataType('Item4') <> rdString then
        TestFailed(17);
    finally
      Free;
    end;

    rini:=TRegistryIniFile.Create(STestRegPath);
    with rini do
    try
      if ReadString('', 'Item1', '') <> '1' then
        TestFailed(20);
      if ReadString('1', 'Item2', '') <> '2' then
        TestFailed(21);
      if ReadString('', 'Item1', '') <> '1' then
        TestFailed(22);
      if not ValueExists('', 'Item4') then
        TestFailed(23);
      if not ValueExists('1', 'Item2') then
        TestFailed(24);
    finally
      Free;
    end;

  finally
    reg.Free;
    ClearReg;
  end;
end;

end.

