{$ifdef FPC} {$mode delphi}  {$endif}
unit tregistry2;

interface

procedure DoRegTest2;

implementation

uses Windows, SysUtils, registry;

const
  STestRegPath = 'Software\FPC-RegTest';
  
procedure TestFailed(ErrCode: integer);
begin
  raise Exception.Create('Test FAILED. Error code: ' + IntToStr(ErrCode));
end;

procedure DoRegTest2;
var
  reg: TRegistry;
  k: HKEY;
begin
  reg:=TRegistry.Create;
  try
    if not reg.OpenKey(STestRegPath, True) then
      TestFailed(1);
    if reg.CurrentPath <> STestRegPath then
      TestFailed(2);
    k:=reg.CurrentKey;
    if not reg.OpenKey('\' + STestRegPath + '\1', True) then
      TestFailed(3);
    if RegCloseKey(k) = 0 then
      TestFailed(4);
    if reg.CurrentPath <> STestRegPath + '\1' then
      TestFailed(5);
  finally
    reg.Free;
  end;
end;

end.

