
function SysFindResource(hModule:HMODULE; lpName: PWideChar; lpType: PWideChar):TResourceHandle; cdecl; external 'coredll' name 'FindResourceW';
function SysLoadResource(hModule:HMODULE; hResInfo: TResourceHandle):HGLOBAL; cdecl; external 'coredll' name 'LoadResource';
function SysSizeofResource(hModule:HMODULE; hResInfo:TResourceHandle):DWORD; cdecl; external 'coredll' name 'SizeofResource';

Function HINSTANCE: HMODULE;
begin
  Result:=sysinstance;
end;

Function FindResource(ModuleHandle: HMODULE; ResourceName, ResourceType: PChar): TResourceHandle;
var
  ws1, ws2: PWideChar;
begin
  if cardinal(ResourceName) shr 16 <> 0 then
    ws1:=PCharToPWideChar(ResourceName)
  else
    ws1:=pointer(ResourceName);
  if cardinal(ResourceType) shr 16 <> 0 then
    ws2:=PCharToPWideChar(ResourceType)
  else
    ws2:=pointer(ResourceType);
  Result:=SysFindResource(ModuleHandle, ws1, ws2);
  if cardinal(ResourceType) shr 16 <> 0 then
    FreeMem(ws2);
  if cardinal(ResourceName) shr 16 <> 0 then
    FreeMem(ws1);
end;

Function LoadResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): HGLOBAL;
begin
  Result:=SysLoadresource(ModuleHandle,Reshandle);
end;

Function SizeofResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): Integer;
begin
  Result:=SysSizeofResource(ModuleHandle,Reshandle);
end;

Function LockResource(ResData: HGLOBAL): Pointer;
begin
  Result:=pointer(ResData);
end;

Function UnlockResource(ResData: HGLOBAL): LongBool;
begin
  UnlockResource := True;
end;

Function FreeResource(ResData: HGLOBAL): LongBool;
begin
  FreeResource := True;
end;
