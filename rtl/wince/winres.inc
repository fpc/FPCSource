
function SysFindResource(hModule:HMODULE; lpName: PWideChar; lpType: PWideChar):TResourceHandle; external 'coredll' name 'FindResourceW';
function SysLoadResource(hModule:HMODULE; hResInfo: TResourceHandle):HGLOBAL; external 'coredll' name 'LoadResource';
function SysSizeofResource(hModule:HMODULE; hResInfo:TResourceHandle):DWORD; external 'coredll' name 'SizeofResource';

Function HINSTANCE: HMODULE;
begin
  Result:=sysinstance;
end;

Function FindResource(ModuleHandle: HMODULE; ResourceName, ResourceType: PChar): TResourceHandle;
var
  ws1, ws2: PWideChar;
begin
  ws1:=PCharToPWideChar(ResourceName);
  ws2:=PCharToPWideChar(ResourceType);
  Result:=SysFindResource(ModuleHandle, ws1, ws2);
  FreeMem(ws2);
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
