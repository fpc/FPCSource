
function SysFindResource(hModule:HMODULE; lpName:Pchar; lpType:Pchar):TResourceHandle; external 'kernel32' name 'FindResourceA';
function SysLoadResource(hModule:HMODULE; hResInfo:TResourceHandle):HGLOBAL; external 'kernel32' name 'LoadResource';
function SysSizeofResource(hModule:HMODULE; hResInfo:TResourceHandle):DWORD; external 'kernel32' name 'SizeofResource';
function SysLockResource(hResData:HGLOBAL):Pointer; external 'kernel32' name 'LockResource';
function SysFreeResource(hResData:HGLOBAL):Longbool; external 'kernel32' name 'FreeResource';

Function HINSTANCE : HMODULE;

begin
  Result:=sysinstance;
end;

Function FindResource(ModuleHandle: HMODULE; ResourceName, ResourceType: PChar): TResourceHandle;

begin
  Result:=SysFindResource(ModuleHandle,ResourceName,ResourceType);
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
  Result:=SysLockResource(ResData);
end;

Function UnlockResource(ResData: HGLOBAL): LongBool;

begin
  Result:=FreeResource(ResData);
end;

Function FreeResource(ResData: HGLOBAL): LongBool;

begin
  Result:=SysFreeResource(ResData);
end;
