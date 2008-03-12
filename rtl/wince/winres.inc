
function SysFindResource(hModule:TFPResourceHMODULE; lpName: PWideChar; lpType: PWideChar):TFPResourceHandle; cdecl; external 'coredll' name 'FindResourceW';
function SysLoadResource(hModule:TFPResourceHMODULE; hResInfo: TFPResourceHandle):TFPResourceHGLOBAL; cdecl; external 'coredll' name 'LoadResource';
function SysSizeofResource(hModule:TFPResourceHMODULE; hResInfo:TFPResourceHandle):DWORD; cdecl; external 'coredll' name 'SizeofResource';

var
  SysInstance : PtrUInt;external name '_FPC_SysInstance' ;

Function IntHINSTANCE: TFPResourceHMODULE;
begin
  IntHINSTANCE:=sysinstance;
end;

Function IntEnumResourceTypes(ModuleHandle : TFPResourceHMODULE; EnumFunc : EnumResTypeProc; lParam : PtrInt) : LongBool;
begin
  IntEnumResourceTypes:=False;
end;

Function IntEnumResourceNames(ModuleHandle : TFPResourceHMODULE; ResourceType : PChar; EnumFunc : EnumResNameProc; lParam : PtrInt) : LongBool;
begin
  IntEnumResourceNames:=False;
end;

Function IntEnumResourceLanguages(ModuleHandle : TFPResourceHMODULE; ResourceType, ResourceName : PChar; EnumFunc : EnumResLangProc; lParam : PtrInt) : LongBool;
begin
  IntEnumResourceLanguages:=False;
end;

Function IntFindResource(ModuleHandle: TFPResourceHMODULE; ResourceName, ResourceType: PChar): TFPResourceHandle;
var
  ws1, ws2: PWideChar;
begin
  if PtrUInt(ResourceName) shr 16 <> 0 then
    ws1:=PCharToPWideChar(ResourceName)
  else
    ws1:=pointer(ResourceName);
  if PtrUInt(ResourceType) shr 16 <> 0 then
    ws2:=PCharToPWideChar(ResourceType)
  else
    ws2:=pointer(ResourceType);
  IntFindResource:=SysFindResource(ModuleHandle, ws1, ws2);
  if PtrUInt(ResourceType) shr 16 <> 0 then
    FreeMem(ws2);
  if PtrUInt(ResourceName) shr 16 <> 0 then
    FreeMem(ws1);
end;

Function IntFindResourceEx(ModuleHandle: TFPResourceHMODULE; ResourceType, ResourceName: PChar; Language : word): TFPResourceHandle;
begin
  IntFindResourceEx:=FindResource(ModuleHandle,ResourceName,ResourceType);
end;

Function IntLoadResource(ModuleHandle: TFPResourceHMODULE; ResHandle: TFPResourceHandle): TFPResourceHGLOBAL;
begin
  IntLoadResource:=SysLoadresource(ModuleHandle,Reshandle);
end;

Function IntSizeofResource(ModuleHandle: TFPResourceHMODULE; ResHandle: TFPResourceHandle): LongWord;
begin
  IntSizeofResource:=SysSizeofResource(ModuleHandle,Reshandle);
end;

Function IntLockResource(ResData: TFPResourceHGLOBAL): Pointer;
begin
  IntLockResource:=pointer(ResData);
end;

Function IntUnlockResource(ResData: TFPResourceHGLOBAL): LongBool;
begin
  IntUnlockResource:= True;
end;

Function IntFreeResource(ResData: TFPResourceHGLOBAL): LongBool;
begin
  IntFreeResource:= True;
end;

const
  InternalResourceManager : TResourceManager =
  (
    HINSTANCEFunc : @IntHINSTANCE;
    EnumResourceTypesFunc : @IntEnumResourceTypes;
    EnumResourceNamesFunc : @IntEnumResourceNames;
    EnumResourceLanguagesFunc : @IntEnumResourceLanguages;
    FindResourceFunc : @IntFindResource;
    FindResourceExFunc : @IntFindResourceEx;
    LoadResourceFunc : @IntLoadResource;
    SizeofResourceFunc : @IntSizeofResource;
    LockResourceFunc : @IntLockResource;
    UnlockResourceFunc : @IntUnlockResource;
    FreeResourceFunc : @IntFreeResource;
  );
