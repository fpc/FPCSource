type
  TResourceHandle = Cardinal;
  HMODULE = Cardinal;
  HGLOBAL = Cardinal;
      
// Win32 API compatible Resource functions
Function HINSTANCE : HMODULE;
Function FindResource(ModuleHandle: HMODULE; ResourceName, ResourceType: PChar): TResourceHandle;
Function FindResource(ModuleHandle: HMODULE; ResourceName, ResourceType: AnsiString): TResourceHandle;
Function LoadResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): HGLOBAL;
Function SizeofResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): Integer;
Function LockResource(ResData: HGLOBAL): Pointer;
Function UnlockResource(ResData: HGLOBAL): LongBool;
Function FreeResource(ResData: HGLOBAL): LongBool;

      