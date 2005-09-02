Function HINSTANCE : HMODULE;

begin
  Result:=0;
end;

Function FindResource(ModuleHandle: HMODULE; ResourceName, ResourceType: PChar): TResourceHandle;

begin
  Result:=0;
end;

Function LoadResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): HGLOBAL;

begin
  Result:=0;
end;

Function SizeofResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): Integer;

begin
  Result:=-1;
end;

Function LockResource(ResData: HGLOBAL): Pointer;

begin
  Result:=Nil;
end;

Function UnlockResource(ResData: HGLOBAL): LongBool;

begin
  Result:=False;
end;

Function FreeResource(ResData: HGLOBAL): LongBool;

begin
  Result:=False;
end;
