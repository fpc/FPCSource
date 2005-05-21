{

 **********************************************************************}

{$MODE OBJFPC}

unit DLLFuncs;

interface

uses SysUtils;

function LoadLibrary(Name: PChar): PtrInt;
function GetProcAddress(Lib: PtrInt; ProcName: PChar): Pointer;
function FreeLibrary(Lib: PtrInt): Boolean;
function getlastdlerror: pchar;


implementation

const
  RTLD_LAZY         = $001;
  RTLD_NOW          = $002;
  RTLD_BINDING_MASK = $003;

{$ifdef Linux}
function dlopen(Name: PChar; Flags: LongInt) : Pointer; cdecl; external 'dl';
function dlsym(Lib: Pointer; Name: PChar) : Pointer; cdecl; external 'dl';
function dlclose(Lib: Pointer): LongInt; cdecl; external 'dl';
function dlerror: pchar; cdecl; external 'dl';
{$else}
function dlopen(Name: PChar; Flags: LongInt) : Pointer; cdecl; external 'c';
function dlsym(Lib: Pointer; Name: PChar) : Pointer; cdecl; external 'c';
function dlclose(Lib: Pointer): LongInt; cdecl; external 'c';
function dlerror: pchar; cdecl; external 'c';
{$endif}

function getlastdlerror: pchar;
begin
  getlastdlerror := dlerror;
end;

function LoadLibrary(Name: PChar): PtrInt;
begin
  Result := PtrInt(dlopen(Name, RTLD_LAZY));
end;

function GetProcAddress(Lib: PtrInt; ProcName: PChar): Pointer;
begin
  Result := dlsym(Pointer(Lib), ProcName);
end;

function FreeLibrary(Lib: PtrInt): Boolean;
begin
  if Lib = 0 then
    Result := False
  else
    Result := dlClose(Pointer(Lib)) = 0;
end;

end.
