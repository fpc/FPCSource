{
    $Id$

 **********************************************************************}

{$MODE OBJFPC}

unit DLLFuncs;

interface

uses SysUtils;

function LoadLibrary(Name: PChar): THandle;
function GetProcAddress(Lib: THandle; ProcName: PChar): Pointer;
function FreeLibrary(Lib: THandle): Boolean;


implementation

const
  RTLD_LAZY         = $001;
  RTLD_NOW          = $002;
  RTLD_BINDING_MASK = $003;

{$ifdef Linux}
function dlopen(Name: PChar; Flags: LongInt) : Pointer; cdecl; external 'dl';
function dlsym(Lib: Pointer; Name: PChar) : Pointer; cdecl; external 'dl';
function dlclose(Lib: Pointer): LongInt; cdecl; external 'dl';
{$else}
function dlopen(Name: PChar; Flags: LongInt) : Pointer; cdecl; external 'c';
function dlsym(Lib: Pointer; Name: PChar) : Pointer; cdecl; external 'c';
function dlclose(Lib: Pointer): LongInt; cdecl; external 'c';
{$endif}

function LoadLibrary(Name: PChar): THandle;
begin
  Result := THandle(dlopen(Name, RTLD_LAZY));
end;

function GetProcAddress(Lib: THandle; ProcName: PChar): Pointer;
begin
  Result := dlsym(Pointer(Lib), ProcName);
end;

function FreeLibrary(Lib: THandle): Boolean;
begin
  if Lib = 0 then
    Result := False
  else
    Result := dlClose(Pointer(Lib)) = 0;
end;

end.


{
  $Log$
  Revision 1.2  2003-08-25 18:16:38  marco
   * BSD fix

  Revision 1.1  2002/10/13 13:57:30  sg
  * Finally, the new units are available: Match the C headers more closely;
    support for OpenGL extensions, and much more. Based on the Delphi units
    by Tom Nuydens of delphi3d.net

}
