{
    $Id$

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


{
  $Log$
  Revision 1.4  2004-12-15 21:17:46  peter
    * thandle -> ptrint

  Revision 1.3  2004/11/24 20:04:09  jonas
    + basic Mac OS X support, only bounce works for now though

  Revision 1.2  2003/08/25 18:16:38  marco
   * BSD fix

  Revision 1.1  2002/10/13 13:57:30  sg
  * Finally, the new units are available: Match the C headers more closely;
    support for OpenGL extensions, and much more. Based on the Delphi units
    by Tom Nuydens of delphi3d.net

}
