{ %target=win32,win64,wince}
program TestFreeLib;

uses
{$IFDEF MSWINDOWS}
  sharemem,
  Windows;
{$ELSE}
  DynLibs;
{$ENDIF}

const
{$IFDEF MSWINDOWS}
  DllName = 'libtest.dll';
{$ELSE}
 {$IFDEF DARWIN}
  DllName = 'liblibtest.dylib';
 {$ELSE}
  DllName = 'liblibtest.so';
 {$ENDIF}
{$ENDIF}

var
{$IFDEF MSWINDOWS}
  DllHandle : THandle;
{$ELSE}
  DllHandle : TLibHandle;
{$ENDIF}

var
  InStr : string;
  status : TFPCHeapstatus;
  f : function : pointer;stdcall;
begin
  DllHandle := LoadLibrary(DllName);
  pointer(f):=getprocaddress(Dllhandle,'LibFunction');
  freemem(f());
  FreeLibrary(DllHandle);
end.
