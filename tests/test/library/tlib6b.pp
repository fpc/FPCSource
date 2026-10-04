{ %target=win32,win64 }
{ %needlibrary }

{ A library whose threadvars cannot be set up while it is loaded fails to load:
  LoadLibrary returns 0 with ERROR_DLL_INIT_FAILED, and the program that loads
  it goes on. The program takes every thread local storage index, so the
  library's run time library gets none, then loads the library of tlib6a.pp. }
program tlib6b;

{$mode objfpc}

const
  TLS_OUT_OF_INDEXES = $ffffffff;
  ERROR_DLL_INIT_FAILED = 1114;

function TlsAlloc: dword; stdcall; external 'kernel32' name 'TlsAlloc';
function TlsFree(index: dword): longbool; stdcall; external 'kernel32' name 'TlsFree';
function LoadLibraryA(name: pansichar): ptruint; stdcall; external 'kernel32' name 'LoadLibraryA';
function FreeLibrary(h: ptruint): longbool; stdcall; external 'kernel32' name 'FreeLibrary';
function GetLastError: dword; stdcall; external 'kernel32' name 'GetLastError';

var
  slots: array[0..4095] of dword;
  n, i: longint;
  index, loaderror: dword;
  h: ptruint;
begin
  n:=0;
  repeat
    index:=TlsAlloc;
    if index=TLS_OUT_OF_INDEXES then
      break;
    if n>high(slots) then
      halt(3);
    slots[n]:=index;
    inc(n);
  until false;
  h:=LoadLibraryA('tlib6a.dll');
  loaderror:=GetLastError;
  for i:=0 to n-1 do
    TlsFree(slots[i]);
  if h<>0 then
    begin
      writeln('the library was loaded without thread local storage');
      FreeLibrary(h);
      halt(1);
    end;
  if loaderror<>ERROR_DLL_INIT_FAILED then
    begin
      writeln('LoadLibrary failed with error ',loaderror,', not ',ERROR_DLL_INIT_FAILED);
      halt(2);
    end;
  writeln('ok');
end.
