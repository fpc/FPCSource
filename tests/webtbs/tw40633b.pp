{ %needlibrary }
{ %target=linux }
{ %opt=-O- }
{ %delfiles=tw40633a }

{ Loads a library deep in the stack and raises an exception in it from a shallower frame }
program tw40633b;

{$mode objfpc}

uses
  dynlibs;

type
  TRaiseFrameCount = function: longint; cdecl;

const
  cDepth = 32;

var
  gLib: TLibHandle;

// Loads the library after recursing aLevel times with 64KB of locals per level
procedure LoadDeep(aLevel: longint);

var
  lBuf: array[0..65535] of byte;

begin
  FillChar(lBuf, SizeOf(lBuf), aLevel);
  if aLevel>0 then
    LoadDeep(aLevel-1)
  else
    gLib:=LoadLibrary('./libtw40633a.so');
  if lBuf[0]<>byte(aLevel) then
    halt(10);
end;


var
  lFunc: TRaiseFrameCount;
  lCount: longint;

begin
  LoadDeep(cDepth);
  if gLib=NilHandle then
    begin
      writeln('Library load failed');
      halt(1);
    end;
  lFunc:=TRaiseFrameCount(GetProcAddress(gLib, 'RaiseFrameCount'));
  if not assigned(lFunc) then
    begin
      writeln('RaiseFrameCount not found');
      halt(2);
    end;
  lCount:=lFunc();
  writeln('ExceptFrameCount in library: ', lCount);
  if lCount<2 then
    begin
      writeln('Exception raised in library must have at least 2 backtrace frames');
      halt(3);
    end;
  UnloadLibrary(gLib);
  writeln('ok');
end.
