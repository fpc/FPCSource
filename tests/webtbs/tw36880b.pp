{ %needlibrary }
{ %target=linux }
{ %delfiles=tw36880a }

{ Unloads a library using cthreads while a thread it did not start still uses its threadvars }
program tw36880b;

{$mode objfpc}

uses
  baseunix, dynlibs;

type
  TTouchThreadVar = function: Integer; cdecl;

function pthread_create(aThread: PPtrUInt; aAttr: Pointer; aStart: Pointer; aArg: Pointer): cint; cdecl; external 'pthread';
function pthread_join(aThread: PtrUInt; aResult: PPointer): cint; cdecl; external 'pthread';

const
  libname = './libtw36880a.'+SharedSuffix;

var
  Touch: TTouchThreadVar;
  TouchResult: Integer;
  Touched, Unloaded: LongInt;

// Waits a millisecond
procedure Pause;

var
  lDelay: TTimeSpec;

begin
  lDelay.tv_sec:=0;
  lDelay.tv_nsec:=1000000;
  fpnanosleep(@lDelay,nil);
end;


// Thread function: uses a threadvar of the library and ends after the library is unloaded
function Worker(aArg: Pointer): Pointer; cdecl;

begin
  TouchResult:=Touch();
  InterlockedExchange(Touched,1);
  while Unloaded=0 do
    Pause;
  Result:=nil;
end;


var
  lCycle: Integer;
  lLib: TLibHandle;
  lThread: PtrUInt;

begin
  for lCycle:=1 to 3 do
    begin
    lLib:=LoadLibrary(libname);
    if lLib=NilHandle then
      begin
      Writeln('Could not load ',libname);
      Halt(1);
      end;
    Touch:=TTouchThreadVar(GetProcAddress(lLib,'TouchThreadVar'));
    if not Assigned(Touch) then
      Halt(2);
    Touched:=0;
    Unloaded:=0;
    if pthread_create(@lThread,nil,@Worker,nil)<>0 then
      Halt(3);
    while Touched=0 do
      Pause;
    if TouchResult<>1 then
      begin
      Writeln('Cycle ',lCycle,': threadvar is ',TouchResult,', expected 1');
      Halt(4);
      end;
    if not UnloadLibrary(lLib) then
      Halt(5);
    InterlockedExchange(Unloaded,1);
    pthread_join(lThread,nil);
    end;
  Writeln('ok');
end.
