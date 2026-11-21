{ %OPT=-O2 }
program tb0773;

{ The implicit finally of a function calls helpers like fpc_popaddrstack and
  fpc_ansistr_decr_ref with g_call_system_proc. On targets that keep floating
  point values in mm registers, but don't define cpumm (e.g. aarch64), these
  calls did not mark the volatile mm registers as clobbered. With -O2 the
  register allocator then put a float Result into d0 already in the try
  block, so any floating point code executed by the helpers (here: FreeMem of
  the temporary UTF8String) destroyed the function result. }

{$mode objfpc}{$H+}

var
  OldMM, NewMM: TMemoryManager;
  Clobber: double;

function ClobberFreeMem(p: pointer): ptruint;
begin
  { use some floating point registers }
  Clobber:=Clobber*3.0+1.0;
  Result:=OldMM.FreeMem(p);
end;

function ClobberFreeMemSize(p: pointer; Size: ptruint): ptruint;
begin
  Clobber:=Clobber*3.0+1.0;
  Result:=OldMM.FreeMemSize(p, Size);
end;

function Find(const Key: UTF8String): boolean;
begin
  Result:=Key='y';
end;

{ AName is converted to a temporary UTF8String, which is freed in the
  implicit finally }
function GetFloat(const AName: string; ADefault: double): double;
begin
  if Find(AName) then
    Result:=1.5
  else
    Result:=ADefault;
end;

var
  s: string;
  d: double;
begin
  Clobber:=7.0;
  GetMemoryManager(OldMM);
  NewMM:=OldMM;
  NewMM.FreeMem:=@ClobberFreeMem;
  NewMM.FreeMemSize:=@ClobberFreeMemSize;
  SetMemoryManager(NewMM);

  s:='x';
  SetCodePage(RawByteString(s), 1252, false);
  d:=GetFloat(s, -1.0);
  s:='y';
  SetCodePage(RawByteString(s), 1252, false);
  if GetFloat(s, -1.0)<>1.5 then
    begin
      SetMemoryManager(OldMM);
      writeln('error 2');
      halt(2);
    end;

  SetMemoryManager(OldMM);
  if d<>-1.0 then
    begin
      writeln('error 1: ', d);
      halt(1);
    end;
  writeln('ok');
end.
