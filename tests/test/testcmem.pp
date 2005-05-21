program testmem;

{$mode objfpc}

uses cmem;

Type
  PLongint = ^Longint;

Var P : PLongint;
    i : longint;
    err : boolean;
begin
  P:=GetMem(1000*SizeOf(Longint));
  For I:=0 to 999 do
    P[i]:=i;
  P:=ReallocMem(P,500*SizeOf(Longint));
  For I:=0 to 499 do
    if P[i]<>i Then
      begin
        Writeln ('Oh-oh, ',i,'th index differs.');
        err:=true;
      end;
  FreeMem(P);
  if err then
    halt(1);
end.
