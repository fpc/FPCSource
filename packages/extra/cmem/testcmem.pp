program testmem;

{$mode objfpc}

uses cmem;

Type
  PLongint = ^Longint;

Var P : PLongint;
    i : longint;

begin
  P:=GetMem(1000*SizeOf(Longint));
  For I:=0 to 999 do
    P[i]:=i;
  P:=ReallocMem(P,500*SizeOf(Longint));
  For I:=0 to 499 do
    if P[i]<>i Then
      Writeln ('Oh-oh, ',i,'th index differs.');
  FreeMem(P);
end.

{
  $Log$
  Revision 1.3  2002-09-08 15:44:40  michael
  + fixed log entry

}
