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
end.  $Log$
end.  Revision 1.2  2002-09-07 15:42:54  peter
end.    * old logs removed and tabs fixed
end.
end.  Revision 1.1  2002/01/29 17:54:59  peter
end.    * splitted to base and extra
end.
}
