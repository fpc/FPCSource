{ Source provided for Free Pascal Bug Report 3328 }
{ Submitted by "Christian Iversen" on  2004-09-21 }
{ e-mail: chrivers@iversen-net.dk }
program fpcdelphi;

var
  err : boolean;

Function A(Const S1, S2: PAnsiChar): Integer; Overload;
Begin
  writeln('pansichar overload');
  err:=false;
End;

Function A(Const S1, S2: AnsiString): Integer; Overload;
Begin
  writeln('ansistring overload');
End;

Var
  X : PAnsiChar;
Begin
  err:=true;
  A(X, '');
  if err then
    halt(1);
End.
