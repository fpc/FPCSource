{ Source provided for Free Pascal Bug Report 2778 }
{ Submitted by "Michael Van Canneyt" on  2003-11-09 }
{ e-mail: michael.vancanneyt@wisa.be }

{$mode delphi}

type
  TMyFunc = Procedure (I : Integer = 10);

Procedure MyRealFunc(I : Integer = 20);

begin
  Writeln('Function received : ',I);
  if i<>10 then
    halt(1);
end;

Var
  M : TMyFunc;

begin
  M:=@MyRealFunc;
  M;
end.
