{ %OPT=-Sen }

{ Source provided for Free Pascal Bug Report 2425 }
{ Submitted by "Marco van de Voort" on  2003-03-18 }
{ e-mail: marco@freepascal.org }
{$mode Delphi}

type xx = class
            private
            function getx(i:integer):integer;
            Property blaat[i:integer]:integer read getx; default;
            End;


function xx.getx(i:integer):integer;

begin
 result:=i;
end;

var y : xx;

begin
 y:=xx.create;
 writeln(y[3]);
 if y[3]<>3 then
  halt(1);
 y.free;
end.
