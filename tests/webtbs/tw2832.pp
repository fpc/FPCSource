{ %opt=-Sew }

{ Source provided for Free Pascal Bug Report 2832 }
{ Submitted by "Mattias Gaertner" on  2003-12-05 }
{ e-mail: mattias@freepascal.org }
program ArrayNotInitialized;

{$mode objfpc}{$H+}

procedure DoSomething;
var
  p: array[1..2] of integer;
begin
  p[1]:=0;
end;

begin
end.
