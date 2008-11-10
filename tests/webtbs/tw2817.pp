{ %opt=-Sen -vn }

{ Source provided for Free Pascal Bug Report 2817 }
{ Submitted by "Mattias Gaertner" on  2003-11-30 }
{ e-mail: mattias@freepascal.org }
program UnusedVar;

{$mode objfpc}{$H+}

procedure DoSomething(var Lines: PPChar; var LineCount: integer);
var
  p: PPChar;
begin
  p:=Lines;
  p[LineCount-1]:=nil;
end;

begin
end.
