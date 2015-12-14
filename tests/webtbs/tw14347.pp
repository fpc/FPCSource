{ %OPT=-Sew -Oodfa }
program Project1;

{$mode objfpc}{$H+}

type
  TRec = record a : Integer; end;
  PRec = ^TRec;

var
  p : PRec;

begin
  writeln( sizeof(p^.a)); // warning here!
end.
