{ %FAIL }
{ %opt=-O4 -Sew }

{ This code can generate trouble because
  uninitialized retrun value in f method 
  can have a pattern that generates a
  floating point exception later.

  As core decided not to generate an error in such cases,
  this test was modified to al least test that a warning
  is issued about non-initialized return value. }

{$mode objfpc}
uses
  sysutils;
type
  tmyclass = class
    function f : double;virtual;
  end;

function tmyclass.f : double;
  begin
  end;

var
  myclass : tmyclass;
begin
  myclass:=tmyclass.create;
  writeln(myclass.f+myclass.f+myclass.f);
  myclass.free;
  writeln('ok');
end.
