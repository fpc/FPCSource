{ %opt=-O4 }
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
