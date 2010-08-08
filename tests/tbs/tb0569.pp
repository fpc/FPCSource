{$mode objfpc}

uses
  ub0569;

type
  TMyGen = specialize TGen<longint>;

var
  MyGen : TMyGen;

begin
  MyGen:=TMyGen.Create;
  if MyGen.getstring<>'Free Pascal' then
    halt(1);
  if MyGen.getwidestring<>'Free Pascal'#1234 then
    halt(2);
  if MyGen.getint<>1234123412341234 then
    halt(3);
  if MyGen.getreal<>333.0 then
    halt(4);
  MyGen.Free;
  writeln('ok');
end.

