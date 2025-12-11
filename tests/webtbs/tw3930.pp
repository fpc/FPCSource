{$ifdef fpc}
{$mode objfpc}
{$endif}
uses
  Classes;

type
  TMyStringList = type TStringlist;

var
  list : TMyStringList;

begin
  TMyStringList.Create.Free;
  if pointer(TMyStringList)<>pointer(TStringList) then
    halt(1);
  writeln('ok');
end.


