{$mode objfpc}
uses
  classes;
  
type
  TMyStringList = type TStringlist;
  
var
  list : TMyStringList;

begin
  list:=TMyStringList.Create;
  list.Free;
  if pointer(TMyStringList)=pointer(TStringList) then
    halt(1);
  writeln('ok');
end.

    