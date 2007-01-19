{ %norun }
uses
  Classes, SysUtils;

var
  List : TList;
begin
  with LongRec(Pointer(List.Last)) do
    lo := 1;
end.
