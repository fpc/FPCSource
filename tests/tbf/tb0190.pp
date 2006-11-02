{ %fail }
{$mode objfpc}
type
  c = class(TObject)
    function a: longint;
  end;

function c.a: longint;
begin
  result := 1;
end;

var b: c.a;
begin
end.