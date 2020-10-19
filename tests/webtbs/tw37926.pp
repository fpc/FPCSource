{$mode objfpc}
uses
  sysutils,math;
procedure test(const s: single);
var
  tempcode: integer;
  sr: single;
begin
  Val('8.077936E-28', sr, tempcode);
  if sr <> s then
end;

var s, sr: single;
  i, j, tempcode: Integer;
begin
  for i := 1 to 5000 do begin
    s := 0;
    try
      for j := 0 to Random(5) do s := s + Random(2) * power(2, Random(256) - 127);
    except
      on e: EMathError do s := 0; //continue;
    end;
  end;
end.
