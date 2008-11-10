{ %norun }
{ %opt=-Sew -vw }

procedure test(a: longint);
var
  i: longint;
begin
  i:=1;
  if (a < 0) then
    begin
      for i := 1 to 2 do
        writeln(i);
    end
  else
    begin
      { the for-loop in the then-branch must not mark i in this block
        as "uninitialised"
      }
      if i > 0 then
        writeln(i);
    end;
end;

begin
end.
