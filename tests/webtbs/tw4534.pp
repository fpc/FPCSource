type
  pbyte = ^byte;

procedure checksigns(a,b: extended);
var
  p1, p2: pbyte;
  i: longint;
begin
  p1 := @a;
  p2 := @b;
  for i := 1 to sizeof(a) do
    begin
      if (p1^ xor p2^) = $80 then
        halt(0);
      inc(p1); 
      inc(p2);
    end;
  halt(1);
end;

var x,y:extended;

Begin
    x:=-0.0;
    y:=0.0;
    checksigns(x,y);
End.

